import pandas as pd
import re
import logging
from typing import List, Dict, Tuple

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('segmentation.log'),
        logging.StreamHandler()
    ]
)

# All possible patterns for identifying image numbers
SEGMENT_PATTERNS = [
    (r'\b(\d{1,2})(?:st|nd|rd|th)\s*(?:one|image|picture)\b', 'ordinal'),  # 1st one
    (r'\b(\d{1,2})\.\s*', 'numbered'),  # 1. 
    (r'\b(\d{1,2})\s*:\s*', 'colon'),  # 1:
    (r'\b(\d{1,2})\s*-', 'dash'),  # 1-
    (r'\bimage\s*(\d{1,2})\b', 'image_num'),  # image 1
    (r'\bpicture\s*(\d{1,2})\b', 'picture_num'),  # picture 1
    (r'\bnumber\s*(\d{1,2})\b', 'number_num'),  # number 1
    (r'\bnext\s*(?:one|image|picture)\b', 'next_one'),  # next one
    (r'\bfirst\s*(?:one|image|picture)\b', 'first_one'),
    (r'\bsecond\s*(?:one|image|picture)\b', 'second_one'),
    (r'\bthird\s*(?:one|image|picture)\b', 'third_one'),
    (r'\bfourth\s*(?:one|image|picture)\b', 'fourth_one'),
    (r'\bfifth\s*(?:one|image|picture)\b', 'fifth_one'),
    (r'\bsixth\s*(?:one|image|picture)\b', 'sixth_one'),
    (r'\bseventh\s*(?:one|image|picture)\b', 'seventh_one'),
    (r'\beighth\s*(?:one|image|picture)\b', 'eighth_one'),
    (r'\bninth\s*(?:one|image|picture)\b', 'ninth_one'),
    (r'\btenth\s*(?:one|image|picture)\b', 'tenth_one'),
    (r'\beleventh\s*(?:one|image|picture)\b', 'eleventh_one'),
    (r'\btwelfth\s*(?:one|image|picture)\b', 'twelfth_one'),
    (r'\b(\d{1,2})\b(?!\d)', 'standalone')  # 1 (standalone)
]

# Compile all regex patterns
COMPILED_PATTERNS = [(re.compile(p[0], re.IGNORECASE), p[1]) for p in SEGMENT_PATTERNS]

def extract_image_number(match_text: str, pattern_type: str) -> Tuple[int, str]:
    """Extract the image number from matched text."""
    try:
        if pattern_type in ('numbered', 'colon', 'dash', 'standalone', 'ordinal',
                          'image_num', 'picture_num', 'number_num'):
            num = int(re.search(r'\d+', match_text).group())
            return num, match_text
        elif pattern_type == 'next_one':
            return -1, match_text  # Special flag to increment
        elif pattern_type.endswith('_one'):
            word = pattern_type.split('_')[0]
            number_map = {
                'first': 1, 'second': 2, 'third': 3, 'fourth': 4, 'fifth': 5,
                'sixth': 6, 'seventh': 7, 'eighth': 8, 'ninth': 9, 'tenth': 10,
                'eleventh': 11, 'twelfth': 12
            }
            return number_map.get(word, 0), match_text
        return 0, match_text
    except Exception as e:
        logging.warning(f"Error extracting number from '{match_text}': {str(e)}")
        return 0, match_text

def segment_description(text: str, expected_segments: int = 12) -> List[str]:
    """
    Split description into segments based on image number references.
    Preserves all original text and maintains exactly 12 segments.
    """
    segments = {i: [] for i in range(1, expected_segments + 1)}
    current_num = 1  # Start with image 1 by default
    last_pos = 0
    
    # Find all matches in the text
    matches = []
    for pattern, pattern_type in COMPILED_PATTERNS:
        for match in pattern.finditer(text):
            num, full_text = extract_image_number(match.group(), pattern_type)
            matches.append((match.start(), match.end(), num, full_text))
    
    # Process matches in order
    for start, end, num, full_text in sorted(matches, key=lambda x: x[0]):
        # Add text before this match to current segment
        if start > last_pos:
            segments[current_num].append(text[last_pos:start])
        
        # Handle the matched number
        if num == -1:  # "next one" - increment current number
            num = current_num + 1
        
        if 1 <= num <= expected_segments:
            # Add the matched text to the appropriate segment
            segments[num].append(full_text)
            current_num = num
            last_pos = end
    
    # Add any remaining text after last match
    if last_pos < len(text):
        segments[current_num].append(text[last_pos:])
    
    # Combine all parts for each segment and clean whitespace
    return [' '.join(' '.join(segments[i]).split()) for i in range(1, expected_segments + 1)]

def load_tangram_boards(board_file: str) -> Dict[Tuple[str, int], Tuple[List[str], List[str]]]:
    """
    Load target and selected tangrams from tangramsFinalBoards.csv.
    Returns a dictionary mapping (gameid, roundNum) to lists of targets and selections.
    """
    try:
        df = pd.read_csv(board_file)
        df = df.rename(columns={'roundNum': 'repetitionNum'})
        tangram_ids = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']
        result = {}
        for _, row in df.groupby(['gameid', 'repetitionNum']).first().reset_index().iterrows():
            gameid = row['gameid']
            round_num = row['repetitionNum']
            targets = [str(row[f'true{tid}']) if f'true{tid}' in row else '' for tid in tangram_ids]
            selections = [str(row[f'sub{tid}']) if f'sub{tid}' in row else '' for tid in tangram_ids]
            result[(gameid, round_num)] = (targets, selections)
        logging.info(f"Loaded {len(result)} game-round pairs from {board_file}")
        return result
    except Exception as e:
        logging.error(f"Failed to load tangram boards from {board_file}: {str(e)}")
        raise

def process_game_messages(df: pd.DataFrame, id_columns: List[str], tangram_boards: Dict) -> pd.DataFrame:
    """
    Process all messages for each game, concatenating and segmenting them.
    Includes target and selected tangrams in the output.
    Returns DataFrame with segmented descriptions and tangram data.
    """
    results = []
    
    # Group messages by game (and round if available)
    grouped = df.groupby(id_columns)['contents'].apply(
        lambda x: ' '.join(x.dropna().astype(str))
    ).reset_index()
    
    for _, row in grouped.iterrows():
        game_id = row[id_columns[0]]
        round_num = row[id_columns[1]] if len(id_columns) > 1 else 1
        
        try:
            segments = segment_description(row['contents'])
            
            # Get target and selected tangrams
            targets, selections = tangram_boards.get((game_id, round_num), ([""] * 12, [""] * 12))
            
            for i, (description, target, selection) in enumerate(zip(segments, targets, selections), 1):
                result = {
                    'gameid': game_id,
                    'roundNum': round_num,
                    'image_number': i,
                    'description': description,
                    'target_tangram': target,
                    'selected_tangram': selection
                }
                results.append(result)
                
        except Exception as e:
            logging.error(f"Error processing game {game_id} round {round_num}: {str(e)}")
            continue
    
    return pd.DataFrame(results)

def load_and_process(input_file: str, board_file: str, output_file: str) -> pd.DataFrame:
    """Load input CSV and process into segmented descriptions with tangram data."""
    try:
        # Load data with flexible column handling
        df = pd.read_csv(input_file)
        logging.info(f"Successfully loaded {len(df)} rows from {input_file}")
        
        # Load tangram boards
        tangram_boards = load_tangram_boards(board_file)
        
        # Verify we have required columns
        if 'contents' not in df.columns:
            raise ValueError("Input file must contain 'contents' column with message text")
        
        # Determine ID columns (gameid and optionally roundNum)
        id_cols = []
        if 'gameid' in df.columns:
            id_cols.append('gameid')
            if 'roundNum' in df.columns:
                id_cols.append('roundNum')
            else:
                df['roundNum'] = 1  # Add default round number
                id_cols.append('roundNum')
        else:
            # If no gameid, create one based on row numbers
            df['gameid'] = df.index // 20  # Approximate grouping
            df['roundNum'] = 1
            id_cols = ['gameid', 'roundNum']
            logging.warning("No 'gameid' column found - creating artificial grouping")
        
        # Process all games
        segmented_df = process_game_messages(df, id_cols, tangram_boards)
        
        # Save results
        segmented_df.to_csv(output_file, index=False)
        logging.info(f"Saved {len(segmented_df)} segments to {output_file}")
        
        return segmented_df
        
    except Exception as e:
        logging.error(f"Failed to process file: {str(e)}")
        raise

if __name__ == "__main__":
    INPUT_FILE = 'import/hawkins_dynamics_free/raw_data/rawUnconstrainedMessages.csv'
    BOARD_FILE = 'import/hawkins_dynamics_free/raw_data/tangramsFinalBoards.csv'
    OUTPUT_FILE = 'import/hawkins_dynamics_free/raw_data/segmented_descriptions.csv'
    
    try:
        print("Starting tangrams description segmentation...")
        result_df = load_and_process(INPUT_FILE, BOARD_FILE, OUTPUT_FILE)
        print("\nSegmentation completed successfully!")
        print(f"Results saved to {OUTPUT_FILE}")
        print("\nSample output:")
        print(result_df.head(24))  # Show first 2 games (24 segments)
    except Exception as e:
        print(f"\nError: {str(e)}")
        print("Check segmentation.log for details")