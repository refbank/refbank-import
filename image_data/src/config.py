from pathlib import Path

# project paths

GEN_ROOT = Path(__file__).parent.parent.parent

DATA_DIR = GEN_ROOT / "image_data"

# data paths

FULL_TANGRAMS_SVGS = DATA_DIR / "images" 
SAMPLE_TANGRAMS_SVG = DATA_DIR / "small_image_set"

MAPPING_FILE = DATA_DIR / "tangram_map.csv"
EMBEDDINGS_DIR = DATA_DIR / "embeddings"

# output paths

#OUTPUT_DIR = DATA_DIR / "outputs"
#SIMILARITY_RESULTS = OUTPUT_DIR / "similarity_results"
#CHECKPOINTS = OUTPUT_DIR / "checkpoints"