
import os
import argparse
import pandas as pd
from sentence_transformers import SentenceTransformer

parser = argparse.ArgumentParser()
parser.add_argument("--model", type=str, default="all-MiniLM-L6-v2")
parser.add_argument("--use_cache", action="store_true")
args = parser.parse_args()

if __name__ == "__main__":
    model = SentenceTransformer(args.model)

    ### read data
    DATA_LOC = "harmonized_data"
    dirs = os.listdir(DATA_LOC)

    all_msg = pd.DataFrame()
    for dir in sorted(dirs):
        if os.path.isdir(os.path.join(DATA_LOC, dir)):
            print(f"Reading {dir}...")

            if args.use_cache:
                if os.path.exists(os.path.join(DATA_LOC, dir, "embeddings.csv")):
                    print("Embeddings already exist, skipping...")
                    continue

            df_msg = pd.read_csv(os.path.join(DATA_LOC, dir, "messages.csv"), on_bad_lines='skip')
            df_trials = pd.read_csv(os.path.join(DATA_LOC, dir, "trials.csv"))
            df_con = pd.read_csv(os.path.join(DATA_LOC, dir, "conditions.csv"))
            df_out = df_trials.merge(df_con, on="condition_id", how="left") \
                            .merge(df_msg, on="trial_id", how="left")
            all_msg = pd.concat([all_msg, df_out])

    if all_msg.empty:
        print("No data found, exiting...")
        exit()

    all_msg["text"] = all_msg["text"].fillna("").astype(str)

    all_msg_concat = all_msg[all_msg["role"] == "describer"] \
                            .groupby(["paper_id", "game_id", "trial_id"])["text"] \
                            .apply(", ".join) \
                            .reset_index()

    ### embed
    print("Creating embeddings...")
    sentences = all_msg_concat['text'].tolist()
    embeddings = model.encode(sentences)

    all_msg_concat["embeddings"] = pd.Series(list(embeddings))

    ### save
    print("Saving embeddings...")
    for dir in sorted(dirs):
        all_msg_concat[all_msg_concat["paper_id"] == dir].to_csv(os.path.join(DATA_LOC, dir, "embeddings.csv"))


