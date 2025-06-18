from config import FULL_TANGRAMS_SVGS, EMBEDDINGS_DIR
from embedding import setup_pretrained_model, preprocess_for_clip
from pathlib import Path
import pandas as pd
import torch
import os
from tqdm import tqdm

def load_existing_embeddings(output_path):
    if output_path.exists():
        df = pd.read_csv(output_path)
        processed_files = set(df["image_name"])
        print(f"Loaded {len(processed_files)} cached embeddings.")
        return df, processed_files
    return pd.DataFrame(), set()

def extract_image_embeddings_batched(image_paths, model, preprocess, device, batch_size=32):
    embeddings = []
    image_names = []
    batch = []

    for path in tqdm(image_paths, desc="Embedding images", unit="img"):
        try:
            image_tensor = preprocess(path).to(device)
            batch.append(image_tensor)
            image_names.append(Path(path).name)

            if len(batch) == batch_size:
                batch_tensor = torch.cat(batch, dim=0)
                with torch.no_grad():
                    batch_embeds = model(batch_tensor).cpu().numpy()
                embeddings.extend(batch_embeds)
                batch = []
        except Exception as e:
            print(f"Error processing {path}: {e}")

    # Final batch
    if batch:
        batch_tensor = torch.cat(batch, dim=0)
        with torch.no_grad():
            batch_embeds = model(batch_tensor).cpu().numpy()
        embeddings.extend(batch_embeds)

    return image_names, embeddings

def save_embeddings_incremental(new_names, new_embeds, output_path, existing_df=None):
    df_new = pd.DataFrame(new_embeds, columns=[f"dim_{i+1}" for i in range(len(new_embeds[0]))])
    df_new.insert(0, "image_name", new_names)
    combined = pd.concat([existing_df, df_new], ignore_index=True) if existing_df is not None else df_new
    output_path.parent.mkdir(parents=True, exist_ok=True)
    combined.to_csv(output_path, index=False)
    print(f"Saved {len(combined)} total embeddings to {output_path}")

if __name__ == "__main__":
    IMAGE_DIR = FULL_TANGRAMS_SVGS
    OUTPUT_CSV = EMBEDDINGS_DIR / "clip_finetuned_embeddings.csv"

    all_image_paths = sorted([
        str(IMAGE_DIR / f)
        for f in os.listdir(IMAGE_DIR)
        if f.lower().endswith((".svg", ".png", ".jpg", ".jpeg"))
    ])

    existing_df, cached_names = load_existing_embeddings(OUTPUT_CSV)
    remaining_paths = [p for p in all_image_paths if Path(p).name not in cached_names]

    print(f"Found {len(all_image_paths)} images total.")
    print(f"Processing {len(remaining_paths)} new images...")

    if not remaining_paths:
        print("No new images to process. Exiting.")
        exit()

    model_files = [
        "clip_controlled/whole+black/model0.pth",
        "clip_controlled/whole+black/model1.pth",
        "clip_controlled/whole+black/model2.pth"
    ]
    model, preprocess, device = setup_pretrained_model(
        repo_id="lil-lab/kilogram-models",
        file_paths=model_files,
        device=None
    )

    new_names, new_embeds = extract_image_embeddings_batched(
        remaining_paths, model, preprocess, device, batch_size=32
    )

    save_embeddings_incremental(new_names, new_embeds, OUTPUT_CSV, existing_df)


