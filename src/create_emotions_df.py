from utils.compute_scores import compute_scores
import pandas as pd

df = pd.read_csv('generated_data/cleaned/final_complete.csv')

dct = compute_scores(df)

df_emotions = pd.DataFrame(dct)

pd.concat([df, df_emotions], axis = 1).to_csv('generated_data/cleaned/emotions.csv')