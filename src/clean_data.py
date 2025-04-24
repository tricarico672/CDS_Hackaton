from utils.drop_data import drop_data
from utils.clean_df import clean_df
from utils.merge_df import merge_df
from utils.prepare_csv import prepare_csv

import pandas as pd
from string import ascii_lowercase

df = pd.read_csv('generated_data/chat_100.csv')

cleaned_df = clean_df(drop_data(df))

prepare_csv(cleaned_df)

additional_obs = pd.read_csv('generated_data/data_chat_local_complete.csv') 

cleaned_additional = clean_df(additional_obs)

final_df = merge_df(df, additional_obs)

print(prepare_csv(final_df).head())

