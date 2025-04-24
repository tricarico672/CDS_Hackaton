import pandas as pd

# clean data
def clean_df(df: pd.DataFrame) -> pd.DataFrame:

    # make text lowercase
    df['text_lower'] = df['text'].str.lower()

    # drop if text is shorter than 500 characters
    min_length = df['text_lower'].str.len() >= 500
    # check if text starts with parenthesis
    parenthesis = df['text_lower'].str.startswith('(')
    
    # check if non ascii letters are used
    non_ascii_mask = df['text'].apply(lambda x: not all(ord(char) < 128 for char in str(x)))

    df_cleaned = df[(~parenthesis) & (~non_ascii_mask) & (min_length)]

    return df_cleaned
