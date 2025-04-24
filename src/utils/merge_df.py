import pandas as pd

def merge_df(df1: pd.DataFrame, df2: pd.DataFrame):
    """
    concatenates two dataframes by row 
    """
    return pd.concat([df1, df2], axis=0)