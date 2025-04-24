import pandas as pd

def drop_data(df:pd.DataFrame, temp:int = 1.3) -> pd.DataFrame:

    return df[df['temperature'] != temp]