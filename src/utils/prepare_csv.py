import pandas as pd

def prepare_csv(df:pd.DataFrame, col_drop: list = ['Unnamed: 0', 'text_lower']) -> pd.DataFrame:
    """
    prepare data for export in csv by dropping the columns specified in the argument
    """
    
    return df.drop(columns=col_drop, axis = 1)