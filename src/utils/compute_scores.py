from emoatlas import EmoScores
import pandas as pd

def compute_scores(df: pd.DataFrame, n = -1):
    emos = EmoScores()
    dct = {'anger' : list(),
'trust' : list(),
'surprise' : list(), 
'disgust' : list(),
'joy' : list(),
'sadness' : list(),
'fear' : list(),
'anticipation' : list()}

    if n == -1:
        for i in range(len(df)):
            text = df['text'].iloc[i]
            scores = emos.zscores(text)
            for k in scores:
                dct[k].append(scores[k])
    else:
        for i in range(len(df)):
            text = df['text'].iloc[i]
            scores = emos.zscores(text)
            for k in scores:
                dct[k].append(scores[k])

    return dct