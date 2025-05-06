import pandas as pd
import json
import numpy as np

output_jsonl = "generated_data/cleaned/final_complete_input.jsonl"

df = pd.read_csv('generated_data/cleaned/final_complete.csv')

df['input'] = np.where(df['type of prompt'] == 'positive', "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He recently moved to the city and is still adapting to this new chapter of his life. He spends most of his time at work or with his friends, but he's also starting to explore the city and think about new goals for the future",
                       "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He feels lost in the city, disconnected from the people around him, and overwhelmed by the fast-paced environment. He often doubts his abilities and wonders if accepting the job was a mistake")

with open(output_jsonl, 'w', encoding='utf-8') as f:
    idx = 1
    for _, row in df.iterrows():
        json_line = {
            "id": idx,
            "input": row["input"]
        }

        idx += 1
        f.write(json.dumps(json_line) + '\n')