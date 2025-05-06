import csv
import json

input_csv = "generated_data/cleaned/final_complete.csv"
output_jsonl = "generated_data/cleaned/final_complete.jsonl"

with open(input_csv, newline='', encoding='utf-8') as csvfile, open(output_jsonl, 'w', encoding='utf-8') as jsonlfile:
    reader = csv.DictReader(csvfile)
    idx = 1
    for row in reader:
        json_obj = {
            "id": idx,
            "text": row["text"]
        }
        idx += 1
        jsonlfile.write(json.dumps(json_obj) + '\n')