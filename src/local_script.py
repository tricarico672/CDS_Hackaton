import requests
import pandas as pd
import re
import pandas as pd
from tqdm import tqdm

def chat_with_mistral(prompt, temperature):
    response = requests.post(
        "http://localhost:11434/api/generate",
        json={
            "model": "mistral",
            "prompt": prompt,
            "stream": False,
            'temperature' : temperature,
            'max_tokens' : 100
        }
    )
    return response.json()["response"]

prompts = {"positive" : "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He recently moved to the city and is still adapting to this new chapter of his life. He spends most of his time at work or with his friends, but he's also starting to explore the city and think about new goals for the future",
           "negative" : "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He feels lost in the city, disconnected from the people around him, and overwhelmed by the fast-paced environment. He often doubts his abilities and wonders if accepting the job was a mistake"
           }
datas = []

# Dataset feature
count_text = list(range(0,34)) # change this to adjust the number of texts to be generated for each category.
temperature_level = [0.1,0.7,1.3]

# iterations count
total_iterations = len(prompts) * len(count_text) * len(temperature_level)

with tqdm(total=total_iterations, desc="Generating letters") as pbar:
    for prompt_type, prompt_text in prompts.items():
        for i in temperature_level:
            for _ in count_text:
                # chat_history = [
                #     {"role": "user", "content": prompt_text}
                # ]

                try:
                    # Send the message and receive the model's response
                    response = chat_with_mistral(prompt_text, i)
                    response_clean = re.sub(r"\[.*?\]|\(.*?\)", "", response)
                    response_clean = response_clean.replace("\n", " ").replace("\t", " ").replace("\r", " ")
                    # build dataset sequentially
                    datas.append({"type of prompt": prompt_type,
                        "temperature": i,
                                  "text": response_clean
                                  })
                    print("Assistant:", response_clean)
                except Exception as e:
                    print("Error:", str(e))
                # time.sleep(3)
                pbar.update(1)

dataspd = pd.DataFrame(datas)
dataspd.to_csv("generated_data/data_chat_local.csv")

