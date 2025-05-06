# Nuova versione: Crea e pulisce automaticamente i testi
import requests  # For sending HTTP requests
import json 
import time
import re
#from data_api import chat_with_mistral
import pandas as pd
from tqdm import tqdm
import dotenv
import os

dotenv.load_dotenv()

# ==================== Configuration ====================
API_KEY = os.getenv('API-KEY')

# Base URL for Mistral's OpenAI-compatible chat completions endpoint
API_URL = "https://api.mistral.ai/v1/chat/completions"

# Choose one of the available models (e.g., 'mistral-tiny', 'mistral-small', 'mistral-medium')
MODEL = "mistral-small"

# Headers for authentication and content type
HEADERS = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json"
}


# ==================== Function to Send a Chat Message ====================
def chat_with_mistral(messages, temperature):
    """
    Sends a list of messages to the Mistral chat API and returns the assistant's response.

    Parameters:
        messages (list): A list of message dictionaries in the OpenAI chat format.
                         Example: [{"role": "user", "content": "Hello!"}]

    Returns:
        str: The assistant's reply as a string.
    """
    payload = {
        "model": MODEL,
        "messages": messages,
        "temperature": temperature,     # Creativity level (0 = deterministic, 1 = more random)
        "top_p": 1.0,           # Nucleus sampling parameter
        "stream": False         # Disable streaming for simple usage
    }

    # Send a POST request to Mistral's API
    response = requests.post(API_URL, headers=HEADERS, data=json.dumps(payload))

    # Raise an error if the request failed
    if response.status_code != 200:
        raise Exception(f"Request failed: {response.status_code} - {response.text}")

    # Parse the JSON response
    response_data = response.json()

    # Extract and return the assistant's reply
    return response_data['choices'][0]['message']['content']


prompts = {"positive" : "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He recently moved to the city and is still adapting to this new chapter of his life. He spends most of his time at work or with his friends, but he's also starting to explore the city and think about new goals for the future",
           "negative" : "As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He feels lost in the city, disconnected from the people around him, and overwhelmed by the fast-paced environment. He often doubts his abilities and wonders if accepting the job was a mistake"
           }
datas = []

# Dataset feature
count_text = list(range(0,15))
temperature_level = [0.1,0.7,1.3]

# iterations count
total_iterations = len(prompts) * len(count_text) * len(temperature_level)

with tqdm(total=total_iterations, desc="Generating letters") as pbar:
    for prompt_type, prompt_text in prompts.items():
        for i in temperature_level:
            for _ in count_text:
                chat_history = [
                    {"role": "user", "content": prompt_text}
                ]

                try:
                    # Send the message and receive the model's response
                    response = chat_with_mistral(chat_history, i)
                    response_clean = re.sub(r"\[.*?\]|\(.*?\)", "", response)
                    response_clean = response_clean.replace("\n", " ").replace("\t", " ").replace("\r", " ")
                    datas.append({"type of prompt": prompt_type,
                        "temperature": i,
                                  "text": response_clean
                                  })
                    print("Assistant:", response_clean)
                except Exception as e:
                    print("Error:", str(e))
                time.sleep(3)
                pbar.update(1)


dataspd = pd.DataFrame(datas)
dataspd.to_csv("generated_data/data_chat.csv")