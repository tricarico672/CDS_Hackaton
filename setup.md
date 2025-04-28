# Getting the data from the LLM

## Running locally

There are two main ways to run this program to interact with LLMs and set up your own study. The first way is to rely on a model that is run locally. To do this, first install Ollama and then in the terminal run the following command `ollama run mistral`.  

## Running using the Mistral API

To run the model using the mistral API make sure that you configure a `.env` file setting the `API-KEY` variable to your Mistral API key.

# Cleaning and preprocessing the data

After having obtained the data from the LLM and having it saved, it is possible to run from the terminal two python scripts:
1. The first will clean the data and ask if you want to save it as a .csv file, otherwise it will not overwrite the previous file you saved
2. The second, will perform a frame analysis and assign an emotional score on eight different dimensions and then save the Pandas DataFrame to a .csv file.

To run the commands it suffices to input in the terminal (once in the root directory of the project) and in a UNIX-like environment:
1. `python src/clean_data.py`
2. `python src/create_emotions_df.py` 
