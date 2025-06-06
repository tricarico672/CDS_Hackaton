# General Information

This is the repository for the Cognitive Data Science Hackaton. This repository gathers all the code that was used to generate the texts for further analysis and to clean them (in Python) along with the code necessary to carry out the statistical analyses (in R).

# Prompts

## Positive

As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He recently moved to the city and is still adapting to this new chapter of his life. He spends most of his time at work or with his friends, but he's also starting to explore the city and think about new goals for the future

## Negative

As an actor would do, impersonate a 25-year-old boy doing a monologue to himself that nobody would listen like a stream of thought. He works as a data scientist in a large company in London. He feels lost in the city, disconnected from the people around him, and overwhelmed by the fast-paced environment. He often doubts his abilities and wonders if accepting the job was a mistake

# Temperature

The experiment was run using three different temperature levels: 0.1, 0.7, and 1.3 to allow for more variation and randomness in the LLM's responses.

# FactGenie Prompt to annotate LLM-produced text
Given the data: in the `.jsonl` file, annotate spans in the field text of the file.

Instructions for annotating the text:

Output the errors as a JSON list "annotations" in which each object contains fields "reason", "text", and "annotation_type". The value of "reason" is the reason for the annotation. The value of "text" is the literal value of the text inside the highlighted span, so that the span can later be identified using string matching. The value of "annotation_type" is an integer index of the error based on the following list:

- 0: semantic (does the text make sense semantically?)
- 1: logical (does the text make sense logically?)
- 2: grammatical (is the text written in a grammatically correct way?)

The list should be sorted by the position of the error in the text. Make sure that the annotations are not overlapping.

# Setup

## Running locally

There are two main ways to run this program to interact with LLMs and set up your own study. The first way is to rely on a model that is run locally. To do this, first install Ollama and then in the terminal run the following command `ollama run mistral`.  

## Running using the Mistral API

To run the model using the mistral API make sure that you configure a `.env` file setting the `API-KEY` variable to your Mistral API key.

## Cleaning and preprocessing the data

After having obtained the data from the LLM and having it saved, it is possible to run from the terminal two python scripts:
1. The first will clean the data and ask if you want to save it as a .csv file, otherwise it will not overwrite the previous file you saved
2. The second, will perform a frame analysis and assign an emotional score on eight different dimensions and then save the Pandas DataFrame to a .csv file.

To run the commands it suffices to input in the terminal (once in the root directory of the project) and in a UNIX-like environment:
1. `python src/clean_data.py`
2. `python src/create_emotions_df.py` 
