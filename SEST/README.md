# Natural Language Processing Fundamentals

Introduction to Natural Language Processing through practical exercises in text preprocessing, representation and sentiment classification.

The project uses a dataset of vaccine-related tweets to explore how unstructured text can be transformed into structured features for statistical and machine learning models.

## Overview

The notebook covers a typical NLP workflow, from raw text preparation to predictive modeling.

Topics include:

* Text cleaning and normalization
* String similarity and fuzzy matching
* Tokenization
* Stopword removal
* Bag-of-Words representation
* TF-IDF
* Text classification
* Word embeddings

## Text Representation

Different approaches are explored to transform textual information into numerical features.

### Bag of Words

`CountVectorizer` is used to represent documents according to word occurrence frequencies.

### TF-IDF

`TfidfVectorizer` extends the representation by weighting terms according to their relevance across the document collection.

### Word2Vec

Word embeddings are introduced using `Word2Vec`, providing a distributed representation where words can be mapped into a continuous vector space.

## Sentiment Classification

The project includes a supervised text classification exercise using **Multinomial Naive Bayes**.

The workflow includes:

* Train/test split
* Text vectorization
* Model training
* Sentiment prediction
* Accuracy evaluation
* Classification metrics

## Dataset

The analysis uses vaccine-related social media posts containing information such as:

* Tweet text
* Vaccine reference
* Text length
* Sentiment classification
* Positive, negative, neutral and mixed sentiment confidence scores

## Repository Structure

```text
SEST/
├── NLP_Exercicios.ipynb
├── vacina_tweets_sentiment.xlsx
└── README.md
```

## Tech Stack

**Python**

Main libraries:

* `pandas`
* `scikit-learn`
* `NLTK`
* `gensim`
* `fuzzywuzzy`
* `dataprep`

## Context

The notebook was developed from an introductory **Natural Language Processing workshop by Visagio**, covering fundamental techniques for processing and modeling textual data.
