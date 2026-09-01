# Natural Language Processing Fundamentals

Introductory exercises on text preprocessing, representation and sentiment classification, using a dataset of vaccine-related tweets.

This notebook was not developed as part of UFSCar coursework — it originates from an introductory NLP workshop by Visagio, structured as a set of guided exercises with hints and blanks to fill in.

## Overview

The exercises walk through a typical NLP pipeline, from raw text handling to a sentiment classification model:

* Text matching with regular expressions (e.g. finding mentions or hashtags)
* Fuzzy text matching and string similarity (Levenshtein distance)
* Text cleaning and stopword removal
* Train/test split
* Text representation with Bag-of-Words (`CountVectorizer`) and TF-IDF (`TfidfVectorizer`)
* Sentiment classification with Multinomial Naive Bayes, evaluated with accuracy and a classification report

As distributed, the notebook keeps several steps (vectorization, model fitting, evaluation) as fill-in-the-blank exercises rather than a fully executed, results-bearing analysis.

## Dataset

`vaccine-tweets-sentiment.xlsx` contains vaccine-related tweets with fields such as:

* Tweet text
* Vaccine reference
* Text length
* Sentiment label and per-class confidence scores (positive, negative, neutral, mixed)

## Repository Structure

```text
natural-language-processing/
├── nlp-fundamentals.ipynb
├── vaccine-tweets-sentiment.xlsx
└── README.md
```

## Tools

**Python** — `pandas`, `scikit-learn`, `nltk`, `gensim`, `fuzzywuzzy`, `dataprep`

## Context

This material comes from an introductory Natural Language Processing workshop by Visagio, used here as a learning exercise alongside the Statistics coursework material in this repository.
