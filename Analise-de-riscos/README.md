# Credit Risk Analysis

Statistical analysis and classification project developed as part of the **Risk Analysis** coursework at UFSCar.

The project explores credit profile data and applies **Logistic Regression** to estimate the probability of a borrower being classified into different payment-risk categories.

## Overview

The analysis combines exploratory data analysis, statistical visualization and predictive modeling to investigate patterns associated with credit risk.

The workflow includes:

* Data preparation and variable encoding
* Exploratory analysis of quantitative and categorical variables
* Distribution and relationship analysis
* Train/test split for model evaluation
* Logistic Regression
* Variable selection using stepwise regression
* Model comparison using predictive accuracy and deviance

## Modeling Approach

The dataset is divided into **80% training data and 20% test data**.

Several Logistic Regression specifications are evaluated, including:

* A full model using all available predictors
* A reduced model using selected credit-related variables
* A stepwise-selected model

Predictions are generated using a probability threshold of `0.5`, and model performance is compared using classification accuracy and model deviance.

## Exploratory Analysis

The project includes several approaches for understanding the relationships between borrower characteristics and credit risk:

* Descriptive statistics
* Pairwise variable analysis
* Boxplots
* Scatter plots
* Logistic probability curves
* Three-dimensional visualization

Variables explored include credit amount, credit duration, age, account status, savings profile, employment information and other borrower characteristics.

## Repository Structure

```text
Analise-de-riscos/
├── T1.R
├── dados.txt
└── README.md
```

`T1.R` contains the complete analysis and modeling workflow, while `dados.txt` contains the dataset used in the study.

## Tech Stack

**R**

Main libraries:

* `data.table`
* `ggplot2`
* `GGally`
* `scatterplot3d`
* `FactoClass`
* `caret`
* `tidyverse`

## Academic Context

This project was developed during the Statistics undergraduate program at the **Federal University of São Carlos (UFSCar)** as an applied exercise in credit risk analysis and statistical classification.
