# Data Mining

Collection of applied data mining exercises covering regression, classification and model validation, developed with Python in Jupyter/Colab notebooks.

Each notebook is an independent exercise rather than part of a single modeling project; together they cover a typical data mining workflow from model fitting to validation and diagnostics.

## Notebooks

### Boston Housing Regression

[`boston-housing-regression.ipynb`](notebooks/boston-housing-regression.ipynb) fits linear and polynomial regression models (`rm`, `lstat` as predictors) to the Boston housing dataset, comparing model fit with R² and MSE and visualizing fitted coefficients.

### Logistic Regression Classification

[`logistic-regression-classification.ipynb`](notebooks/logistic-regression-classification.ipynb) trains a logistic regression classifier on a synthetic dataset, building a confusion matrix, computing accuracy/precision/recall/F1 manually, plotting the ROC curve and searching for the classification threshold that maximizes F1-score.

### Polynomial Model Selection

[`polynomial-model-selection.ipynb`](notebooks/polynomial-model-selection.ipynb) fits polynomials of increasing degree (1 to 20) to synthetic data and uses a train/validation/test split to illustrate overfitting and degree selection based on validation error.

### Polynomial Regression Challenge

[`polynomial-regression-challenge.ipynb`](notebooks/polynomial-regression-challenge.ipynb) combines exploratory data analysis, k-fold cross-validation for polynomial degree selection, an OLS model fit with `statsmodels`, and residual diagnostics (Shapiro-Wilk normality test, Breusch-Pagan heteroscedasticity test).

## SVM Interactive Demo

[`svm-interactive-demo/`](svm-interactive-demo/) is a separate interactive Dash application for visualizing SVM decision boundaries — see its own [README](svm-interactive-demo/README.md).

## Repository Structure

```text
data-mining/
├── notebooks/
│   ├── boston-housing-regression.ipynb
│   ├── logistic-regression-classification.ipynb
│   ├── polynomial-model-selection.ipynb
│   └── polynomial-regression-challenge.ipynb
│
└── svm-interactive-demo/
    ├── app.py
    ├── preview.png
    └── README.md
```

## Tools

**Python**

Main libraries used across the notebooks:

* `pandas`, `numpy`
* `scikit-learn`
* `statsmodels`
* `scipy`
* `matplotlib`, `seaborn`

## Academic Context

These notebooks were developed as applied exercises during Statistics/Data Science coursework at UFSCar involving regression, classification and model validation.
