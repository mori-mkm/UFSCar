# Analysis of Covariance (ANCOVA)

Example of Analysis of Covariance comparing three treatment groups while adjusting for a continuous covariate.

## Overview

The notebook builds a small dataset with a treatment factor (3 levels), a response variable `y` and a covariate `x`, and visualizes the relationship between the covariate and the response by treatment group (scatter plot) and the response distribution by treatment (boxplot).

## Methods

Rather than fitting the model directly with a formula interface, the notebook constructs the design matrices explicitly and estimates parameters via the normal equations (matrix algebra with `numpy`):

* A **full model** design matrix encoding treatment effects and the centered covariate
* A **reduced model** design matrix using only the centered covariate
* Parameter estimation via `(X'X)⁻¹X'y`
* Comparison of the residual sum of squares (SSE) between the full and reduced models
* F-distribution critical values used to support the test of whether the treatment effect is significant beyond the covariate

## Repository Structure

```text
ancova/
├── ancova-analysis.ipynb
└── README.md
```

## Tools

**Python** — `pandas`, `numpy`, `scipy.stats`, `statsmodels`, `seaborn`
