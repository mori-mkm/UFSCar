# Time Series Analysis

Collection of statistical exercises developed as part of the **Time Series** coursework at UFSCar.

The project explores the fundamental workflow of time series modeling, from descriptive analysis and pattern identification to model fitting, diagnostics and statistical interpretation.

## Overview

The analyses focus on understanding the temporal structure of observed data and selecting models capable of representing their underlying dynamics.

The general workflow includes:

* Time series visualization
* Descriptive analysis
* Identification of temporal patterns
* Model specification
* Parameter estimation
* Model comparison
* Residual diagnostics
* Interpretation of the fitted model

## Time Series Modeling

The main notebook (`time-series-modeling.ipynb`) analyzes `data/series-01.csv`: it checks stationarity with the Augmented Dickey-Fuller test, inspects the ACF/PACF plots to identify the autoregressive order, and uses `pmdarima.auto_arima` to select the best-fitting model, which converges to an **AR(1)** specification.

`data/series-02.csv` is an additional series included for further modeling exercises; it is not analyzed in the current notebook.

## Model Diagnostics

The fitted AR(1) model is validated through residual diagnostics:

* **Ljung-Box test** — checks for remaining autocorrelation in the residuals
* **Jarque-Bera test** — checks whether the residuals are approximately normally distributed
* Residual plots (density and QQ-plot) confirming the residuals behave as white noise

## Visualizations

The `figures/` folder contains the plots produced during the analysis:

* `series-overview.png` — the observed series
* `fitted-model.png` — the fitted model output
* `residual-diagnostics.png` — residual diagnostic plots

## Repository Structure

```text
time-series/
├── data/
│   ├── series-01.csv
│   └── series-02.csv
│
├── figures/
│   ├── series-overview.png
│   ├── fitted-model.png
│   └── residual-diagnostics.png
│
└── notebooks/
    └── time-series-modeling.ipynb
```

## Tools

**Python** — `statsmodels`, `pmdarima`, `plotly`, `matplotlib`

## Academic Context

These materials were developed during the Statistics undergraduate program at the **Federal University of São Carlos (UFSCar)** as practical applications of time series modeling and statistical diagnostics.
