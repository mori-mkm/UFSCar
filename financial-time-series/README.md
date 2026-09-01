# Financial Time Series

Collection of analyses applying time series methods to financial return series: correlation between assets, volatility modeling, state-space models and Box-Jenkins/GARCH estimation. The project mixes three languages — Python, R and SAS — each used for a different piece of the analysis.

## Notebooks and Scripts

### Equity Correlation Analysis (Python)

[`notebooks/equity-correlation-analysis.ipynb`](notebooks/equity-correlation-analysis.ipynb) downloads stock price series with `yfinance`/`pandas_datareader`, examines the correlation between two equities (rolling Pearson correlation), and normalizes a small portfolio to compare its cumulative performance against the Ibovespa index.

### GARCH Volatility Modeling (R)

[`r/garch-volatility-modeling.R`](r/garch-volatility-modeling.R) computes log-returns for a stock series, tests stationarity with the Augmented Dickey-Fuller test, fits an ARIMA model via `auto.arima` (Box-Jenkins methodology), checks residual autocorrelation with the Ljung-Box test, and then fits a GARCH(1,1) model to the residuals with the `rugarch` package to model time-varying volatility.

### State-Space Models (R)

[`r/state-space-models.R`](r/state-space-models.R) applies state-space modeling with the Kalman filter/smoother (`dlm` package) to a return series, progressing through a local-level model, a local linear trend model, and a local level model with a seasonal component. Model fit is compared with AIC/BIC, residuals are checked for autocorrelation (Ljung-Box) and normality (Shapiro-Wilk), and the local-level model is used to produce a short-horizon forecast with confidence intervals.

### Log-Returns Analysis (SAS)

[`sas/tim-log-returns.sas`](sas/tim-log-returns.sas) computes daily log-returns for a stock price series and fits and compares several models from the GARCH family (GARCH, IGARCH, EGARCH, GARCH-M, TGARCH, PGARCH, among others) using `PROC AUTOREG`, selecting among them with information criteria (AIC/SBC).

## Repository Structure

```text
financial-time-series/
├── notebooks/
│   └── equity-correlation-analysis.ipynb
│
├── r/
│   ├── garch-volatility-modeling.R
│   └── state-space-models.R
│
└── sas/
    └── tim-log-returns.sas
```

## Methods

* Log-returns and stationarity testing
* Box-Jenkins (ARIMA) modeling
* GARCH-family volatility models
* State-space models with Kalman filtering and smoothing
* Residual diagnostics: Ljung-Box, Shapiro-Wilk

## Tools

* **Python** — `yfinance`, `pandas_datareader`, `seaborn`
* **R** — `rugarch`, `fGarch`, `forecast`, `tseries`, `dlm`
* **SAS** — `PROC AUTOREG`

## Academic Context

These materials were developed during Statistics coursework at UFSCar covering financial time series and volatility modeling.
