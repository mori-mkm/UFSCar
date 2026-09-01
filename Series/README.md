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

The main activity consists of analyzing an observed series and developing an appropriate statistical model.

The analysis considers both the characteristics of the original series and the behavior of the residuals after model fitting.

Model selection is supported by statistical diagnostics rather than relying only on visual fit.

## Model Diagnostics

An important part of the analysis is evaluating whether the fitted model adequately captures the structure of the series.

This includes examining:

* Residual behavior
* Remaining temporal dependence
* Model fit
* Differences between observed and fitted values

These diagnostics help determine whether the selected specification provides an adequate representation of the data.

## Repository Structure

```text
Series/
├── modelos/
│   ├── Atividade1.ipynb
│   ├── serie.csv
│   └── imagens/
│
├── modelo2/
│   └── serie2.csv
│
└── README.md
```

### `modelos`

Contains the main time series analysis, including the notebook, source data and diagnostic visualizations.

### `modelo2`

Contains an additional time series dataset intended for further modeling exercises.

## Tech Stack

**Python**

The analysis is implemented primarily through Jupyter Notebooks using Python's statistical and data analysis ecosystem.

## Academic Context

These materials were developed during the Statistics undergraduate program at the **Federal University of São Carlos (UFSCar)** as practical applications of time series modeling and statistical diagnostics.
