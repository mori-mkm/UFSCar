# Wavelet Multivariate Time Series Analysis

<p align="center">
  <strong>Undergraduate Thesis in Statistics — Federal University of São Carlos (UFSCar)</strong>
</p>

<p align="center">
  Financial time series analysis using Continuous Wavelet Transform,
  multiresolution decomposition, multiscale correlation and wavelet coherence.
</p>

<p align="center">
  <img
    src="figures/wavelet-decomposition.png"
    alt="Wavelet transform represented as a 3D surface and scalogram"
    width="950"
  />
</p>

<p align="center">
  <em>
    Continuous Wavelet Transform represented as a 3D coefficient surface
    and scalogram.
  </em>
</p>

---

## Overview

This repository contains the analysis developed for my undergraduate thesis in
Statistics at the **Federal University of São Carlos (UFSCar)**, defended in 2023.

The study investigates relationships among four financial time series:

- **IBOVESPA**
- **Dow Jones Industrial Average**
- **S&P 500**
- **Bitcoin**

The main idea is to explore how correlations between financial markets change
across **time and frequency/scale**, rather than relying only on a single global
correlation measure.

Wavelet methods are especially useful in this context because financial series
are typically non-stationary and their behavior can change considerably across
different market regimes.

---

## Key findings

The analysis highlighted three main patterns:

### 1. Market shocks appear clearly in the wavelet domain

The COVID-19 period produced important disturbances across the financial series.
The time-scale representation made these changes easier to identify than through
the original series alone.

### 2. Dow Jones and S&P 500 showed the strongest relationship

The two US equity indices presented strong positive multiscale correlations
across the decomposition levels.

Their wavelet coherence was also consistently high, indicating similar behavior
across multiple time scales.

### 3. Bitcoin behaved differently from traditional equity indices

Bitcoin and the Dow Jones showed relatively weak relationships at higher-frequency
components.

Their strongest relationship appeared at lower-frequency components associated
with the long-term trend.

IBOVESPA and Dow Jones presented an intermediate behavior, with their relationship
changing according to both time and scale.

---

## Methodology

The workflow combines classical time-series diagnostics with wavelet-based
analysis.

```text
Yahoo Finance
     │
     ▼
Financial time series
     │
     ├── IBOVESPA
     ├── Dow Jones
     ├── S&P 500
     └── Bitcoin
     │
     ▼
Missing-data treatment
     │
     ▼
Descriptive analysis
     │
     ▼
Stationarity diagnostics
     │
     ▼
Wavelet analysis
     │
     ├── Continuous Wavelet Transform
     ├── Scalograms
     ├── Multiresolution Analysis
     ├── Multiscale Correlation
     └── Wavelet Coherence
```

### Techniques

| Stage | Technique |
| --- | --- |
| Data quality | Missing-value inspection and interpolation |
| Time-series diagnostics | Rolling mean and variance |
| Stationarity | Augmented Dickey-Fuller test |
| Time-frequency analysis | Continuous Wavelet Transform (CWT) |
| Signal decomposition | MODWT / Multiresolution Analysis |
| Dependence analysis | Multiscale Wavelet Correlation |
| Time-scale dependence | Wavelet Coherence |

---

## Data

Financial data were obtained from **Yahoo Finance**.

| Series | Yahoo Finance ticker |
| --- | --- |
| IBOVESPA | `^BVSP` |
| Dow Jones Industrial Average | `^DJI` |
| S&P 500 | `^GSPC` |
| Bitcoin | `BTC-USD` |

The analysis covers financial observations between **2012 and 2023**.

The data are downloaded programmatically, so the repository does not need to
store a static copy of the original market dataset.

---

## Technologies

### Python

Used for:

- market-data collection;
- data manipulation;
- missing-data analysis;
- stationarity diagnostics;
- Continuous Wavelet Transform;
- scalograms;
- 3D wavelet visualizations.

Main libraries:

`pandas` · `NumPy` · `yfinance` · `statsmodels` · `PyWavelets` ·
`Matplotlib` · `missingno`

### MATLAB

Used for:

- Maximum Overlap Discrete Wavelet Transform (MODWT);
- multiresolution decomposition;
- multiscale wavelet correlation;
- wavelet coherence.

---

## Repository structure

```text
undergraduate-thesis/
│
├── README.md
├── requirements.txt
│
├── figures/
│   └── wavelet-decomposition.png
│
├── notebooks/
│   └── financial-wavelet-analysis.ipynb
│
├── matlab/
│   └── wavelet-analysis.m
│
└── thesis/
    └── TCC_MKM_versao_final.pdf
```

---

## Code

### Python analysis

[`notebooks/financial-wavelet-analysis.ipynb`](notebooks/financial-wavelet-analysis.ipynb)

Contains the reconstructed Python workflow used for:

1. collecting the financial series;
2. inspecting missing values;
3. evaluating stationarity;
4. visualizing the series;
5. computing the Continuous Wavelet Transform;
6. generating scalograms and 3D wavelet representations.

### MATLAB analysis

[`matlab/wavelet-analysis.m`](matlab/wavelet-analysis.m)

Contains the MATLAB routines used for:

- multiresolution decomposition;
- multiscale correlation;
- wavelet coherence.

---

## Thesis

The complete undergraduate thesis is available here:

**[Read the full thesis](thesis/TCC_MKM_versao_final.pdf)**

**Title:** *Análise de séries temporais multivariadas via Wavelet*  
**English title:** *Wavelet Multivariate Time Series Analysis*  
**Program:** Bachelor of Statistics — UFSCar  
**Advisor:** Prof. Dr. Maria Sílvia de Assis Moura  
**Defense:** August 24, 2023

---

## Source-code reconstruction

The original development directory contained exploratory material produced
during earlier stages of the research.

Some of those experiments involved meteorological data from INMET, but they
were **not part of the final defended thesis**.

For this repository, the analysis was reconstructed using the source code
included in **Appendix A of the final thesis** as the reference.

This keeps the repository focused on the work that was actually presented and
defended.

A few inconsistencies between the thesis text and the appendix code were
preserved and documented inside the notebook rather than silently changed.

---

## Reproducibility

Install the Python dependencies with:

```bash
pip install -r requirements.txt
```

Then open:

```text
notebooks/financial-wavelet-analysis.ipynb
```

The exact package versions used in the original 2023 environment were not
preserved.

Because Yahoo Finance and some Python APIs have changed since the original
analysis, minor compatibility adjustments may be necessary to execute the
notebook in a modern environment.

The repository therefore prioritizes **faithful documentation of the defended
analysis** over silently rewriting the original methodology.

---

## Future work

The thesis suggests extending the analysis by modeling individual wavelet
decomposition levels separately.

One possible direction would be to model:

- short-term variability at higher-frequency decomposition levels;
- longer-term behavior at lower-frequency / trend levels;
- relationships between significantly correlated wavelet components across
  financial series.

This could provide a foundation for wavelet-based forecasting models.

---

## Author

**Matheus Kengi Mori**

Bachelor of Statistics  
Federal University of São Carlos — UFSCar
