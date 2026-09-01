# Undergraduate Thesis

Statistics undergraduate thesis work combining meteorological data from INMET (Brazil's National Institute of Meteorology) with financial market series, exploring wavelet-based methods for analyzing time-varying signal structure.

## Workflow

The analysis is organized into four notebooks, meant to be read in order:

### 01 — INMET Data Consolidation

[`01-inmet-data-consolidation.ipynb`](notebooks/01-inmet-data-consolidation.ipynb) reads the raw INMET weather station CSV files (organized by year, one file per station), extracts station metadata from each file's header, groups stations by state, and consolidates them into one averaged daily series per state, saved under `data/inmet-by-state/`.

### 02 — Weather Station Analysis

[`02-weather-station-analysis.ipynb`](notebooks/02-weather-station-analysis.ipynb) loads the automatic and conventional weather station catalogs (`data/station-catalogs/`) and profiles them: counts by operating status and station type, and a pivot table cross-tabulating status against station type.

### 03 — Wavelet Method Exploration

[`03-wavelet-method-exploration.ipynb`](notebooks/03-wavelet-method-exploration.ipynb) is a small methodological experiment applying the Continuous Wavelet Transform (CWT) with `PyWavelets` to a synthetic demo signal, computing and visualizing the resulting scalogram, including a 3D surface visualization of the wavelet coefficients.

### 04 — Financial Market Wavelet Analysis

[`04-financial-market-wavelet-analysis.ipynb`](notebooks/04-financial-market-wavelet-analysis.ipynb) downloads financial index series (Ibovespa, Dow Jones, S&P 500, Nasdaq, USD/BRL, SSE50), handles missing data, and applies the Continuous Wavelet Transform to the Dow Jones and Ibovespa series to inspect their time-scale structure (scalograms and 3D coefficient surfaces). It also includes an exploratory attempt at wavelet coherence analysis using the `piwavelet` package, tested on synthetic random signals rather than the financial series themselves.

### Archived Notebook

[`archive/legacy-complete-workflow.ipynb`](notebooks/archive/legacy-complete-workflow.ipynb) is an earlier, monolithic version of the workflow that combines the station analysis, data consolidation and wavelet exploration steps in a single notebook. It is kept for reference only — the numbered notebooks above are the current, organized version of the analysis.

## Data

```text
data/
├── inmet-by-state/       # consolidated daily weather series, one CSV per state
└── station-catalogs/     # automatic and conventional station metadata
```

### Raw INMET data limitation

The original raw INMET station files (one CSV per station per year, referenced by notebook 01) are **not versioned in this repository** — only the consolidated, state-level output of that consolidation step is available under `data/inmet-by-state/`. As a result, notebook 01 cannot be re-run end-to-end from this repository alone; it is included to document the consolidation process that produced the datasets used downstream.

## Repository Structure

```text
undergraduate-thesis/
├── data/
│   ├── inmet-by-state/
│   └── station-catalogs/
│
└── notebooks/
    ├── 01-inmet-data-consolidation.ipynb
    ├── 02-weather-station-analysis.ipynb
    ├── 03-wavelet-method-exploration.ipynb
    ├── 04-financial-market-wavelet-analysis.ipynb
    └── archive/
        └── legacy-complete-workflow.ipynb
```

## Tools

**Python** — `pandas`, `numpy`, `PyWavelets`, `yfinance`, `pandas_datareader`, `missingno`, `matplotlib`, `plotly`, `seaborn`

## Academic Context

This is the most substantial project in the repository: the undergraduate thesis (TCC) developed at UFSCar, combining meteorological data engineering with an exploratory application of wavelet analysis to financial time series.

## Reproducibility

Full reproducibility is limited by the missing raw INMET source files (see above) and by hardcoded local file paths in some cells of notebooks 01 and the archived legacy notebook. The consolidated datasets in `data/` allow notebooks 02–04 to be followed without needing the original raw files.
