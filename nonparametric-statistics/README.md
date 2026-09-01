# Nonparametric Statistics

Collection of exercises in nonparametric statistics: simulation studies comparing binomial and normal distributions, nonparametric group comparison tests, and spline regression. Each item is an independent exercise rather than a single unified analysis.

## Notebooks and Scripts

### Binomial-Normal Simulation

[`notebooks/binomial-normal-simulation.ipynb`](notebooks/binomial-normal-simulation.ipynb) explores the binomial distribution and its normal approximation: binomial exact tests (`stats.binom_test`) under different alternative hypotheses, sampling from a binomial distribution, a normality test (D'Agostino) on the sample, and a visual comparison of normal and binomial density curves.

### Group Comparison Tests

[`scripts/group-comparison-tests.R`](scripts/group-comparison-tests.R) simulates three groups from normal distributions with different means, then compares them descriptively (boxplots, histograms, summary statistics with confidence intervals) and tests the normality of each group with the Shapiro-Wilk test and QQ-plots. It also runs a median (chi-squared) test and the **Kruskal-Wallis test** as nonparametric alternatives to a one-way ANOVA.

### Spline Regression

[`scripts/spline-regression.R`](scripts/spline-regression.R) fits regression splines (`splines::bs`) to the `mcycle` dataset (motorcycle acceleration data) with different numbers of knots (3 and 10), compares them to a degree-7 polynomial fit, and reconstructs the cubic spline basis manually to inspect its coefficients directly.

## Repository Structure

```text
nonparametric-statistics/
├── notebooks/
│   └── binomial-normal-simulation.ipynb
│
└── scripts/
    ├── group-comparison-tests.R
    └── spline-regression.R
```

## Methods

* Binomial exact tests and normal approximation
* Statistical simulation
* Shapiro-Wilk normality test
* Kruskal-Wallis test
* Regression splines

## Tools

* **Python** — `scipy.stats`, `seaborn`
* **R** — `splines`, `MASS`, `rstatix`, `FSA`, `PMCMRplus`, `ggpubr`

## Academic Context

These exercises were developed during Statistics coursework at UFSCar as applications of nonparametric inference and simulation.
