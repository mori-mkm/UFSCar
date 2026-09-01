# Analysis of Variance (ANOVA)

Two examples of Analysis of Variance for comparing group means and evaluating factor effects.

## Notebooks

### Growth Study ANOVA

[`growth-study-anova.ipynb`](growth-study-anova.ipynb) analyzes a growth-difference outcome across gender and deficiency-severity groups. It builds the ANOVA design matrix explicitly and fits it with `statsmodels.formula.api.ols`, comparing several reduced models (dropping non-significant terms) before fitting a two-factor model with an interaction term (`genero * desen`).

### Two-Way ANOVA

[`two-way-anova.ipynb`](two-way-anova.ipynb) is a two-factor ANOVA example (plant height as a function of watering frequency and sunlight exposure), fit with `statsmodels` and interpreted through the significance of each main effect and the interaction term.

## Methods

* One- and two-factor Analysis of Variance
* Design matrices and linear models via `statsmodels`
* F-tests for main effects and interaction effects
* Interaction plots

## Repository Structure

```text
anova/
├── growth-study-anova.ipynb
├── two-way-anova.ipynb
└── README.md
```

## Tools

**Python** — `pandas`, `numpy`, `statsmodels`, `matplotlib`
