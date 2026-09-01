# Design and Analysis of Experiments

Collection of statistical applications developed as part of the **Design and Analysis of Experiments** coursework at UFSCar.

The notebooks explore classical methods for comparing treatments, modeling experimental effects and understanding how different experimental structures influence statistical inference.

## Topics

### Analysis of Variance

Applications of **ANOVA** for evaluating whether differences between treatment or group means are statistically significant.

The examples include one- and two-factor experimental settings and demonstrate how variation can be decomposed into different sources.

### Analysis of Covariance

Application of **ANCOVA**, combining analysis of variance with regression to evaluate treatment effects while accounting for a continuous covariate.

The notebook explores treatment groups, covariate relationships and adjusted comparisons using statistical models.

### Nested Designs

Example of a **nested experimental design**, where levels of one factor are contained within levels of another.

This structure is useful when experimental units naturally follow a hierarchical organization, such as suppliers and materials.

### Tukey's Test

Application of **Tukey's Honest Significant Difference (HSD)** procedure for multiple comparisons after an ANOVA.

The example demonstrates how pairwise differences between treatment means can be evaluated while controlling the overall error rate.

## Repository Structure

```text
experimental-design/
├── ancova/
│   └── ancova-analysis.ipynb
│
├── anova/
│   ├── growth-study-anova.ipynb
│   └── two-way-anova.ipynb
│
├── nested-design/
│   └── suppliers-materials-design.ipynb
│
├── tukey-hsd/
│   └── wine-competition-analysis.ipynb
│
└── data/
    ├── growth-factorial-experiment.csv
    ├── machine-operator-productivity.csv
    └── muscle-fatigue-experiment.csv
```

The `data/` folder holds datasets from related experimental design exercises (factorial growth experiment, machine/operator productivity, and a muscle fatigue study) that are not tied to a single notebook above.

## Methods

The material covers concepts including:

* Analysis of Variance
* Analysis of Covariance
* Factorial experiments
* Nested experimental designs
* Multiple comparisons
* Treatment effects
* F-tests
* Linear statistical models

## Tech Stack

**Python**

Main libraries used across the notebooks include:

* `pandas`
* `numpy`
* `statsmodels`
* `matplotlib`
* `seaborn`

## Academic Context

These materials were developed during the Statistics undergraduate program at the **Federal University of São Carlos (UFSCar)** as practical applications of experimental design and statistical inference.
