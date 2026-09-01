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
PAE/
├── ANCOVA/
│   └── Exemplo_Aula13.ipynb
│
├── ANOVA/
│   ├── Exemplo ANOVA com dois fatores.ipynb
│   └── exemplo_crianças_deficiência_crescimento.ipynb
│
├── Nested_desing/
│   └── Exemplo_Fornecedores_e_matérias_primas.ipynb
│
└── Tukey’s Test/
    └── exemplo_competicao_vinho.ipynb
```

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
