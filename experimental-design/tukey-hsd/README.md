# Tukey's Honest Significant Difference (HSD)

Example applying a one-way ANOVA followed by Tukey's HSD test for pairwise comparisons, using wine competition scores across four groups.

## Overview

The notebook first runs a one-way ANOVA (`scipy.stats.f_oneway`) across four score groups to test whether their means differ. Since the ANOVA result is significant, it proceeds with Tukey's HSD test (`statsmodels.stats.multicomp.pairwise_tukeyhsd`) to identify exactly which pairs of group means differ, while controlling the family-wise error rate.

## Methods

* One-way ANOVA (F-test)
* Tukey's HSD post-hoc multiple comparison procedure
* Interpretation of adjusted p-values and confidence intervals for pairwise mean differences

## Repository Structure

```text
tukey-hsd/
├── wine-competition-analysis.ipynb
└── README.md
```

## Tools

**Python** — `pandas`, `numpy`, `scipy.stats`, `statsmodels`
