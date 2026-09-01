# Nested Experimental Design

Example of a nested (hierarchical) experimental design, where one factor's levels are contained within the levels of another.

## Overview

The notebook analyzes a material purity measurement collected from batches ("lines") nested within suppliers — each supplier has its own set of lines, so the line factor only makes sense within its supplier and is not crossed with it.

## Methods

* Parameter estimation for the nested model (overall mean, supplier effects, line-within-supplier effects) computed directly from group means
* ANOVA for a nested design, fit with `statsmodels` using the `fornecedor/linha` (supplier/line) nesting notation
* F-tests for the supplier effect and the line-within-supplier effect

## Repository Structure

```text
nested-design/
├── suppliers-materials-design.ipynb
└── README.md
```

## Tools

**Python** — `pandas`, `numpy`, `statsmodels`
