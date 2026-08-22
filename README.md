# Behavioral components define operational suitability for detection dogs

Reproducible R code accompanying:

> Zheng J, Lazarowski L, Lanier AL, Haney PS, Chester EM, Waggoner P, and Wang X (2026). **Behavioral components define operational suitability metric for detection dog success.** *Frontiers in Veterinary Science* 13:1887925. [https://doi.org/10.3389/fvets.2026.1887925](https://doi.org/10.3389/fvets.2026.1887925)

## Overview

This repository estimates **Computationally Synthesized Suitability (CSS)** scores from standardized behavioral component scores collected at 3, 6, 10, and 12 months. It also provides code to:

- compare CSS with trainer-reported suitability;
- describe CSS in relation to final training disposition;
- construct Spearman correlation networks; and
- evaluate longitudinal changes in CSS.

## Repository structure

```text
.
├── R/
│   ├── config.R                 # variables, formulas, cutoffs, and plotting settings
│   ├── data_validation.R        # input and outcome validation
│   ├── css_models.R             # age-specific linear models and CSS calculation
│   ├── evaluation.R             # correlations and classification metrics
│   ├── networks.R               # Spearman/FDR correlation networks
│   └── longitudinal.R           # long-format conversion and per-dog slopes
├── analysis/
│   ├── 01_estimate_css.R
│   ├── 02_evaluate_css.R
│   ├── 03_correlation_networks.R
│   └── 04_longitudinal_css.R
├── data/
│   └── README.md
├── outputs/                     # generated files; ignored by Git
├── run_all.R
├── DESCRIPTION
└── CITATION.cff
```

Shared functions live in `R/`; the numbered scripts are intentionally short. This gives each scientific output a clear entry point without duplicating model code.

## Data

Individual-level research data are not included. Place an authorized copy of the analysis workbook at:

```text
data/private/time_variant_and_invariant_data.xlsx
```

The entire `data/private/` directory is excluded by `.gitignore`. See [`data/README.md`](data/README.md) for the expected variables and privacy guidance.

## Installation

Use R 4.3 or later. From the repository root:

```r
install.packages(c(
  "readxl", "writexl", "dplyr", "tidyr", "ggplot2",
  "psych", "igraph", "pROC", "caret", "testthat"
))
```

For a fully locked environment, initialize `renv` after the repository has been reviewed:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

## Run the analysis

Run the complete workflow:

```r
source("run_all.R")
```

Or run individual stages:

```r
source("analysis/01_estimate_css.R")
source("analysis/02_evaluate_css.R")
source("analysis/03_correlation_networks.R")
source("analysis/04_longitudinal_css.R")
```

Generated tables, figures, and model summaries are written to `outputs/`.

## CSS models

The reduced formulas below reproduce the predictor sets in the original public code:

| Age | Behavioral components used to estimate trainer-reported suitability |
|---:|---|
| 3 months | Physical Possessiveness of Toy, Independence, Hunt, Work/Effort, Excitability |
| 6 months | Physical Possessiveness of Toy, Independence, Surfaces, Work/Effort, Excitability |
| 10 months | Focus on Toy/Reward, Independence, People, Vehicles/Urban Clutter, Excitability |
| 12 months | Hunt, Surfaces, People, Work/Effort, Excitability |

CSS is the fitted value from the corresponding linear model. The historical descriptive cutoffs are 2.6, 2.8, 3.0, and 3.2 at 3, 6, 10, and 12 months, respectively.

## Reproducibility notes

- Input selection is based on column names, not spreadsheet positions.
- Sample size is inferred from the data rather than fixed at 180.
- Scripts are noninteractive and do not call `setwd()` or `View()`.
- Outcome levels are fixed as `Washout` (negative) and `Sale Quality` (positive).
- Correlation networks use Spearman correlations with false-discovery-rate adjustment.
- The correlation threshold is defined once in `R/config.R`, preventing a mismatch between analysis and figure labels.
- In-sample CSS values quantify reconstruction of trainer suitability. They should not be interpreted as out-of-sample validation of training outcomes.

## Citation

If you use this code, cite the article above. Repository metadata are also provided in `CITATION.cff`.

## License and data use

No software license has been assigned in this draft. The copyright holder and institution should approve a license before public release. The article is available under CC BY, but that does not automatically determine the software or research-data license.

The code does not grant permission to redistribute individual-level canine records. Confirm sponsor, institutional, ethics, and data-sharing requirements before releasing any data or fitted model object.
