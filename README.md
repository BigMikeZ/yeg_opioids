# Did Supervised Consumption Sites Change Opioid Death Rates in Alberta?
**Author:** Mike Zhang — [GitHub](https://github.com/BigMikeZ)

[View Full Report Here](https://bigmikez.github.io/yeg_opioid/)

## Research Question
Did the opening of supervised consumption sites in Edmonton and Calgary change opioid and all-substance drug poisoning death rates in those cities, relative to comparison zones elsewhere in Alberta?

This question sits inside an active and heavily politicized policy debate as the Alberta government has closed several SCS facilities in recent years, including one of the sites examined in this analysis, and has signaled further closures as part of a shift toward a recovery-oriented treatment model.

## Key Finding

Calgary showed no statistically significant effect of SCS implementation on opioid or all-substance death rates, in either direction.

Edmonton showed a statistically significant increase in the death rate trend following SCS implementation — a result that held up across multiple robustness checks (comparison-zone exclusion, an all-substance specificity check). However, a specificity check comparing opioid-specific deaths to all-substance deaths found the effect was nearly identical across both outcomes. Since SCS operate primarily through opioid-specific overdose reversal (naloxone), this pattern suggests Edmonton's increase is more likely to reflect a broader, zone-wide confound during this period, such as a shift in drug supply toxicity, housing conditions, or other concurrent local factors, rather than a causal effect of the SCS itself.

In short, this analysis found no evidence that SCS reduced opioid deaths in either city, but also found no reliable evidence that SCS caused Edmonton's observed increase — the pattern is real, but its cause remains unresolved. See the full report for the complete reasoning behind this conclusion, including the assumption checks and sensitivity analyses that shape it.

## Data

Monthly opioid and all-substance poisoning death rates (per 100,000 person-years) for all five Alberta Health Services (AHS) zones, from January 2016 to March 2026, sourced from the [Alberta Substance Use Surveillance System (ASUSS)](https://www.alberta.ca/substance-use-surveillance-data#jumplinks-0) dashboard.

## Methods

A controlled interrupted time series design was used, with Edmonton and Calgary modeled separately (given their different SCS opening dates) against a shared pool of comparison zones. Models were fit using Generalized Least Squares with an AR(1) correlation structure to account for autocorrelation in the monthly time series, with COVID-19 modeled as a separate co-intervention (its own level and slope change) to avoid confounding it with the SCS effect. Model assumptions, including parallel pre-trends, linearity, and residual diagnostics, were formally tested, and several sensitivity and specificity analyses were conducted to assess the robustness of the primary findings, including a discovery that the comparison zones used in this analysis were not fully untreated (each had its own SCS on a different timeline), which is addressed directly in the report as a design limitation.

## Limitations

The comparison zones used in this analysis (North, Central, South) each opened their own SCS at different points during the study period, meaning this is more accurately a staggered-rollout comparison than a true treated-versus-untreated design. Other limitations — including undocumented population denominator methodology and residual skew in the primary models, likely driven by small monthly death counts in lower-population zones — are discussed in full in the report.

## Tech Stack
- **Language:** R
- **GLS modeling**: `nlme`
- **Data manipulation and visualization**: `tidyverse`
- **Date handling:** `lubridate`
- **Result tables:** `gtsummary`
- **Report generation:** Quarto

## Repository Structure
```
├── data/
│   ├── raw/            # Extracted ASUSS CSV exports
│   └── processed/      # Cleaned, combined analysis dataset
├── scripts/            # Data extraction, cleaning, modeling, and visualization code
├── models/             # Saved models
├── yeg_opioid_report   # Quarto Markdown file with scripts to render final report
├── docs/               # Final report rendered as HTML
└── README.md           # README file 
```

