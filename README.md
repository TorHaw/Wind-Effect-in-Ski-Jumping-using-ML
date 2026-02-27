# Wind Compensation in International Ski Jumping

This repository provides the computational framework and datasets used to validate wind compensation constants in elite ski jumping. We employ a combination of parametric (Linear) and non-parametric (Random Forest, GAM) models to evaluate the relationship between longitudinal wind speed and jump distance.

## Repository Structure
* `data/`: PDF scraped FIS World Cup datasets for Male and Female divisions.
* `src/`: Unified analysis script (`wind_effect_modeling.R`) containing preprocessing, modeling, and publication-grade visualization logic. `wind_effect_modling.R` is a cleaned combination of our original code from the male and female ski jumping analysis which we include as well. This additionally contains the scraper used to convert information from FIS PDFs into usable CSV formatted data.
* `results/`: Output directory for high-resolution (600 DPI) figures.

## Execution Instructions
The analysis is dataset-agnostic. To reproduce the findings:
1. Ensure R is installed with the libraries found at the top of the code.
2. Ensure the raw CSV files are in the `data/` directory.
3. Run the script: `Rscript src/wind_effect_modeling.R`.

## Analytical Methodology
* **Variable Selection**: Focused on in-run speed ($V_0$), longitudinal wind ($U_w$), and Hill Size ($HS$).
* **Model Validation**: 80/20 train-test split utilizing RMSE and $R^2$ as primary metrics.
* **Graphic Generation**: All figures are generated using `ggplot2` with `cairo-png` rendering for high-fidelity publication outputs.
