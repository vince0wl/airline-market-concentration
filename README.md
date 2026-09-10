# Empirical Analysis of Concentration and Polarization in the Italian Aviation Sector (2010 – 2019)

This repository contains the R source code and the statistical methodology developed to analyze the competitive evolution and network structure of air transport in Italy during the decade preceding the pandemic crisis.

The study contrasts two opposing forces: the **attraction effect of market entry** (which fosters market de-concentration through the expansion of low-cost carriers) and **strategic investment in hub capacity** (which protects the spatial dominance of major airports through congestion and frequency saturation).

## Implemented Econometric Methodology

The models are grounded in the Industrial Economics literature (Sutton 1991, Demsetz 1973, Oliveira 2016). The following statistical solutions have been implemented in the code:

1. **Logit Transformation of Limited Dependent Variables (LDV):** To model market share (`share`) and the concentration index (`HHI`) — which are bounded by definition within the (0, 1) interval — a logit transformation was applied:
$$\text{logit}(HHI) = \ln\left(\frac{HHI}{1 - HHI}\right)$$
This allows the data to be mapped onto the real line, avoiding biased estimates or theoretically impossible predictions.

2. **Two-Way Fixed Effects (Within) Panel Models:** Used to capture within-panel variability while controlling for time-invariant specific characteristics of individual airports (e.g., geographical location) and common temporal shocks.

3. **Newey-West correction (HAC – Heteroskedasticity and Autocorrelation Consistent):** Applied with both lag = 2 (for the annual ENAC dataset) and lag = 4 (for the monthly Eurostat time series) to ensure the scientific validity of the inference (robust p-values) in the presence of heteroskedasticity and residual autocorrelation.

4. **Gini coefficient and Lorenz curve (ineq):** Used as indicators of spatial inequality to examine the "Italian Paradox": the coexistence of a decline in average route concentration (HHI) and an increasing polarization of actual traffic around a very small number of dominant hubs (major hubs and LCC bases).

## Used R Packages
- `tidyverse` (dplyr, ggplot2, lubridate) for data cleaning and manipulation
- `plm` for estimating econometric models on panel data
- `ineq` for calculating the Gini coefficient and Lorenz curve
- `sandwich` & `lmtest` for coefficient testing with Newey-West (HAC) correction
- `stargazer` for professional formatting of regression tables
