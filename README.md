## Overview

This repository contains the R code used in the paper:

Baes et al. (2026). *A quantitative analysis to detect mass mortality events (wrecks) in Procellariiformes*. Journal of Applied Ecology.

Laura Baes, Ana Carolina Ewbank, Henrique Chupil, Carolina Reigada

Date created: 06-22-2026

Any doubts, please contact: laurabaescaetano@gmail.com

The workflow was developed to identify mass mortality events (MMEs) from long-term seabird stranding records using a sequential analytical framework combining:

1. Time-series decomposition;
2. Outlier detection using the interquartile range (IQR);
3. Changepoint detection;
4. Kernel density estimation (KDE);
5. Hotspot characterization.

Although developed for Procellariiformes stranded along the Brazilian coast, the framework can be adapted to other taxa and geographic regions.
Data used for this code is available from the Dryad Digital Repository: https://doi.org/10.5061/dryad.cc2fqz6nc (Baes et al., 2026). 

## Workflow

### 1. Summary script
Loads Brazilian spatial datasets (country boundaries, coastline, states and regions) used throughout the analyses and figures.

### 2. Summary epidemiology
Summarizes the dataset by species, sex, age class and condition (alive/dead).

### 3. Temporal analysis – all species – Brazil
Performs STL time-series decomposition and identifies candidate mortality peaks using IQR-based outlier detection on the remainder component.

### 4. Deposition rates for each month
Calculates monthly deposition-rate metrics, including daily mean strandings, standard deviation and mean + 2 SD thresholds.

### 5. Changepoint function
Defines the changepoint detection function used to identify abrupt changes in daily stranding rates within candidate mortality periods.

### 6. Creating a linearized coastline for Brazil
Linearizes the Brazilian coastline and projects stranding locations onto a one-dimensional coastal distance gradient.

### 7. Isopleth function for hotspots
Creates a function to calculate hotspot metrics from KDE isopleths, including spatial extent, density and number of stranded birds.

### 8. Sequential analytical steps
Applies changepoint detection and kernel density estimation to each candidate mass mortality event identified during the temporal analysis.

### 9. Final plots
Produces all figures presented in the manuscript.

### 10. Hotspot analysis
Summarizes hotspot characteristics for all detected mass mortality events.

### 11. Bandwidth sensitivity test
Evaluates kernel density bandwidth selection through sensitivity and scale-space analyses.

## Requirements

The analyses were performed in R (version 4.5.2).

Main packages:

- readxl
- rnaturalearth
- rnaturalearthdata
- sf
- plyr
- dplyr
- ggplot2
- gridExtra
- lme4
- tidyr
- lubridate
- changepoint
- ggtext
- patchwork
- lwgeom
