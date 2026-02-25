# AMR-Associated Infections Analysis (Sweden 2015–2025)

Analysis of pandemic disruptions and post-COVID patterns of antimicrobial resistance–associated infections in Sweden: regional incidence for **ESBL/CARBA**, **VRE**, and **MRSA**.

## Overview

- **Pre-COVID (2015–2019):** baseline  
- **COVID (2020–2021):** pandemic period  
- **Post-COVID (2022–2025):** recovery  

The analysis uses negative binomial regression (period-based IRR and interrupted time-series) with region fixed effects and log-population offset. Population denominators come from Statistics Sweden (SCB) via the PxWeb API.

## Data

| File | Description |
|------|-------------|
| `AMR_Associated_Infections.csv` | Regional case counts and incidence rates (per 100,000) by year. Source: Swedish Public Health Agency (Folkhälsomyndigheten) / SmiNet. |

Population data are fetched from SCB (annual table `BefolkningNy` for 2015–2024; 2025 uses the monthly table `BefolkManadCKM` if needed). Results are cached under the project or parent folder (e.g. `../scb_population_county_2015_2024.csv`).

## Project structure

| File / folder | Purpose |
|---------------|---------|
| `AMR_Associated_Infections_Analysis.R` | Main script: load data, fetch SCB population, build long-format data, run period and ITS models, produce summary and IRR/ITS tables, optional figures (if pre-generated). |
| `make_amr_its_counterfactual_plots.R` | Build ITS counterfactual figures (observed vs fitted vs no-interruption) and write CSVs + PNGs into `amr_its_plots/`. |
| `make_amr_its_counterfactual_panel.R` | Single panel figure combining ESBL/CARBA, VRE, and MRSA counterfactual plots. |
| `create_amr_maps.R` | Choropleth maps of mean regional incidence by period (Pre-COVID, COVID, Post-COVID) for ESBL-CARBA, VRE, MRSA; outputs `map_amr_infections_combined.png`. |
| `amr_its_plots/` | Output folder for ITS national series CSVs and counterfactual PNGs. |

## Requirements

- **R** (tested with a recent R 4.x).
- **R packages:**  
  `tidyverse`, `MASS`, `broom`, `pxweb`, `knitr`, `kableExtra` for the main script;  
  `make_amr_its_counterfactual_*.R` also use `patchwork` and `cowplot`;  
  `create_amr_maps.R` uses `sf`, `rnaturalearth`, `ggspatial`, `RColorBrewer`, `patchwork`, `ggrepel`.

Install from CRAN if needed, e.g.:

```r
install.packages(c("tidyverse", "MASS", "broom", "pxweb", "knitr", "kableExtra",
                  "patchwork", "cowplot", "sf", "rnaturalearth", "ggspatial",
                  "RColorBrewer", "ggrepel"))
```

## How to run

1. **Set working directory** to the folder that contains `AMR_Associated_Infections.csv` (the `AMR` folder).

2. **Main analysis (tables and model objects):**
   ```r
   source("AMR_Associated_Infections_Analysis.R")
   ```
   This loads data, fetches/caches SCB population, runs period and ITS models, and prints/formats tables (e.g. summary stats, IRR table, ITS effects, AIC). If `amr_its_plots/` and `map_amr_infections_combined.png` already exist, it will include those figures.

3. **ITS counterfactual figures and CSVs** (optional; creates or updates `amr_its_plots/`):
   ```bash
   Rscript make_amr_its_counterfactual_plots.R
   ```
   Or the combined panel:
   ```bash
   Rscript make_amr_its_counterfactual_panel.R
   ```

4. **Maps** (optional; creates `map_amr_infections_combined.png`):
   ```r
   source("create_amr_maps.R")
   ```
   Or from the shell:
   ```bash
   Rscript create_amr_maps.R
   ```

Recommended order: run `make_amr_its_counterfactual_plots.R` (and optionally `create_amr_maps.R`) if you want the main script to display the pre-generated figures; otherwise the main script still runs all models and tables.

## Outputs

- **Console / R environment:** `summary_stats`, `national_trends`, `all_irr`, `its_effects`, `aic_tbl`, `regional_summary`, and formatted `kable` tables.
- **amr_its_plots/:**  
  - CSVs: `its_national_series_esbl_carba.csv`, `its_national_series_vre.csv`, `its_national_series_mrsa.csv` (observed and fitted/counterfactual series).  
  - PNGs: counterfactual plots per pathogen (and panel if you ran the panel script).
- **Root:** `map_amr_infections_combined.png` (from `create_amr_maps.R`).

## References

- Swedish Public Health Agency. [Sjukdomsstatistik / Disease statistics](https://www.folkhalsomyndigheten.se/).
- Statistics Sweden (SCB). [PxWeb API](https://www.scb.se/en/About-us/open-data-api/).
