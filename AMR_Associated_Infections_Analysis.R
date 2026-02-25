# =============================================================================
# Setup
# =============================================================================
# Load packages
library(tidyverse)
library(knitr)
library(kableExtra)
library(MASS)
library(broom)
library(pxweb)

# =============================================================================
# Load data
# =============================================================================
# nolint start
# Load the dataset
dat <- read_csv("AMR_Associated_Infections.csv", show_col_types = FALSE) %>%
  filter(year >= 2015, year <= 2025)

# Fetch SCB population by county and year (2015-2024, annual)
fetch_scb_population_county <- function(years = 2015:2024,
                                       cache_path = "../scb_population_county_2015_2024.csv") {
  if (file.exists(cache_path)) {
    cached <- read_csv(cache_path, show_col_types = FALSE)
    # If cached regions are already län codes, use as-is
    if ("scb_region" %in% names(cached) && all(nchar(as.character(cached$scb_region)) == 2)) {
      return(cached)
    }
    # Otherwise, fall through and rebuild (older cache used text labels)
  }

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy"
  md <- tryCatch(
    pxweb::pxweb_get(url),
    error = function(e) stop("SCB API call failed for annual population table: ", e$message)
  )

  # Dimension metadata
  region_var <- md[["variables"]][[1]]
  civil_var  <- md[["variables"]][[2]]
  age_var    <- md[["variables"]][[3]]
  sex_var    <- md[["variables"]][[4]]
  cont_var   <- md[["variables"]][[5]]
  tid_var    <- md[["variables"]][[6]]

  # County regions are the 21 two-digit län codes (exclude "00" = Riket)
  county_region_codes <- region_var$values[nchar(region_var$values) == 2 & region_var$values != "00"]

  # Safety: should be 21 counties
  if (length(county_region_codes) != 21) {
    stop("SCB query setup error: expected 21 county regions, found ", length(county_region_codes), ".")
  }

  years_chr <- as.character(years)
  years_chr <- years_chr[years_chr %in% tid_var$values]

  if (length(years_chr) == 0) {
    stop("SCB query setup error: none of the requested years exist in the SCB table.")
  }

  q <- pxweb::pxweb_query(list(
    Region = county_region_codes,
    Civilstand = civil_var$values,
    # IMPORTANT: use total age only to avoid double-counting (table includes both Totalt and single ages)
    Alder = if ("tot" %in% age_var$values) "tot" else age_var$values[grepl("^tot", tolower(age_var$valueTexts))][1],
    Kon = sex_var$values,
    ContentsCode = "BE0101N1",  # Folkmängd
    Tid = years_chr
  ))

  # pxweb_get_data may return either a pxweb object or a data.frame depending on pxweb version
  res <- tryCatch(
    pxweb::pxweb_get_data(url, q),
    error = function(e) stop("SCB API call failed to fetch population data: ", e$message)
  )
  df <- if (is.data.frame(res)) res else pxweb::pxweb_as_data_frame(res, column.name.type = "code", variable.value.type = "code")

  # Standardize key column names defensively
  region_col <- dplyr::case_when(
    "Region" %in% names(df) ~ "Region",
    "region" %in% names(df) ~ "region",
    TRUE ~ names(df)[1]
  )
  year_col <- dplyr::case_when(
    "Tid" %in% names(df) ~ "Tid",
    "år" %in% names(df) ~ "år",
    "year" %in% names(df) ~ "year",
    TRUE ~ names(df)[length(names(df)) - 1]
  )
  value_col <- dplyr::case_when(
    "value" %in% names(df) ~ "value",
    "values" %in% names(df) ~ "values",
    TRUE ~ names(df)[length(names(df))]
  )

  pop <- df %>%
    dplyr::rename(
      scb_region = !!region_col,
      year = !!year_col,
      value = !!value_col
    ) %>%
    mutate(
      year = as.integer(.data$year),
      value = as.numeric(.data$value),
      scb_region = as.character(.data$scb_region)
    ) %>%
    # If Region came back as text labels, map to SCB codes using metadata
    left_join(
      tibble::tibble(code = region_var$values, label = region_var$valueTexts),
      by = c("scb_region" = "label")
    ) %>%
    mutate(scb_region = dplyr::if_else(!is.na(.data$code), .data$code, .data$scb_region)) %>%
    dplyr::select(-code) %>%
    group_by(scb_region, year) %>%
    summarise(population = sum(value, na.rm = TRUE), .groups = "drop")

  write_csv(pop, cache_path)
  pop
}

region_key <- tibble::tribble(
  ~region, ~scb_region,
  "Stockholm", "01",
  "Uppsala", "03",
  "Södermanland", "04",
  "Östergötland", "05",
  "Jönköping", "06",
  "Kronoberg", "07",
  "Kalmar", "08",
  "Gotland", "09",
  "Blekinge", "10",
  "Skåne", "12",
  "Halland", "13",
  "Västra Götaland", "14",
  "Värmland", "17",
  "Orebro", "18",
  "Västmanland", "19",
  "Dalarna", "20",
  "Gävleborg", "21",
  "Västernorrland", "22",
  "Jämtland", "23",
  "Västerbotten", "24",
  "Norrbotten", "25"
)

pop_2015_2024 <- fetch_scb_population_county(2015:2024)

# Check if 2025 is available in the annual table, otherwise use monthly table
url_annual <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy"
md_annual <- tryCatch(
  pxweb::pxweb_get(url_annual),
  error = function(e) stop("SCB API call failed to check 2025 availability: ", e$message)
)
tid_var_annual <- md_annual[["variables"]][[6]]
has_2025_annual <- "2025" %in% tid_var_annual$values

if (has_2025_annual) {
  # Use annual table for 2025
  pop_2025 <- fetch_scb_population_county(2025, cache_path = "../scb_population_county_2025.csv")
} else {
  # Fall back to monthly table for 2025
  # Fetch SCB population for 2025 from monthly table (latest available month)
  fetch_scb_population_2025_latest_month <- function(cache_prefix = "../scb_population_county_2025_") {
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkManadCKM"
  md <- tryCatch(
    pxweb::pxweb_get(url),
    error = function(e) stop("SCB API call failed for monthly population table: ", e$message)
  )
  vars <- md[["variables"]]
  codes <- vapply(vars, function(v) v[["code"]], character(1))

  region_var <- vars[[which(codes == "Region")]]
  age_var <- vars[[which(codes == "Alder")]]
  sex_var <- vars[[which(codes == "Kon")]]
  cont_var <- vars[[which(codes == "ContentsCode")]]
  tid_var <- vars[[which(codes == "Tid")]]

  # County regions are the 21 two-digit län codes (exclude "00" = Riket)
  county_region_codes <- region_var$values[nchar(region_var$values) == 2 & region_var$values != "00"]
  if (length(county_region_codes) != 21) {
    stop("SCB monthly query setup error: expected 21 county regions, found ", length(county_region_codes), ".")
  }

  # Pick the latest available month in 2025 (e.g., 2025M10)
  tid_2025 <- tid_var$values[grepl("^2025M", tid_var$values)]
  if (length(tid_2025) == 0) {
    stop("SCB monthly table does not contain any 2025 months (Tid starting with '2025M').")
  }
  latest_tid <- max(tid_2025)

  cache_path <- paste0(cache_prefix, latest_tid, ".csv")
  if (file.exists(cache_path)) {
    cached <- read_csv(cache_path, show_col_types = FALSE)
    if ("scb_region" %in% names(cached) && all(nchar(as.character(cached$scb_region)) == 2)) {
      return(cached)
    }
    # Otherwise, rebuild (older cache used text labels)
  }

  q <- pxweb::pxweb_query(list(
    Region = county_region_codes,
    # IMPORTANT: monthly table includes many overlapping age groups; query totals only
    Alder = if ("TotSA" %in% age_var$values) "TotSA" else age_var$values[1],
    Kon = if ("TotSa" %in% sex_var$values) "TotSa" else sex_var$values[1],
    ContentsCode = cont_var$values[1],
    Tid = latest_tid
  ))

  res <- tryCatch(
    pxweb::pxweb_get_data(url, q),
    error = function(e) stop("SCB API call failed to fetch monthly population data: ", e$message)
  )
  df <- if (is.data.frame(res)) res else pxweb::pxweb_as_data_frame(res, column.name.type = "code", variable.value.type = "code")

  # Standardize key column names defensively
  region_col <- dplyr::case_when(
    "Region" %in% names(df) ~ "Region",
    "region" %in% names(df) ~ "region",
    TRUE ~ names(df)[1]
  )
  value_col <- dplyr::case_when(
    "value" %in% names(df) ~ "value",
    "values" %in% names(df) ~ "values",
    TRUE ~ names(df)[length(names(df))]
  )

  pop_2025 <- df %>%
    dplyr::rename(
      scb_region = !!region_col,
      value = !!value_col
    ) %>%
    mutate(
      value = as.numeric(.data$value),
      scb_region = as.character(.data$scb_region)
    ) %>%
    # If Region came back as text labels, map to SCB codes using metadata
    left_join(
      tibble::tibble(code = region_var$values, label = region_var$valueTexts),
      by = c("scb_region" = "label")
    ) %>%
    mutate(scb_region = dplyr::if_else(!is.na(.data$code), .data$code, .data$scb_region)) %>%
    dplyr::select(-code) %>%
    group_by(scb_region) %>%
    summarise(population = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2025L)

  write_csv(pop_2025, cache_path)
  pop_2025
  }
  pop_2025 <- fetch_scb_population_2025_latest_month()
}

pop_all_years <- bind_rows(pop_2015_2024, pop_2025)

# Reshape case + incidence data to long format
dat_long <- dat %>%
  pivot_longer(
    cols = c(esbl_carba_cases, vre_cases, mrsa_cases),
    names_to = "pathogen",
    values_to = "cases"
  ) %>%
  mutate(    pathogen = case_when(
    pathogen == "esbl_carba_cases" ~ "ESBL/CARBA",
    pathogen == "vre_cases" ~ "VRE",
    pathogen == "mrsa_cases" ~ "MRSA"
  )) %>%
  left_join(
    dat %>%
      pivot_longer(
        cols = c(esbl_carba_incidence, vre_incidence, mrsa_incidence),
        names_to = "pathogen",
        values_to = "incidence"
      ) %>%
      mutate(pathogen = case_when(
        pathogen == "esbl_carba_incidence" ~ "ESBL/CARBA",
        pathogen == "vre_incidence" ~ "VRE",
        pathogen == "mrsa_incidence" ~ "MRSA"
      )) %>%
      dplyr::select(region, year, pathogen, incidence),
    by = c("region", "year", "pathogen")
  ) %>%
  left_join(region_key, by = "region") %>%
  left_join(pop_all_years, by = c("scb_region", "year")) %>%
  mutate(
    period = case_when(
      year <= 2019 ~ "Pre-COVID",
      year %in% 2020:2021 ~ "COVID",
      year >= 2022 ~ "Post-COVID"
    ),
    period = factor(period, levels = c("Pre-COVID", "COVID", "Post-COVID")),
    region = factor(region)
  ) %>%
  filter(!is.na(population), population > 0)

# Fail fast if any county-year is missing population after join
missing_pop <- dat_long %>%
  group_by(region, year) %>%
  summarise(pop_ok = all(!is.na(population)), .groups = "drop") %>%
  filter(!pop_ok)

if (nrow(missing_pop) > 0) {
  stop("Missing SCB population after join for: ", paste0(missing_pop$region, " ", missing_pop$year, collapse = ", "))
}
# nolint end

# =============================================================================
# Summary table (by pathogen and period)
# =============================================================================
# Summary statistics by pathogen and period
# First compute national annual totals, then summarize by period
national_annual <- dat_long %>%
  group_by(pathogen, period, year) %>%
  summarise(
    annual_cases = sum(cases, na.rm = TRUE),
    annual_incidence = weighted.mean(incidence, population, na.rm = TRUE),
    .groups = "drop"
  )

summary_stats <- national_annual %>%
  group_by(pathogen, period) %>%
  summarise(
    `Total Cases` = sum(annual_cases, na.rm = TRUE),
    `Mean Annual Cases` = round(mean(annual_cases, na.rm = TRUE), 1),
    `SD` = round(sd(annual_cases, na.rm = TRUE), 1),
    `Mean Incidence` = round(mean(annual_incidence, na.rm = TRUE), 2),
    .groups = "drop"
  )

summary_stats %>%
  kable(
    caption = "Summary of AMR-associated infection cases and incidence rates by pathogen and COVID period",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10
  ) %>%
  collapse_rows(columns = 1, valign = "middle")

# =============================================================================
# National trends (by year)
# =============================================================================
national_trends <- dat_long %>%
  group_by(pathogen, year) %>%
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    mean_incidence = round(mean(incidence, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    period = case_when(
      year <= 2019 ~ "Pre-COVID",
      year %in% 2020:2021 ~ "COVID",
      year >= 2022 ~ "Post-COVID"
    )
  )

national_trends %>%
  pivot_wider(
    id_cols = year,
    names_from = pathogen,
    values_from = c(total_cases, mean_incidence),
    names_glue = "{pathogen}_{.value}"
  ) %>%
  dplyr::select(
    Year = year,
    `ESBL/CARBA Cases` = `ESBL/CARBA_total_cases`,
    `ESBL/CARBA Inc.` = `ESBL/CARBA_mean_incidence`,
    `VRE Cases` = VRE_total_cases,
    `VRE Inc.` = VRE_mean_incidence,
    `MRSA Cases` = MRSA_total_cases,
    `MRSA Inc.` = MRSA_mean_incidence
  ) %>%
  kable(
    caption = "National case totals and mean regional incidence rates by year",
    booktabs = TRUE,
    align = c("c", rep("r", 6))
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  ) %>%
  add_header_above(c(" " = 1, "ESBL/CARBA" = 2, "VRE" = 2, "MRSA" = 2))

# =============================================================================
# Regression models (period-based IRR)
# =============================================================================
# nolint start
# Function to fit Negative Binomial model with population offset, adjusted for region
fit_irr_model <- function(data, pathogen_name, ref_period = "Pre-COVID") {

  pathogen_data <- data %>%
    filter(pathogen == pathogen_name) %>%
    filter(!is.na(cases), cases >= 0, !is.na(population), population > 0) %>%
    mutate(
      period = relevel(period, ref = ref_period),
      log_pop = log(population)
    )

  # Fit negative binomial regression with population offset, adjusted for region
  # (No fallback model: fail loudly if NB cannot be fitted.)
  model <- MASS::glm.nb(cases ~ period + region + offset(log_pop), data = pathogen_data)

  # Extract coefficients with confidence intervals (only period effects)
  coefs <- tidy(model, conf.int = TRUE, exponentiate = FALSE) %>%
    filter(str_detect(term, "period")) %>%
    mutate(
      IRR = exp(estimate),
      IRR_lower = exp(estimate - 1.96 * std.error),
      IRR_upper = exp(estimate + 1.96 * std.error),
      comparison = case_when(
        term == "periodCOVID" & ref_period == "Pre-COVID" ~ "COVID vs Pre-COVID",
        term == "periodPost-COVID" & ref_period == "Pre-COVID" ~ "Post-COVID vs Pre-COVID",
        term == "periodPre-COVID" & ref_period == "COVID" ~ "Pre-COVID vs COVID",
        term == "periodPost-COVID" & ref_period == "COVID" ~ "Post-COVID vs COVID"
      ),
      pathogen = pathogen_name
    ) %>%
    filter(!is.na(comparison)) %>%
    dplyr::select(pathogen, comparison, IRR, IRR_lower, IRR_upper, p.value)

  return(coefs)
}

# Fit models for each pathogen with Pre-COVID as reference
irr_results <- bind_rows(
  fit_irr_model(dat_long, "ESBL/CARBA", "Pre-COVID"),
  fit_irr_model(dat_long, "VRE", "Pre-COVID"),
  fit_irr_model(dat_long, "MRSA", "Pre-COVID")
)

# Also calculate Post-COVID vs COVID
irr_covid_ref <- bind_rows(
  fit_irr_model(dat_long, "ESBL/CARBA", "COVID"),
  fit_irr_model(dat_long, "VRE", "COVID"),
  fit_irr_model(dat_long, "MRSA", "COVID")
) %>%
  filter(comparison == "Post-COVID vs COVID")

# Combine all results
all_irr <- bind_rows(irr_results, irr_covid_ref) %>%
  arrange(pathogen, comparison)
# nolint end

# =============================================================================
# IRR table (formatted)
# =============================================================================
all_irr %>%
  mutate(
    IRR_formatted = sprintf("%.2f", IRR),
    CI_formatted = sprintf("(%.2f - %.2f)", IRR_lower, IRR_upper),
    p_formatted = case_when(
      p.value < 0.001 ~ "<0.001",
      p.value < 0.01 ~ sprintf("%.3f", p.value),
      TRUE ~ sprintf("%.2f", p.value)
    )
  ) %>%
  dplyr::select(
    Pathogen = pathogen,
    Comparison = comparison,
    IRR = IRR_formatted,
    `95% CI` = CI_formatted,
    `P-value` = p_formatted
  ) %>%
  kable(
    caption = "Incidence Rate Ratios (IRRs) comparing COVID periods, adjusted for region",
    booktabs = TRUE,
    align = c("l", "l", "r", "c", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10
  ) %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  footnote(
    general = "IRR < 1 indicates lower incidence compared to reference period; IRR > 1 indicates higher incidence.",
    general_title = "Note: ",
    footnote_as_chunk = TRUE
  )

# =============================================================================
# Interrupted time-series (ITS) models
# =============================================================================
# nolint start
dat_its <- dat_long %>%
  mutate(
    log_pop = log(population),
    t = year - 2015,
    covid_step = as.integer(year >= 2020),
    covid_slope = pmax(0, year - 2020),
    post_step = as.integer(year >= 2022),
    post_slope = pmax(0, year - 2022)
  )

fit_its <- function(data, pathogen_name) {
  d <- data %>% filter(pathogen == pathogen_name)

  m <- MASS::glm.nb(
    cases ~ t + covid_step + covid_slope + post_step + post_slope + region + offset(log_pop),
    data = d
  )

  eff <- broom::tidy(m) %>%
    filter(term %in% c("covid_step", "covid_slope", "post_step", "post_slope")) %>%
    mutate(
      IRR = exp(estimate),
      IRR_lower = exp(estimate - 1.96 * std.error),
      IRR_upper = exp(estimate + 1.96 * std.error),
      effect = dplyr::recode(
        term,
        covid_step = "COVID step (from 2020)",
        covid_slope = "COVID slope change (per year)",
        post_step = "Post step (from 2022)",
        post_slope = "Post slope change (per year)"
      ),
      pathogen = pathogen_name
    ) %>%
    dplyr::select(pathogen, effect, IRR, IRR_lower, IRR_upper, p.value)

  list(model = m, effects = eff)
}

fit_period <- function(data, pathogen_name) {
  d <- data %>%
    filter(pathogen == pathogen_name) %>%
    mutate(log_pop = log(population))

  MASS::glm.nb(cases ~ period + region + offset(log_pop), data = d)
}

its_fits <- list(
  `ESBL/CARBA` = fit_its(dat_its, "ESBL/CARBA"),
  VRE = fit_its(dat_its, "VRE"),
  MRSA = fit_its(dat_its, "MRSA")
)

its_effects <- bind_rows(
  its_fits$`ESBL/CARBA`$effects,
  its_fits$VRE$effects,
  its_fits$MRSA$effects
)

aic_tbl <- tibble::tibble(
  Pathogen = c("ESBL/CARBA", "VRE", "MRSA"),
  `AIC (Period model)` = c(
    AIC(fit_period(dat_long, "ESBL/CARBA")),
    AIC(fit_period(dat_long, "VRE")),
    AIC(fit_period(dat_long, "MRSA"))
  ),
  `AIC (ITS model)` = c(
    AIC(its_fits$`ESBL/CARBA`$model),
    AIC(its_fits$VRE$model),
    AIC(its_fits$MRSA$model)
  )
) %>%
  mutate(
    `AIC (Period model)` = round(`AIC (Period model)`, 1),
    `AIC (ITS model)` = round(`AIC (ITS model)`, 1)
  )
# nolint end

# =============================================================================
# ITS effects table (formatted)
# =============================================================================
its_effects %>%
  mutate(
    IRR_formatted = sprintf("%.2f", IRR),
    CI_formatted = sprintf("(%.2f - %.2f)", IRR_lower, IRR_upper),
    p_formatted = case_when(
      p.value < 0.001 ~ "<0.001",
      p.value < 0.01 ~ sprintf("%.3f", p.value),
      TRUE ~ sprintf("%.2f", p.value)
    )
  ) %>%
  dplyr::select(
    Pathogen = pathogen,
    Effect = effect,
    IRR = IRR_formatted,
    `95% CI` = CI_formatted,
    `P-value` = p_formatted
  ) %>%
  kable(
    caption = "Interrupted time-series effects (negative binomial + offset + region FE)",
    booktabs = TRUE,
    align = c("l", "l", "r", "c", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10
  ) %>%
  collapse_rows(columns = 1, valign = "middle")

# =============================================================================
# AIC comparison (period vs ITS)
# =============================================================================
aic_tbl %>%
  kable(
    caption = "Model fit comparison by AIC (lower is better)",
    booktabs = TRUE,
    align = c("l", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10
  )

# =============================================================================
# Counterfactual figures (ITS)
# =============================================================================
f <- "amr_its_plots/its_observed_fitted_counterfactual_esbl_carba.png"
if (file.exists(f)) knitr::include_graphics(f)

f <- "amr_its_plots/its_observed_fitted_counterfactual_vre.png"
if (file.exists(f)) knitr::include_graphics(f)

f <- "amr_its_plots/its_observed_fitted_counterfactual_mrsa.png"
if (file.exists(f)) knitr::include_graphics(f)

# =============================================================================
# Interpretation (helper and formatted rows for narrative)
# =============================================================================
# nolint start
# Helper to pull one row and format IRR + CI + p
get_row <- function(pathogen_name, comparison_name) {
  all_irr %>%
    filter(pathogen == pathogen_name, comparison == comparison_name) %>%
    slice(1) %>%
    mutate(
      irr_txt = sprintf("%.2f", IRR),
      ci_txt = sprintf("(%.2f–%.2f)", IRR_lower, IRR_upper),
      p_txt = case_when(
        p.value < 0.001 ~ "<0.001",
        p.value < 0.01 ~ sprintf("%.3f", p.value),
        TRUE ~ sprintf("%.2f", p.value)
      ),
      pct_txt = sprintf("%.0f", (IRR - 1) * 100)
    )
}

esbl_covid <- get_row("ESBL/CARBA", "COVID vs Pre-COVID")
esbl_post_pre <- get_row("ESBL/CARBA", "Post-COVID vs Pre-COVID")
esbl_post_covid <- get_row("ESBL/CARBA", "Post-COVID vs COVID")

vre_covid <- get_row("VRE", "COVID vs Pre-COVID")
vre_post_pre <- get_row("VRE", "Post-COVID vs Pre-COVID")
vre_post_covid <- get_row("VRE", "Post-COVID vs COVID")

mrsa_covid <- get_row("MRSA", "COVID vs Pre-COVID")
mrsa_post_pre <- get_row("MRSA", "Post-COVID vs Pre-COVID")
mrsa_post_covid <- get_row("MRSA", "Post-COVID vs COVID")
# nolint end

# =============================================================================
# Map figure (pre-generated)
# =============================================================================
# Include the pre-generated map
if (file.exists("map_amr_infections_combined.png")) {
  knitr::include_graphics("map_amr_infections_combined.png")
}

# =============================================================================
# Appendix: regional summary table
# =============================================================================
# Regional summary
regional_summary <- dat_long %>%
  group_by(region, pathogen, period) %>%
  summarise(
    mean_inc = round(mean(incidence, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(pathogen, period),
    values_from = mean_inc,
    names_glue = "{pathogen}_{period}"
  )

regional_summary %>%
  dplyr::select(
    Region = region,
    starts_with("ESBL/CARBA"),
    starts_with("VRE"),
    starts_with("MRSA")
  ) %>%
  kable(
    caption = "Mean incidence rates (per 100,000) by region, pathogen, and period",
    booktabs = TRUE,
    align = c("l", rep("r", 9)),
    col.names = c("Region",
                  "Pre", "COVID", "Post",
                  "Pre", "COVID", "Post",
                  "Pre", "COVID", "Post")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 8
  ) %>%
  add_header_above(c(" " = 1, "ESBL/CARBA" = 3, "VRE" = 3, "MRSA" = 3))

# =============================================================================
# Session info
# =============================================================================
sessionInfo()
