#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(pxweb)
})

# ------------------------------------------------------------
# ITS sanity-check figure(s): observed vs fitted vs counterfactual
# Model: NB ITS with step/slope changes at 2020 and 2022 + region FE + offset(log(pop)).
# Output: PNGs only (not added to any PDF).
# ------------------------------------------------------------

dir.create("amr_its_plots", showWarnings = FALSE)

dat <- readr::read_csv("AMR_Associated_Infections.csv", show_col_types = FALSE) %>%
  dplyr::filter(year >= 2015, year <= 2025)

# ---- SCB population helpers (same logic as the Rmds) ----
fetch_scb_population_county <- function(years = 2015:2024,
                                       cache_path = "../scb_population_county_2015_2024.csv") {
  if (file.exists(cache_path)) {
    cached <- readr::read_csv(cache_path, show_col_types = FALSE)
    if ("scb_region" %in% names(cached) && all(nchar(as.character(cached$scb_region)) == 2)) {
      return(cached)
    }
  }

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy"
  md <- pxweb::pxweb_get(url)

  region_var <- md[["variables"]][[1]]
  civil_var  <- md[["variables"]][[2]]
  age_var    <- md[["variables"]][[3]]
  sex_var    <- md[["variables"]][[4]]
  tid_var    <- md[["variables"]][[6]]

  county_region_codes <- region_var$values[nchar(region_var$values) == 2 & region_var$values != "00"]
  years_chr <- as.character(years)
  years_chr <- years_chr[years_chr %in% tid_var$values]

  q <- pxweb::pxweb_query(list(
    Region = county_region_codes,
    Civilstand = civil_var$values,
    # IMPORTANT: use total age only to avoid double-counting (table includes both Totalt and single ages)
    Alder = if ("tot" %in% age_var$values) "tot" else age_var$values[grepl("^tot", tolower(age_var$valueTexts))][1],
    Kon = sex_var$values,
    ContentsCode = "BE0101N1",
    Tid = years_chr
  ))

  res <- pxweb::pxweb_get_data(url, q)
  df <- if (is.data.frame(res)) res else pxweb::pxweb_as_data_frame(res, column.name.type = "code", variable.value.type = "code")

  region_col <- dplyr::case_when(
    "Region" %in% names(df) ~ "Region",
    "region" %in% names(df) ~ "region",
    TRUE ~ names(df)[1]
  )
  year_col <- dplyr::case_when(
    "Tid" %in% names(df) ~ "Tid",
    "year" %in% names(df) ~ "year",
    TRUE ~ names(df)[length(names(df)) - 1]
  )
  value_col <- dplyr::case_when(
    "value" %in% names(df) ~ "value",
    "values" %in% names(df) ~ "values",
    TRUE ~ names(df)[length(names(df))]
  )

  pop <- df %>%
    dplyr::rename(scb_region = !!region_col, year = !!year_col, value = !!value_col) %>%
    dplyr::mutate(
      year = as.integer(.data$year),
      value = as.numeric(.data$value),
      scb_region = as.character(.data$scb_region)
    ) %>%
    dplyr::left_join(
      tibble::tibble(code = region_var$values, label = region_var$valueTexts),
      by = c("scb_region" = "label")
    ) %>%
    dplyr::mutate(scb_region = dplyr::if_else(!is.na(.data$code), .data$code, .data$scb_region)) %>%
    dplyr::select(-code) %>%
    dplyr::group_by(scb_region, year) %>%
    dplyr::summarise(population = sum(value, na.rm = TRUE), .groups = "drop")

  readr::write_csv(pop, cache_path)
  pop
}

fetch_scb_population_2025_latest_month <- function(cache_prefix = "../scb_population_county_2025_") {
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkManadCKM"
  md <- pxweb::pxweb_get(url)
  vars <- md[["variables"]]
  codes <- vapply(vars, function(v) v[["code"]], character(1))

  region_var <- vars[[which(codes == "Region")]]
  age_var <- vars[[which(codes == "Alder")]]
  sex_var <- vars[[which(codes == "Kon")]]
  cont_var <- vars[[which(codes == "ContentsCode")]]
  tid_var <- vars[[which(codes == "Tid")]]

  county_region_codes <- region_var$values[nchar(region_var$values) == 2 & region_var$values != "00"]
  tid_2025 <- tid_var$values[grepl("^2025M", tid_var$values)]
  latest_tid <- max(tid_2025)

  cache_path <- paste0(cache_prefix, latest_tid, ".csv")
  if (file.exists(cache_path)) {
    cached <- readr::read_csv(cache_path, show_col_types = FALSE)
    if ("scb_region" %in% names(cached) && all(nchar(as.character(cached$scb_region)) == 2)) {
      return(cached)
    }
  }

  q <- pxweb::pxweb_query(list(
    Region = county_region_codes,
    # IMPORTANT: monthly table includes many overlapping age groups; query totals only
    Alder = if ("TotSA" %in% age_var$values) "TotSA" else age_var$values[1],
    Kon = if ("TotSa" %in% sex_var$values) "TotSa" else sex_var$values[1],
    ContentsCode = cont_var$values[1],
    Tid = latest_tid
  ))

  res <- pxweb::pxweb_get_data(url, q)
  df <- if (is.data.frame(res)) res else pxweb::pxweb_as_data_frame(res, column.name.type = "code", variable.value.type = "code")

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
    dplyr::rename(scb_region = !!region_col, value = !!value_col) %>%
    dplyr::mutate(value = as.numeric(.data$value), scb_region = as.character(.data$scb_region)) %>%
    dplyr::left_join(
      tibble::tibble(code = region_var$values, label = region_var$valueTexts),
      by = c("scb_region" = "label")
    ) %>%
    dplyr::mutate(scb_region = dplyr::if_else(!is.na(.data$code), .data$code, .data$scb_region)) %>%
    dplyr::select(-code) %>%
    dplyr::group_by(scb_region) %>%
    dplyr::summarise(population = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(year = 2025L)

  readr::write_csv(pop_2025, cache_path)
  pop_2025
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

pop_all_years <- bind_rows(
  fetch_scb_population_county(2015:2024),
  fetch_scb_population_2025_latest_month()
)

data_long <- dat %>%
  tidyr::pivot_longer(
    cols = c(esbl_carba_cases, vre_cases, mrsa_cases),
    names_to = "pathogen",
    values_to = "cases"
  ) %>%
  dplyr::mutate(pathogen = dplyr::case_when(
    pathogen == "esbl_carba_cases" ~ "ESBL-CARBA",
    pathogen == "vre_cases" ~ "VRE",
    pathogen == "mrsa_cases" ~ "MRSA"
  )) %>%
  dplyr::left_join(region_key, by = "region") %>%
  dplyr::left_join(pop_all_years, by = c("scb_region", "year")) %>%
  dplyr::mutate(
    region = factor(region),
    log_pop = log(population),
    t = year - 2015,
    covid_step = as.integer(year >= 2020),
    covid_slope = pmax(0, year - 2020),
    post_step = as.integer(year >= 2022),
    post_slope = pmax(0, year - 2022)
  ) %>%
  dplyr::filter(!is.na(population), population > 0, !is.na(cases), cases >= 0)

predict_scenarios <- function(d, model) {
  d_actual <- d

  d_no_int <- d %>%
    dplyr::mutate(
      covid_step = 0L,
      covid_slope = 0,
      post_step = 0L,
      post_slope = 0
    )

  tibble::tibble(
    year = d$year,
    region = d$region,
    cases_obs = d$cases,
    pop = d$population,
    pred_cases_actual = as.numeric(stats::predict(model, newdata = d_actual, type = "response")),
    pred_cases_no_interruptions = as.numeric(stats::predict(model, newdata = d_no_int, type = "response"))
  )
}

make_plot_for_pathogen <- function(pathogen_name) {
  d <- data_long %>% dplyr::filter(pathogen == pathogen_name)

  m <- MASS::glm.nb(
    cases ~ t + covid_step + covid_slope + post_step + post_slope + region + offset(log_pop),
    data = d
  )

  preds <- predict_scenarios(d, m)

  nat <- preds %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      cases_obs = sum(cases_obs, na.rm = TRUE),
      pop = sum(pop, na.rm = TRUE),
      pred_cases_actual = sum(pred_cases_actual, na.rm = TRUE),
      pred_cases_no_interruptions = sum(pred_cases_no_interruptions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      inc_obs = (cases_obs / pop) * 100000,
      inc_fit = (pred_cases_actual / pop) * 100000,
      inc_cf_no_int = (pred_cases_no_interruptions / pop) * 100000
    )

  p <- ggplot(nat, aes(x = year)) +
    geom_line(aes(y = inc_obs, color = "Observed"), linewidth = 1.1) +
    geom_point(aes(y = inc_obs, color = "Observed"), size = 1.6) +
    geom_line(aes(y = inc_fit, color = "Fitted (ITS)"), linewidth = 1.1) +
    geom_line(aes(y = inc_cf_no_int, color = "Counterfactual: no interruptions"), linewidth = 1.1, linetype = "dashed") +
    geom_vline(xintercept = 2020, color = "grey45", linewidth = 0.6) +
    geom_vline(xintercept = 2022, color = "grey45", linewidth = 0.6) +
    scale_color_manual(
      values = c(
        "Observed" = "black",
        "Fitted (ITS)" = "#2C7FB8",
        "Counterfactual: no interruptions" = "#D95F02"
      )
    ) +
    scale_x_continuous(
      limits = c(2015, 2025),
      breaks = 2015:2025
    ) +
    labs(
      x = "Year",
      y = "Notification rate per 100,000",
      color = NULL
    ) +
    theme_gray(base_size = 12, base_family = "serif") +
    theme(
      legend.position = "right",
      legend.key = element_rect(fill = "grey92", color = NA),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )

  # Create safe filename from pathogen name
  safe_name <- tolower(pathogen_name) %>%
    stringr::str_replace("/", "_") %>%
    stringr::str_replace(" ", "_")
  
  out_path <- file.path("amr_its_plots", paste0("its_observed_fitted_counterfactual_", safe_name, ".png"))
  ggsave(out_path, p, width = 11, height = 6.5, dpi = 600)

  nat %>% readr::write_csv(file.path("amr_its_plots", paste0("its_national_series_", safe_name, ".csv")))
}

purrr::walk(c("ESBL-CARBA", "VRE", "MRSA"), make_plot_for_pathogen)

cat("Done. Wrote PNGs to amr_its_plots/:\n")
cat("- its_observed_fitted_counterfactual_esbl_carba.png\n")
cat("- its_observed_fitted_counterfactual_vre.png\n")
cat("- its_observed_fitted_counterfactual_mrsa.png\n")


