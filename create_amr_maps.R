# =============================================================================
# Regional notification rates for selected antimicrobial resistance phenotypes in Sweden (2015-2025)
# Choropleth Maps Comparing Pre-COVID, COVID, and Post-COVID Periods
# =============================================================================
#
# This script generates a combined figure showing regional mean notification rates
# for ESBL-CARBA, VRE, and MRSA across three time periods:
#   - Pre-COVID (2015-2019)
#   - COVID (2020-2021)
#   - Post-COVID (2022-2025)
#
# Data source: Swedish Public Health Agency (Folkhälsomyndigheten)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LOAD PACKAGES
# -----------------------------------------------------------------------------

pkgs <- c("tidyverse", "sf", "rnaturalearth", "ggspatial", 
          "RColorBrewer", "patchwork", "ggrepel")

to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 2. LOAD AND PREPARE DATA
# -----------------------------------------------------------------------------

# Read notification rate data
dat <- readr::read_csv("AMR_Associated_Infections.csv", show_col_types = FALSE) %>%
  dplyr::filter(year >= 2015, year <= 2025) %>%
  tidyr::pivot_longer(
    cols = c(esbl_carba_incidence, vre_incidence, mrsa_incidence),
    names_to = "pathogen_type",
    values_to = "incidence_per_100k"
  ) %>%
  dplyr::mutate(
    pathogen_type = dplyr::case_when(
      pathogen_type == "esbl_carba_incidence" ~ "ESBL-CARBA",
      pathogen_type == "vre_incidence" ~ "VRE",
      pathogen_type == "mrsa_incidence" ~ "MRSA"
    ),
    period = dplyr::case_when(
      year <= 2019 ~ "pre_covid",
      year %in% 2020:2021 ~ "covid",
      year >= 2022 ~ "post_covid"
    )
  )

# Calculate mean notification rate by region, phenotype, and period
map_dat <- dat %>%
  dplyr::group_by(region, pathogen_type, period) %>%
  dplyr::summarise(mean_inc100k = mean(incidence_per_100k, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    period_label = factor(
      dplyr::case_when(
        period == "pre_covid" ~ "Pre-COVID (2015-2019)",
        period == "covid" ~ "COVID (2020-2021)",
        period == "post_covid" ~ "Post-COVID (2022-2025)"
      ),
      levels = c("Pre-COVID (2015-2019)", "COVID (2020-2021)", "Post-COVID (2022-2025)")
    )
  )

# -----------------------------------------------------------------------------
# 3. LOAD SPATIAL DATA
# -----------------------------------------------------------------------------

# Swedish county boundaries
swe1 <- rnaturalearth::ne_states(country = "Sweden", returnclass = "sf") %>%
  dplyr::select(-dplyr::any_of("region")) %>%
  dplyr::rename(region = name)

# Neighboring countries for reference map
neighbors <- rnaturalearth::ne_countries(
  country = c("Norway", "Finland", "Denmark", "Estonia", "Latvia", 
              "Lithuania", "Poland", "Germany"),
  scale = "medium",
  returnclass = "sf"
)

# County centroids for label placement
swe1_centroids <- swe1 %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry()

# -----------------------------------------------------------------------------
# 4. DEFINE MAP CREATION FUNCTION
# -----------------------------------------------------------------------------

create_pathogen_map <- function(pathogen_name, show_period_labels = FALSE, 
                                 show_scale_bar = FALSE) {
  

  pathogen_dat <- map_dat %>% dplyr::filter(pathogen_type == pathogen_name)
  map_sf <- swe1 %>% dplyr::left_join(pathogen_dat, by = "region")
  
  pathogen_values <- pathogen_dat$mean_inc100k[!is.na(pathogen_dat$mean_inc100k)]
  pathogen_breaks <- quantile(pathogen_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  
  p <- ggplot2::ggplot(map_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = mean_inc100k), color = "white", linewidth = 0.4) +
    ggplot2::scale_fill_gradientn(
      name = paste0(pathogen_name, "\n(notifications per 100,000)"),
      colors = RColorBrewer::brewer.pal(9, "YlOrRd"),
      na.value = "grey95",
      breaks = pathogen_breaks,
      labels = function(x) round(x, 1),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 0.6,
        barheight = 12,
        frame.colour = "grey30",
        frame.linewidth = 0.3,
        ticks.colour = "grey30",
        ticks.linewidth = 0.3
      )
    ) +
    ggplot2::facet_wrap(~period_label, ncol = 3) +
    ggplot2::theme_minimal(base_family = "serif") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.justification = c(0, 0.5),
      legend.title = ggplot2::element_text(size = 9, face = "bold", family = "serif",
                                            margin = ggplot2::margin(b = 8)),
      legend.text = ggplot2::element_text(size = 8, family = "serif"),
      legend.margin = ggplot2::margin(l = 5),
      strip.text = if (show_period_labels) {
        ggplot2::element_text(size = 11, face = "bold", family = "serif",
                               color = "grey20", margin = ggplot2::margin(t = 5, b = 8))
      } else {
        ggplot2::element_blank()
      },
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2),
      panel.spacing = ggplot2::unit(0.15, "cm")
    )
  
  if (show_scale_bar) {
    p <- p + ggspatial::annotation_scale(
      location = "bl", width_hint = 0.12,
      bar_cols = c("grey20", "white"), line_col = "grey20",
      text_col = "grey20", text_cex = 0.6, text_family = "serif",
      style = "bar", unit_category = "metric",
      pad_x = ggplot2::unit(0.3, "cm"), pad_y = ggplot2::unit(0.3, "cm")
    )
  }
  
  return(p)
}

# -----------------------------------------------------------------------------
# 5. CREATE INDIVIDUAL PATHOGEN MAPS
# -----------------------------------------------------------------------------

p_esbl <- create_pathogen_map("ESBL-CARBA", show_period_labels = TRUE) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = ggplot2::unit(0.2, "cm"), pad_y = ggplot2::unit(0.2, "cm"),
    height = ggplot2::unit(0.7, "cm"), width = ggplot2::unit(0.5, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey20", "white"), line_col = "grey20",
      text_col = "grey20", text_size = 7
    )
  )

p_vre <- create_pathogen_map("VRE")
p_mrsa <- create_pathogen_map("MRSA", show_scale_bar = TRUE)

# -----------------------------------------------------------------------------
# 6. CREATE REFERENCE MAP OF SWEDEN
# -----------------------------------------------------------------------------

p_reference <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = neighbors, fill = "#c2b280", color = "#8b7355", linewidth = 0.2) +
  ggplot2::geom_sf(data = swe1, fill = "#8fbc8f", color = "#556b2f", linewidth = 0.4) +
  ggrepel::geom_text_repel(
    data = swe1_centroids,
    ggplot2::aes(x = lon, y = lat, label = region),
    size = 3.2, family = "serif", fontface = "bold", color = "grey20",
    segment.color = "grey40", segment.size = 0.3, segment.alpha = 0.6,
    box.padding = 0.4, point.padding = 0.25, force = 2.5,
    max.overlaps = 25, min.segment.length = 0
  ) +
  ggplot2::coord_sf(xlim = c(10, 25), ylim = c(53, 70.5), expand = FALSE) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = ggplot2::unit(0.8, "cm"), pad_y = ggplot2::unit(0.3, "cm"),
    height = ggplot2::unit(0.8, "cm"), width = ggplot2::unit(0.6, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("white", "grey40"), line_col = "grey20",
      text_col = "white", text_size = 8
    )
  ) +
  ggspatial::annotation_scale(
    location = "br", width_hint = 0.3,
    bar_cols = c("white", "grey40"), line_col = "grey20",
    text_col = "white", text_cex = 0.7, text_family = "serif",
    style = "bar", unit_category = "metric",
    pad_x = ggplot2::unit(4, "cm"), pad_y = ggplot2::unit(4, "cm")
  ) +
  ggplot2::theme_void(base_family = "serif") +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#4682b4", color = NA),
    plot.background = ggplot2::element_rect(fill = "#4682b4", color = NA),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )

# -----------------------------------------------------------------------------
# 7. COMBINE AND SAVE FINAL FIGURE
# -----------------------------------------------------------------------------

# Stack pathogen maps vertically
pathogen_stack <- (p_esbl + ggplot2::theme(plot.margin = ggplot2::margin(5, 2, 5, 2))) / 
                  (p_vre + ggplot2::theme(plot.margin = ggplot2::margin(5, 2, 5, 2))) / 
                  (p_mrsa + ggplot2::theme(plot.margin = ggplot2::margin(5, 2, 5, 2))) +
  patchwork::plot_layout(heights = c(1.1, 1, 1))

# Combine with reference map
combined_map <- (pathogen_stack | p_reference) + 
  patchwork::plot_layout(widths = c(2, 1.2))

# Export
ggplot2::ggsave(
  filename = "map_amr_infections_combined.png",
  plot = combined_map,
  width = 16,
  height = 14,
  dpi = 600,
  bg = "white"
)

message("Done! Created: map_amr_infections_combined.png")

