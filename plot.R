#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)
message("\nLoading data and generating plots\n")
yr_quarter <- args[1]
suppressPackageStartupMessages({
  library(glue)
  library(magrittr)
  library(ggplot2)
  library(viridis)
})

fig_path <- glue("{yr_quarter}/figures")

if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}

theme_min <- function(legend.position = "bottom", ...) {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = legend.position, ...)
}

output_figure <- function(plot, filename, legend.position = "bottom", width = 12, height = 6, ...) {
  # return(plot + theme_min())
  ggsave(
    plot = plot + theme_min(legend.position, ...),
    filename = paste0(filename, ".png"), path = fig_path,
    width = width, height = height, units = "in", dpi = 300
  )
  message("Plot saved as ", file.path(fig_path, filename), ".png")
}

# Wikipedia.org Portal

## EventLogging-based Metrics
# Data <- readr::read_tsv(glue("{yr_quarter}/data/portal/overall.tsv"))

### Device
Data <- readr::read_tsv(glue("{yr_quarter}/data/portal/device.tsv"))
p <- ggplot(Data, aes(x = date, y = ctr, color = device)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%a\n%d %b", date_minor_breaks = "1 week") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  scale_color_brewer("Device", palette = "Set1") +
  facet_wrap(~ device) +
  labs(
    title = "Wikipedia.org Portal clickthrough rate by device",
    subtitle = glue("{yr_quarter} data"),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  ) +
  theme_min()
(p)
output_figure(p, "portal-engagement-device")

## Referrer Breakdown
Data <- readr::read_tsv(glue("{yr_quarter}/data/portal/pageviews-referrer.tsv")) %>%
  dplyr::filter(date >= Sys.Date() - 181)
p <- ggplot(Data, aes(x = date, y = pageviews, color = referrer)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_line(alpha = 0.75) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer", palette = "Set1") +
  guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(
    title = "Wikipedia.org Portal pageviews",
    subtitle = glue("{yr_quarter} data")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
(p)
output_figure(p, "portal-pageviews-referrer", width = 5.3 * 1.5, height = 3.4 * 1.5)
p <- Data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = pageviews / sum(pageviews)) %>%
  ggplot(aes(x = date, y = prop, color = referrer)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_line(alpha = 0.75) +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Proportion of pageviews", labels = scales::percent_format()) +
  scale_color_brewer("Referrer", palette = "Set1") +
  guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(
    title = "Wikipedia.org Portal pageviews",
    subtitle = glue("{yr_quarter} data")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
(p)
output_figure(p, "portal-pageviews-proportion", width = 5.3 * 1.5, height = 3.4 * 1.5)

## Platform Breakdown
Data <- readr::read_tsv(glue("{yr_quarter}/data/portal/pageviews-platform.tsv"))
Data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = pageviews / sum(pageviews)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(date == "2017-09-30")
p <- ggplot(Data, aes(x = date, y = pageviews, fill = device)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_area(position = "stack", color = "black") +
  scale_x_date("Date", date_breaks = "1 week", date_labels = "%a\n%d %b") +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_fill_brewer("Device", palette = "Set1") +
  labs(
    title = "Wikipedia.org Portal pageviews by device",
    subtitle = glue("{yr_quarter} data"),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  ) +
  theme_minimal(base_size = 14)
(p)
output_figure(p, "portal-pageviews-platform")

# Wikimedia Maps

## Kartotherian
Data <- readr::read_tsv(glue("{yr_quarter}/data/maps/tiles.tsv"))
p <- ggplot(Data, aes(x = date, y = tiles)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-14")), linetype = "dashed") +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Tiles served", labels = polloi::compress) +
  scale_color_brewer("Referrer", palette = "Set1") +
  labs(
    title = "Wikimedia Maps: Kartotherian tile server usage",
    subtitle = glue("{yr_quarter} data"),
    caption = "Pokemon Go fan site Pkget was shut down on August 14th, 2017"
  ) +
  theme_minimal(base_size = 14)
(p)
output_figure(p, "maps-kartotherian-total")
p <- ggplot(Data, aes(x = date, y = avg)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-07-01")), xmax = as.numeric(as.Date("2017-09-30")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-07-01", "2017-09-30"))), color = "gray50") +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-14")), linetype = "dashed") +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Average tiles per user", labels = polloi::compress) +
  scale_color_brewer("Referrer", palette = "Set1") +
  labs(
    title = "Wikimedia Maps: Kartotherian tile server usage",
    subtitle = glue("{yr_quarter} data"),
    caption = "Pokemon Go fan site Pkget was shut down on August 14th, 2017"
  ) +
  theme_minimal(base_size = 14)
(p)
output_figure(p, "maps-kartotherian-average")

## Prevalence
readr::read_tsv(glue("{yr_quarter}/data/maps/maplink_prevalence.tsv")) %>%
  dplyr::mutate(maplink_prevalence = sprintf("%.1f%%", 100 * prevalence)) %>%
  View
readr::read_tsv(glue("{yr_quarter}/data/maps/mapframe_prevalence.tsv")) %>%
  dplyr::mutate(mapframe_prevalence = sprintf("%.1f%%", 100 * prevalence)) %>%
  View
