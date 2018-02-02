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

# Wikimedia Maps

## Kartotherian
Data <- readr::read_tsv(glue("{yr_quarter}/data/maps/tiles.tsv"))
p <- ggplot(Data, aes(x = date, y = tiles)) +
  geom_rect(
    data = NULL, xmin = as.numeric(as.Date("2017-10-01")), xmax = as.numeric(as.Date("2017-12-31")), ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "gray90", color = NA
  ) +
  geom_vline(xintercept = as.numeric(as.Date(c("2017-10-01", "2017-12-31"))), color = "gray50") +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date(c("2016-11-16", "2017-08-14"))), linetype = "dashed") +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Tiles served", labels = polloi::compress) +
  scale_color_brewer("Referrer", palette = "Set1") +
  labs(
    title = "Wikimedia Maps: Kartotherian tile server usage",
    subtitle = "Dashed lines indicate Nov '16 launch and August '17 shutdown of Pkget"
  ) +
  theme_minimal(base_size = 14)
(p)
output_figure(p, "maps-kartotherian-total")

# Comparison:
maplink <- readr::read_tsv(glue("{yr_quarter}/data/maps/maplink_prevalence.tsv")) %>%
  dplyr::mutate(feature = "maplink")
mapframe <- readr::read_tsv(glue("{yr_quarter}/data/maps/mapframe_prevalence.tsv")) %>%
  dplyr::mutate(feature = "mapframe")
prevalence <- rbind(maplink, mapframe) %>%
  tidyr::gather(metric, value, -c(project, feature)) %>%
  dplyr::mutate(value = sprintf("%.4f (%0.1f%%)", value, 100 * value)) %>%
  tidyr::spread(metric, value) %>%
  dplyr::arrange(project, feature) %>%
  dplyr::filter(!grepl("NaN", QoQ)) %>%
  dplyr::select(project, feature, `quarter before that`, `last quarter`, QoQ)
rbind(maplink, mapframe) %>%
  dplyr::filter(!is.nan(QoQ), !is.infinite(QoQ)) %>%
  dplyr::group_by(feature) %>%
  dplyr::summarize(average = mean(QoQ, na.rm = TRUE))
