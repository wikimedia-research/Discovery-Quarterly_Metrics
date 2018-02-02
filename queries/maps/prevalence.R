#!/usr/bin/env Rscript

# Mapframe & Maplink prevalence across wikis

args = commandArgs(trailingOnly = TRUE)
yr_quarter <- args[1]

message("\nFetching mapframe & maplink prevalence stats\n")

suppressPackageStartupMessages({
  library(magrittr)
  library(glue)
})

tsv_path <- glue("{yr_quarter}/data/maps/prevalence.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

mapframe_prevalence <- polloi::read_dataset(
  "discovery/metrics/maps/mapframe_prevalence.tsv",
  col_types = "Dciii"
)
maplink_prevalence <- polloi::read_dataset(
  "discovery/metrics/maps/maplink_prevalence.tsv",
  col_types = "Dciii"
)

lang_proj <- polloi::get_langproj()
mapframe_prevalence %<>% dplyr::left_join(lang_proj, by = c("wiki" = "wikiid"))
maplink_prevalence %<>% dplyr::left_join(lang_proj, by = c("wiki" = "wikiid"))

mapframe_prevalence %>%
  dplyr::filter(date < "2018-01-01", date >= "2017-07-01") %>%
  dplyr::mutate(quarter = dplyr::if_else(date >= "2017-10-01", "recent", "previous")) %>%
  dplyr::group_by(quarter, date, language, project) %>%
  dplyr::summarize(prevalence = sum(mapframe_articles) / sum(total_articles)) %>%
  dplyr::group_by(quarter, language, project) %>%
  dplyr::summarize(prevalence = mean(prevalence, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(quarter, prevalence) %>%
  dplyr::mutate(QoQ = (recent - previous) / previous) %>%
  dplyr::arrange(desc(QoQ)) %>%
  dplyr::transmute(
    project = paste0(dplyr::if_else(is.na(language), "", paste0(language, " ")), project),
    `last quarter` = recent, `quarter before that` = previous, QoQ = QoQ
  ) %>%
  readr::write_tsv(sub("prevalence.tsv", "maplink_prevalence.tsv", tsv_path, fixed = TRUE))
maplink_prevalence %>%
  dplyr::filter(date < "2018-01-01", date >= "2017-07-01") %>%
  dplyr::mutate(quarter = dplyr::if_else(date >= "2017-10-01", "recent", "previous")) %>%
  dplyr::group_by(quarter, date, language, project) %>%
  dplyr::summarize(prevalence = sum(maplink_articles) / sum(total_articles)) %>%
  dplyr::group_by(quarter, language, project) %>%
  dplyr::summarize(prevalence = mean(prevalence, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(quarter, prevalence) %>%
  dplyr::mutate(QoQ = (recent - previous) / previous) %>%
  dplyr::arrange(desc(QoQ)) %>%
  dplyr::transmute(
    project = paste0(dplyr::if_else(is.na(language), "", paste0(language, " ")), project),
    `last quarter` = recent, `quarter before that` = previous, QoQ = QoQ
  ) %>%
  readr::write_tsv(sub("prevalence.tsv", "mapframe_prevalence.tsv", tsv_path, fixed = TRUE))

message("\nData written!\n")
