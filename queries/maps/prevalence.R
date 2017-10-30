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

mapframe_prevalence <- readr::read_tsv(
  "/srv/published-datasets/discovery/metrics/maps/mapframe_prevalence.tsv",
  col_types = "Dciii"
)
maplink_prevalence <- readr::read_tsv(
  "/srv/published-datasets/discovery/metrics/maps/maplink_prevalence.tsv",
  col_types = "Dciii"
)

lang_proj <- polloi::get_langproj()
mapframe_prevalence %<>% dplyr::left_join(lang_proj, by = c("wiki" = "wikiid"))
maplink_prevalence %<>% dplyr::left_join(lang_proj, by = c("wiki" = "wikiid"))

mapframe_prevalence %>%
  dplyr::filter(date == "2017-09-30") %>%
  dplyr::group_by(language, project) %>%
  dplyr::summarize(prevalence = sum(mapframe_articles) / sum(total_articles)) %>%
  dplyr::ungroup() %>%
  # dplyr::top_n(10, prevalence) %>%
  dplyr::arrange(desc(prevalence)) %>%
  dplyr::transmute(
    project = paste0(dplyr::if_else(is.na(language), "", paste0(language, " ")), project),
    prevalence = prevalence
  ) %>%
  readr::write_tsv(sub("prevalence.tsv", "maplink_prevalence.tsv", tsv_path, fixed = TRUE))
maplink_prevalence %>%
  dplyr::filter(date == "2017-09-30") %>%
  dplyr::group_by(language, project) %>%
  dplyr::summarize(prevalence = sum(maplink_articles) / sum(total_articles)) %>%
  dplyr::ungroup() %>%
  # dplyr::top_n(10, prevalence) %>%
  dplyr::arrange(desc(prevalence)) %>%
  dplyr::transmute(
    project = paste0(dplyr::if_else(is.na(language), "", paste0(language, " ")), project),
    prevalence = prevalence
  ) %>%
  readr::write_tsv(sub("prevalence.tsv", "mapframe_prevalence.tsv", tsv_path, fixed = TRUE))

message("\nData written!\n")
