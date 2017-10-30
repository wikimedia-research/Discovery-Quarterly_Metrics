#!/usr/bin/env Rscript

# Kartotherian tile usage

args = commandArgs(trailingOnly = TRUE)
yr_quarter <- args[1]

message("\nFetching maps tile data leading up to ", Sys.Date(), "\n")

suppressPackageStartupMessages({
  library(magrittr)
  library(glue)
})

tsv_path <- glue("{yr_quarter}/data/maps/tiles.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

suppressWarnings(
  tiles <- readr::read_tsv(
    "/srv/published-datasets/discovery/metrics/maps/tile_aggregates_no_automata.tsv",
    col_types = "Dcddcciidddd"
  )
)

results <- tiles %>%
  dplyr::filter(date >= (Sys.Date() - 180)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(tiles = sum(total), avg = sum(total) / sum(users))

readr::write_tsv(results, tsv_path)
message("\nData written to ", tsv_path, "\n")
