#!/usr/bin/env Rscript

# Wikipedia.org PVs by mobile vs desktop

args = commandArgs(trailingOnly = TRUE)
yr_quarter <- args[1]

message("\nFetching portal pageviews by platform for the 60 days leading up to ", Sys.Date(), "\n")

suppressPackageStartupMessages({
  library(glue)
})

tsv_path <- glue("{yr_quarter}/data/portal/pageviews-platform.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

pageviews <- readr::read_tsv(
  "/srv/published-datasets/discovery/metrics/portal/pageviews_by_device.tsv",
  col_types = "Dci"
)

readr::write_tsv(pageviews, tsv_path)
message("\nData written to ", tsv_path, "\n")
