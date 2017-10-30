#!/usr/bin/env Rscript

# Wikipedia.org bounce rate by platform

args = commandArgs(trailingOnly = TRUE)
yr_quarter <- args[1]

message("\nFetching portal events for the 90 days leading up to ", Sys.Date(), "\n")

suppressPackageStartupMessages({
  library(magrittr)
  library(glue)
})

if (!dir.exists(glue::glue("{yr_quarter}/data/portal"))) {
  dir.create(glue::glue("{yr_quarter}/data/portal"), recursive = TRUE)
}

query <- "SELECT
  timestamp AS ts,
  userAgent AS user_agent,
  event_session_id AS session,
  event_event_type AS type,
  event_section_used AS section_used
FROM WikipediaPortal_15890769
WHERE
  LEFT(timestamp, 8) = '{condensed_date}'
  AND (
    event_cohort IS NULL
    OR event_cohort IN('null', 'baseline')
  )
  AND event_country != 'US'
  AND event_event_type IN('landing', 'clickthrough');"

null2na <- function(x) {
  return(lapply(x, function(y) {
    if (is.null(y)) {
      return(as.character(NA))
    } else {
      return(y)
    }
  }))
}

# con <- RMySQL::dbConnect(RMySQL::MySQL(), host = "127.0.0.1", group = "client", dbname = "log", port = 3307)
con <- wmf::mysql_connect("log")
results <- dplyr::bind_rows(lapply(
  seq(Sys.Date() - 91, Sys.Date() - 1, by = "day"),
  function(date) {
    message("Fetching data from ", format(date, "%Y-%m-%d"))
    condensed_date <- format(date, "%Y%m%d")
    query <- glue(query, .open = "{", .close = "}")
    result <- wmf::mysql_read(query, "log", con)
    result$ts <- lubridate::ymd_hms(result$ts)
    user_agents <- purrr::map_df(result$user_agent, ~ null2na(jsonlite::fromJSON(.x, simplifyVector = FALSE)))
    return(cbind(date = date, result[, setdiff(names(result), "user_agent")], user_agents))
  }
))
wmf::mysql_disconnect(con)

results <- results %>%
  dplyr::mutate(
    is_mobile = browser_family %in% c("Opera Mini") | grepl("^Symbian", os_family) |
      os_family %in% c("iOS", "Android", "Firefox OS", "BlackBerry OS", "Chrome OS", "Kindle", "Windows Phone") |
      grepl("(phone)|(mobile)|(tablet)|(lumia)", device_family, ignore.case = TRUE)
  ) %>%
  dplyr::arrange(date, session, ts) %>%
  dplyr::group_by(date, session) %>%
  dplyr::mutate(valid = any("landing" %in% type)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(valid) %>%
  dplyr::select(-valid)

nth_non_na <- function(x, n) {
  if (sum(!is.na(x)) > 0) {
    if (n == Inf) {
      return(rev(x[!is.na(x)])[1])
    } else {
      return(x[!is.na(x)][n])
    }
  } else {
    return(as.character(NA))
  }
}

sessions <- results %>%
  dplyr::group_by(date, is_mobile, session) %>%
  dplyr::summarize(
    clickthrough = any(type == "clickthrough"),
    last_action = nth_non_na(section_used, Inf),
    first_action = nth_non_na(section_used, 1)
  ) %>%
  dplyr::ungroup()

most_common <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(as.character(NA))
  } else {
    if (length(x) > 0) {
      y <- names(head(sort(table(x), decreasing = TRUE), 1))
      if (is.null(y)) {
        warning("x: ", paste0(x, collapse = ", "))
        return(as.character(NA))
      } else {
        return(y)
      }
    } else {
      return(as.character(NA))
    }
  }
}

write_tsv <- function(x, name) {
  readr::write_tsv(x, name)
  message("\nData written to ", name, "\n")
}

sessions %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    sessions = n(),
    mobile = sum(is_mobile),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = round(1 - ctr, 4),
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  write_tsv(glue("{yr_quarter}/data/portal/overall.tsv"))

sessions %>%
  dplyr::mutate(device = dplyr::if_else(is_mobile, "mobile", "desktop")) %>%
  dplyr::group_by(date, device) %>%
  dplyr::summarize(
    sessions = n(),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = round(1 - ctr, 4),
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  write_tsv(glue("{yr_quarter}/data/portal/device.tsv"))
