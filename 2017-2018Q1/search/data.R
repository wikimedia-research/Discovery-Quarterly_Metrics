# Get search_result_pages data of July
start_date <- as.Date("2017-07-01")
end_date <- as.Date("2017-07-31")
daily_serp_july <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  query <- paste("USE wmf;
    SELECT
      date,
      access_method,
      agent_type,
      COUNT(DISTINCT CONCAT(client_ip, user_agent, query)) AS n_search,
      COUNT(*) AS n_serp,
      COUNT(DISTINCT CONCAT(client_ip, user_agent)) AS n_user
    FROM (
      SELECT
        CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
        access_method,
        client_ip,
        user_agent,
        agent_type,
        PARSE_URL(CONCAT('http://', uri_host, uri_path, uri_query), 'QUERY', 'search') AS query
      FROM webrequest",
      clause_data$date_clause,
      " AND webrequest_source = 'text'
        AND is_pageview
        -- pageviews that are search results pages
        AND page_id IS NULL
        AND (
        LENGTH(PARSE_URL(CONCAT('http://', uri_host, uri_path, uri_query), 'QUERY', 'search')) > 0
        OR LENGTH(PARSE_URL(CONCAT('http://', uri_host, uri_path, uri_query), 'QUERY', 'searchToken')) > 0
        )
    ) AS serp
    GROUP BY date, access_method, agent_type;") 
  results <- wmf::query_hive(query)
  return(results)
}))
readr::write_rds(daily_serp_july, "data/quarterly_metrics/2017_2018_Q1/daily_serp_july.rds", "gz")
# Local
system("scp chelsyx@stat5:~/data/quarterly_metrics/2017_2018_Q1/daily_serp_july.rds 2017-2018Q1/search/data/")

# Get pageviews_from_fulltext_search data of July
start_date <- as.Date("2017-07-01")
end_date <- as.Date("2017-07-31")
daily_pv_from_serp_july <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  query <- paste("USE wmf;
    SELECT
      date,
      access_method,
      agent_type,
      COUNT(DISTINCT CONCAT(client_ip, user_agent, query)) AS n_search,
      COUNT(*) AS n_pv,
      COUNT(DISTINCT CONCAT(client_ip, user_agent)) AS n_user
    FROM (
      SELECT
        CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
        access_method,
        client_ip,
        user_agent,
        agent_type,
        PARSE_URL(referer, 'QUERY', 'search') AS query
      FROM webrequest",
      clause_data$date_clause,
      " AND webrequest_source = 'text'
        AND is_pageview
        -- only those that have been referred by a search results page:
        AND referer_class = 'internal'
        AND (
        LENGTH(PARSE_URL(referer, 'QUERY', 'search')) > 0
        OR LENGTH(PARSE_URL(referer, 'QUERY', 'searchToken')) > 0
        )
    ) AS pv
    GROUP BY date, access_method, agent_type;") 
  results <- wmf::query_hive(query)
  return(results)
}))
readr::write_rds(daily_pv_from_serp_july, "data/quarterly_metrics/2017_2018_Q1/daily_pv_from_serp_july.rds", "gz")
# Local
system("scp chelsyx@stat5:~/data/quarterly_metrics/2017_2018_Q1/daily_pv_from_serp_july.rds 2017-2018Q1/search/data/")

# Get auto vs fulltext ctr
query <- "
SELECT 
  LEFT(timestamp, 8) AS date,
  timestamp,
  event_uniqueId AS event_id,
  event_searchSessionId AS session_id,
  event_pageViewId AS page_id,
  event_source,
  event_action AS action
FROM TestSearchSatisfaction2_16909631
WHERE LEFT(timestamp, 8) >= '20170701'
AND LEFT(timestamp, 8) < '20171001'
AND event_action IN('searchResultPage', 'click')
AND (event_subTest IS NULL OR event_subTest IN ('null', 'baseline'));
"
results <- wmf::mysql_read(query, "log")
# De-duplicate, clean, and sort:
results$timestamp <- as.POSIXct(results$timestamp, format = "%Y%m%d%H%M%S")
results <- results[order(results$event_id, results$timestamp), ]
results <- results[!duplicated(results$event_id, fromLast = TRUE), ]
results <- data.table::as.data.table(results[order(results$session_id, results$page_id, results$timestamp), ])
# Remove outliers (see https://phabricator.wikimedia.org/T150539):
serp_counts <- results[action == "searchResultPage", list(SERPs = .N), by = "session_id"]
valid_sessions <- serp_counts$session_id[serp_counts$SERPs < 1000]
# Filter:
results <- results[results$session_id %in% valid_sessions, ]
clickthroughs <- results[, {
  data.frame(clickthrough = any(action == "click", na.rm = TRUE))
}, by = c("date", "event_source", "session_id", "page_id")]
ctr_desktop_q1 <- clickthroughs %>%
  dplyr::group_by(date, event_source) %>%
  dplyr::summarize(clickthroughs = sum(clickthrough),
                   "Result pages opened" = n(),
                   "search sessions" = length(unique(session_id))
  )
readr::write_rds(ctr_desktop_q1, "data/quarterly_metrics/2017_2018_Q1/ctr_desktop_q1.rds", "gz")
# Local
system("scp chelsyx@stat5:~/data/quarterly_metrics/2017_2018_Q1/ctr_desktop_q1.rds 2017-2018Q1/search/data/")
