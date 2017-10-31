library(tidyverse)

# SERP visits: desktop vs mobile web vs api
all_api_calls <- polloi::read_dataset("discovery/metrics/search/search_api_usage.tsv", col_types = "Dcci") %>%
  dplyr::filter(date >= "2017-07-01", date <= "2017-09-30", api != "cirrus (more like)") %>%
  group_by(date) %>%
  summarize(calls = sum(calls)) %>%
  rename(n_serp=calls) %>%
  mutate(access_method = "api")

daily_serp <- polloi::read_dataset("discovery/metrics/search/search_result_pages.tsv", col_types = "Dcciii") %>%
  rbind(readr::read_rds("2017-2018Q1/search/data/daily_serp_july.rds")) %>%
  mutate(date = lubridate::ymd(date)) %>%
  filter(date < as.Date("2017-10-01"))

daily_serp %>%
  filter(access_method != "mobile app") %>%
  group_by(date, access_method) %>%
  summarise(n_serp = sum(n_serp)) %>%
  bind_rows(all_api_calls) %>%
  rename(Group = access_method) %>%
  ggplot(aes(x=date, y=n_serp, colour=Group)) + 
  # geom_smooth(method = "lm", formula = y ~ splines::bs(x, 27), se = FALSE, size=1.2) + 
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels=polloi::compress, name = "Number of search requests", trans = scales::sqrt_trans()) +
  ggtitle("Number of search requests", subtitle = "API vs Desktop Full-text vs Mobile Web Full-text. Bots included.") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", legend.justification = "left")

daily_serp %>%
  filter(access_method != "mobile app") %>%
  group_by(date, access_method) %>%
  summarise(n_serp = sum(n_serp)) %>%
  rename(Group = access_method) %>%
  ggplot(aes(x=date, y=n_serp, colour=Group)) + 
  geom_line(size=1.2) +
  geom_smooth(se = FALSE) +
  scale_x_date(name = "Date") +
  scale_y_continuous(labels=polloi::compress, name = "Number of search result pages") +
  scale_color_manual(values=scales::hue_pal()(3)[2:3]) + 
  ggtitle("Desktop Full-text vs Mobile Web Full-text",  subtitle = "Bots included.") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

# clickthrough rate by day
desktop_clt <- polloi::read_dataset("discovery/metrics/search/desktop_event_counts.tsv", col_types = "Dci") %>%
  dplyr::filter(!is.na(action), !is.na(events), date >= "2017-04-01", date < "2017-10-01") %>%
  tidyr::spread(action, events, fill = 0)
mobile_clt <- polloi::read_dataset("discovery/metrics/search/mobile_event_counts.tsv", col_types = "Dci") %>%
  dplyr::filter(!is.na(action), !is.na(events), date >= "2017-04-01", date < "2017-10-01") %>%
  tidyr::spread(action, events, fill = 0)
app_clt <- polloi::read_dataset("discovery/metrics/search/app_event_counts.tsv", col_types = "Dcci") %>%
  dplyr::filter(!is.na(action), !is.na(events), date >= "2017-04-01", date < "2017-10-01") %>%
  dplyr::distinct(date, platform, action, .keep_all = TRUE) %>%
  tidyr::spread(action, events, fill = 0)
clickthrough_rate <- list(
  desktop = dplyr::select(desktop_clt, c(date, clickthroughs, `Result pages opened`)),
  mobile = dplyr::select(mobile_clt, c(date, clickthroughs, `Result pages opened`)),
  app = dplyr::select(app_clt, c(date, clickthroughs, `Result pages opened`))
) %>%
  dplyr::bind_rows(.id = "platform") %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(clickthroughs = sum(clickthroughs), serps = sum(`Result pages opened`)) %>%
  dplyr::transmute(
    date = date,
    `Clickthrough rate` = clickthroughs / serps
  )

clickthrough_rate %>%
  ggplot(aes(x=date, y=`Clickthrough rate`)) + 
  geom_line(size=1.2, color = "#377EB8") +
  geom_smooth(se = FALSE, color = "#377EB8") + 
  annotate("rect",
    xmin=as.Date("2017-04-01"),
    xmax=as.Date("2017-06-30"),
    ymin=-Inf,
    ymax=Inf,
    fill="grey", alpha=0.9) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels=scales::percent, name = "Clickthrough Rate") +
  ggtitle("Clickthrough Rate, by day") +
  theme_minimal(base_size = 15)

# ZRR by day
zrr_all_no_automata <- polloi::read_dataset("discovery/metrics/search/cirrus_query_aggregates_no_automata.tsv", col_types = "Dd") %>%
  dplyr::filter(!is.na(rate), date >= "2017-04-01", date < "2017-10-01")
zrr_all_no_automata %>%
  ggplot(aes(x=date, y=rate)) + 
  geom_line(size=1.2, color = "#E41A1C") +
  geom_smooth(se = FALSE, color = "#E41A1C") + 
  annotate("rect",
           xmin=as.Date("2017-04-01"),
           xmax=as.Date("2017-06-30"),
           ymin=-Inf,
           ymax=Inf,
           fill="grey", alpha=0.5) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels=scales::percent, name = "Zero Results Rate") +
  ggtitle("Zero Results Rate, by day") +
  theme_minimal(base_size = 15)

# Pageviews from full-text search
daily_pv_from_serp <- polloi::read_dataset("discovery/metrics/search/pageviews_from_fulltext_search.tsv", col_types = "Dcciii") %>%
  rbind(readr::read_rds("2017-2018Q1/search/data/daily_pv_from_serp_july.rds")) %>%
  filter(agent_type == "user", date < "2017-10-01") %>%
  mutate(date = lubridate::ymd(date))
daily_pv_from_serp %>%
  group_by(date, access_method) %>%
  summarise(n_pv = sum(n_pv)) %>%
  rename('Access Method' = access_method) %>%
  ggplot(aes(x=date, y=n_pv, fill=`Access Method`, order=desc(`Access Method`))) +
  geom_area() +
  # scale_fill_brewer("Access Method", palette = "Set1") +
  scale_x_date(name = "Date", date_breaks = "1 week", date_labels = "%A\n%b %d") +
  scale_y_continuous(labels=polloi::compress, name = "Pageviews") +
  ggtitle("Pageviews from full-text search") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom") 

# Return rate
desktop_return_rate <- polloi::read_dataset("discovery/metrics/search/desktop_return_rate", col_types = "Diiii")
desktop_return_rate %>%
  mutate(`Return to make different search` = return_to_make_another_search / n_session,
       `Return to the same search page` = return_to_same_search / n_search) %>%
  select(date, `Return to make different search`, `Return to the same search page`) %>%
  gather(key=Type, value = rate, -date) %>%
  ggplot(aes(x=date, y=rate, colour=Type)) + 
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::percent, name = "Return Rate") +
  geom_vline(xintercept = as.numeric(as.Date("2017-04-25")),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2017-04-26"), y = 0.2, label = "sample rate changed", angle = 90) +
  ggtitle("Return rate after users clickthrough on search engine result pages", subtitle = "Full-text search on desktop") +
  labs(caption = "*On April 25th, we changed the sample rates for several projects.") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

# Sister project clickthrough
sister_search_traffic <- polloi::read_dataset("discovery/metrics/search/sister_search_traffic.tsv", col_types = "Dcccli") %>%
  dplyr::mutate(
    project = polloi::capitalize_first_letter(project),
    access_method = polloi::capitalize_first_letter(access_method)
  )

sister_search_traffic %>%
  group_by(date, project) %>%
  summarise(pageviews = sum(pageviews)) %>%
  mutate(Project = factor(project,
                          levels = c("Wiktionary", "Wikibooks", "Wikisource", "Wikiquote", "Wikinews", "Wikiversity", "Wikimedia Commons", "Wikivoyage", "Wikispecies", "Simple Wiktionary"))) %>%
  ggplot(aes(x=date, y=pageviews, fill=Project, order=rev(Project))) +
  geom_area() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Pageviews") +
  scale_fill_brewer("Project", palette = "Paired") +
  ggtitle("Traffic to sister projects from Wikipedia search result pages, broken down by project") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = as.numeric(as.Date("2017-06-15")),
             linetype = "dashed", color = "black") + 
  labs(caption = "*On 2017-06-15 we deployed the sister search feature to all Wikipedia in all languages.")

# sister search prevalence on SERP
sister_search_prevalence <- polloi::read_dataset("discovery/metrics/search/sister_search_prevalence.tsv", col_types = "Dcii") %>%
  dplyr::group_by(wiki_id) %>%
  dplyr::mutate(include = all((has_sister_results + no_sister_results) > 20)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(include) %>%
  dplyr::mutate(prevalence = has_sister_results / (has_sister_results + no_sister_results)) %>%
  dplyr::left_join(
    polloi::get_langproj()[, c("wikiid", "language")],
    by = c("wiki_id" = "wikiid")
  ) %>%
  dplyr::select(c(date, language, prevalence)) %>%
  dplyr::group_by(language) %>%
  dplyr::summarize(median_prev = median(prevalence, na.rm = TRUE)) %>%
  dplyr::arrange(median_prev) %>%
  tail(15)
sister_search_prevalence$language <- factor(sister_search_prevalence$language, levels = sister_search_prevalence$language)

ggplot(sister_search_prevalence, aes(y=language, x=median_prev, label = paste0(round(median_prev * 100, 1), "%"))) + 
  geom_segment(aes(y=language, 
                   yend=language, 
                   x=0, 
                   xend=median_prev), color = "grey50") + 
  geom_point(col=ifelse(sister_search_prevalence$median_prev > 0.7, "tomato2", ifelse(sister_search_prevalence$median_prev > 0.65, "turquoise", "black")), size=5) + 
  geom_text(nudge_x = 0.03) + 
  scale_x_continuous(labels = scales::percent) +
  labs(title="Wikipedia searches that showed sister project search results", 
       subtitle="Median prevalence for top 15 languages", 
       y = "Language",
       x = "Prevalence") +  
  theme_minimal(base_size = 15)

# auto complete vs full-text
zrr_breakdown <- polloi::read_dataset("discovery/metrics/search/cirrus_query_breakdowns_no_automata.tsv", col_types = "Dcd") %>%
  dplyr::filter(!is.na(rate), date >= "2017-07-01", date < "2017-10-01",
                query_type %in% c("comp_suggest", "full_text")) %>%
  dplyr::mutate(query_type = ifelse(query_type == "comp_suggest", "Autocomplete", "Full-text"))
zrr_breakdown %>%
  ggplot(aes(x=date, y=rate, colour=query_type)) + 
  # geom_smooth(method = "lm", formula = y ~ splines::bs(x, 20), se = FALSE, size=1.2) + 
  geom_line(size=1.2) +
  geom_smooth(se = FALSE) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::percent, name = "Zero Results Rate") +
  scale_color_brewer("Search Type", palette = "Set1") +
  ggtitle("Zero Results Rate, by search type") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

ctr_breakdown <- readr::read_rds("2017-2018Q1/search/data/ctr_desktop_q1.rds") %>%
  ungroup %>%
  mutate(
    date = lubridate::ymd(date),
    event_source = ifelse(event_source == "fulltext", "Full-text", "Autocomplete"),
    ctr = clickthroughs / `Result pages opened`)
ctr_breakdown %>%
  ggplot(aes(x=date, y=ctr, colour=event_source)) + 
  geom_line(size=1.2) +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::percent, name = "Clickthrough Rate", trans = scales::sqrt_trans()) +
  scale_color_brewer("Search Type", palette = "Set1") +
  ggtitle("Clickthrough Rate on desktop, by search type") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")
