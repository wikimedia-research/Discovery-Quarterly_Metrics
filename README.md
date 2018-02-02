# Discovery's Quarterly Metrics

Phabricator: [T171528](https://phabricator.wikimedia.org/T171528) & [T178958](https://phabricator.wikimedia.org/T178958)

```bash
QUARTER='2017-2018Q2'
nice ionice ./main.sh ${QUARTER} >> main.log 2>&1
```

Output:

- **${QUARTER}/data/**: has datasets generated by various queries
- **${QUARTER}/figures/**: has all the plots generated by [plot.R](plot.R)

## Dependencies

```R
install.packages(
  c("devtools", "tidyverse", "glue", "ISOcodes"),
  repos = c(CRAN = "https://cran.rstudio.com")
)
devtools::install_git("https://gerrit.wikimedia.org/r/wikimedia/discovery/wmf")
devtools::install_git("https://gerrit.wikimedia.org/r/wikimedia/discovery/polloi")
```

## Metrics

- **Wikimedia Maps**
  - total article, articles with some sort of map (-link or -frame) and maplink prevalence % vs total articles by top languages
  - tiles served breakdown (total tiles and average per user)
- **Search Platform**
  - searches: api vs desktop vs mobile (use splines?)
    - quarter over quarter data changes (if possible)
  - clickthrough rate by day
  - ZRR by day
  - autocomplete vs full-text
  - page views for full-text search (desktop vs mobile)
  - return rate after users clickthrough on SERP (same page, or diff search)
  - sister project clickthrough and prevalence on SERP
  - traffic to sister projects from SERP (all languages)
