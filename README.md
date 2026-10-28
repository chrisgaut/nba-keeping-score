# nba-keeping-score
Automated daily email notifications summarizing the previous day's NBA games.

Each morning a GitHub Actions job finds the previous night's games, scrapes
their box scores from [Basketball Reference](https://www.basketball-reference.com)
(basic and advanced box scores, line score, four factors), and emails them.

## Setup

Requires R (4.5+). Dependencies are pinned with [renv](https://rstudio.github.io/renv/):

```r
renv::restore()
```

## Usage

All commands run from the project root.

```bash
# Rebuild data/schedule.csv for the current season (or pass e.g. 2027)
Rscript R/schedule.R

# Run the tests (uses saved pages in tests/testthat/fixtures, no network)
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Project layout

| Path | Purpose |
|---|---|
| `R/utils.R` | Dates (Eastern time), throttled HTTP, commented-table handling |
| `R/schedule.R` | Builds `data/schedule.csv` from the season's monthly schedule pages |
| `R/scrape.R` | Fetches and parses a box score page |
| `data/schedule.csv` | Every game of the season: date, teams, game id, box score URL |
| `tests/testthat/` | Parser tests against saved Basketball Reference pages |

## Being polite to Basketball Reference

Sports Reference blocks clients that make more than ~20 requests per minute.
All requests go through `bref_get()`, which sends an identifying user agent,
limits to one request every 4 seconds, and backs off on errors. A daily run
makes at most ~15 requests. Please keep these limits if you reuse this code,
and review the [Sports Reference terms of use](https://www.sports-reference.com/termsofuse.html).
