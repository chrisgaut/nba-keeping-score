# nba-keeping-score
Automated daily email notifications summarizing the previous day's NBA games.

Each morning a GitHub Actions job finds the previous night's games, scrapes
their box scores from [Basketball Reference](https://www.basketball-reference.com)
(box scores, line score, four factors), and emails them.

## Setup

Requires R (4.5+). Dependencies are pinned with [renv](https://rstudio.github.io/renv/):

```r
renv::restore()
```

### Email credentials

Sending uses a Gmail [app password](https://myaccount.google.com/apppasswords)
(requires 2-Step Verification), never your account password. For local runs,
put these in a `.Renviron` file in the project root (it is git-ignored):

```
GMAIL_USER=you@gmail.com
GMAIL_APP_PASSWORD=your-16-character-app-password
```

`EMAIL_TO` is optional and defaults to `GMAIL_USER`. In GitHub Actions the same
names are set as repository secrets.

## Usage

All commands run from the project root.

```bash
# Email last night's box scores (Eastern time)
Rscript R/main.R

# A specific night; --dry-run builds the email without sending it
Rscript R/main.R --date 2026-10-24 --dry-run

# Rebuild data/schedule.csv for the current season (or pass e.g. 2027)
Rscript R/schedule.R

# Run the tests (uses saved pages in tests/testthat/fixtures, no network)
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Project layout

| Path | Purpose |
|---|---|
| `R/main.R` | Daily entry point: schedule refresh, scraping, player cache, email |
| `R/utils.R` | Dates (Eastern time), throttled HTTP, commented-table handling |
| `R/schedule.R` | Builds `data/schedule.csv` from the season's monthly schedule pages |
| `R/scrape.R` | Fetches and parses a box score page |
| `R/players.R` | Maintains the player id cache (seed with `Rscript R/players.R`) |
| `R/email.R` | Builds the HTML email and sends it through Gmail SMTP |
| `data/schedule.csv` | Every game of the season: date, teams, game id, box score URL (each run refreshes the current month) |
| `data/players.csv` | Player id cache: name, current team, player page URL, last game seen |
| `tests/testthat/` | Parser tests against saved Basketball Reference pages |

## Being polite to Basketball Reference

Sports Reference blocks clients that make more than ~20 requests per minute.
All requests go through `bref_get()`, which sends an identifying user agent,
limits to one request every 4 seconds, and backs off on errors. A daily run
makes at most ~15 requests. Please keep these limits if you reuse this code,
and review the [Sports Reference terms of use](https://www.sports-reference.com/termsofuse.html).
