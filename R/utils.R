# Shared helpers: dates, logging, and polite HTTP access to Basketball Reference.

BREF_BASE_URL <- "https://www.basketball-reference.com"
BREF_USER_AGENT <- "nba-keeping-score (personal daily box score email; github.com/chrisgaut/nba-keeping-score)"
NBA_TZ <- "America/New_York"

log_info <- function(...) {
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ...)
}

# "Today" and "last night" are defined in Eastern time, regardless of the
# runner's clock (GitHub Actions runs in UTC).
today_et <- function() {
  as.Date(lubridate::with_tz(Sys.time(), NBA_TZ))
}

yesterday_et <- function() {
  today_et() - 1
}

# Basketball Reference labels seasons by the year they end: the 2026-27
# season is "2027". Anything from August onward belongs to the next season.
season_for_date <- function(date) {
  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  ifelse(month >= 8L, year + 1L, year)
}

# Sports Reference blocks clients making more than ~20 requests/minute, so
# every request goes through one shared throttle (~1 request every 4 seconds)
# and retries transient failures with backoff.
bref_get <- function(path) {
  url <- paste0(BREF_BASE_URL, path)
  log_info("GET ", url)

  resp <- httr2::request(url) |>
    httr2::req_user_agent(BREF_USER_AGENT) |>
    # A bucket of 1 token refilled every 4s: no bursts, ~15 requests/minute.
    httr2::req_throttle(capacity = 1, fill_time_s = 4, realm = "basketball-reference") |>
    httr2::req_retry(
      max_tries = 4,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = \(attempt) 30 * attempt
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_perform()

  httr2::resp_body_string(resp)
}

# Sports Reference ships some tables (line score, four factors, and sometimes
# others) inside HTML comments and un-comments them with JavaScript. Parse the
# page plus every commented-out fragment that contains a table, so tables can
# be found by id wherever they live.
read_bref_html <- function(html) {
  page <- xml2::read_html(html)
  comments <- xml2::xml_text(xml2::xml_find_all(page, "//comment()"))
  comments <- comments[grepl("<table", comments, fixed = TRUE)]
  fragments <- lapply(comments, xml2::read_html)
  list(page = page, fragments = fragments)
}

find_bref_table <- function(doc, id) {
  selector <- paste0("table#", id)
  for (node in c(list(doc$page), doc$fragments)) {
    table <- rvest::html_element(node, selector)
    if (!inherits(table, "xml_missing")) return(table)
  }
  NULL
}
