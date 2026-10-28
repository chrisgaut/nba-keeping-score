# Builds data/schedule.csv from Basketball Reference's monthly schedule pages.
#
# Usage (from the project root):
#   Rscript R/schedule.R          # current season
#   Rscript R/schedule.R 2027     # a specific season (labelled by end year)

SCHEDULE_PATH <- "data/schedule.csv"

schedule_index_path <- function(season) {
  sprintf("/leagues/NBA_%d_games.html", season)
}

# The season index page shows the first month and links to every month,
# including playoff months once they are scheduled.
schedule_month_paths <- function(index_html, season) {
  page <- xml2::read_html(index_html)
  hrefs <- rvest::html_attr(rvest::html_elements(page, "div.filter a"), "href")
  pattern <- sprintf("^/leagues/NBA_%d_games-[a-z]+\\.html$", season)
  unique(hrefs[grepl(pattern, hrefs)])
}

# One row per game. The game id (e.g. "202610240DEN") is present for every
# game, played or not, and is all we need to build the box score URL.
parse_schedule_page <- function(html) {
  page <- xml2::read_html(html)
  rows <- rvest::html_elements(page, "table#schedule tbody tr:not(.thead)")

  cell <- function(stat) {
    rvest::html_element(rows, sprintf("[data-stat='%s']", stat))
  }
  cell_text <- function(stat) {
    trimws(rvest::html_text(cell(stat)))
  }
  team_abbr <- function(stat) {
    # csk looks like "BOS.202610200DET"
    sub("\\..*$", "", rvest::html_attr(cell(stat), "csk"))
  }

  game_id <- rvest::html_attr(cell("date_game"), "csk")
  has_box <- !is.na(rvest::html_attr(rvest::html_element(cell("box_score_text"), "a"), "href"))

  tibble::tibble(
    game_id = game_id,
    game_date = as.Date(substr(game_id, 1, 8), format = "%Y%m%d"),
    start_time_et = cell_text("game_start_time"),
    visitor_abbr = team_abbr("visitor_team_name"),
    visitor_name = cell_text("visitor_team_name"),
    home_abbr = team_abbr("home_team_name"),
    home_name = cell_text("home_team_name"),
    box_url = paste0(BREF_BASE_URL, "/boxscores/", game_id, ".html"),
    status = ifelse(has_box, "final", "scheduled"),
    notes = cell_text("game_remarks")
  ) |>
    dplyr::filter(!is.na(game_id))
}

build_schedule <- function(season) {
  index_html <- bref_get(schedule_index_path(season))
  month_paths <- schedule_month_paths(index_html, season)
  log_info("Found ", length(month_paths), " schedule months for ", season)

  # The index page already holds the first month (the first link); don't
  # fetch it twice.
  first_month <- parse_schedule_page(index_html)
  pages <- lapply(month_paths[-1], \(path) parse_schedule_page(bref_get(path)))

  dplyr::bind_rows(first_month, pages) |>
    dplyr::distinct(game_id, .keep_all = TRUE) |>
    dplyr::arrange(game_date, game_id)
}

update_schedule <- function(season, path = SCHEDULE_PATH) {
  schedule <- build_schedule(season)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(schedule, path, na = "")
  log_info("Wrote ", nrow(schedule), " games to ", path)
  invisible(schedule)
}

read_schedule <- function(path = SCHEDULE_PATH) {
  readr::read_csv(
    path,
    col_types = readr::cols(game_date = readr::col_date(), .default = readr::col_character())
  )
}

games_on <- function(schedule, date) {
  dplyr::filter(schedule, game_date == as.Date(date))
}

if (sys.nframe() == 0L) {
  source("R/utils.R")
  args <- commandArgs(trailingOnly = TRUE)
  season <- if (length(args) > 0) as.integer(args[[1]]) else season_for_date(today_et())
  update_schedule(season)
}
