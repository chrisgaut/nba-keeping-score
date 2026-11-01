# Maintains data/players.csv: a cache of Basketball Reference player ids.
#
# The box score page is the source of truth for ids (every player row carries
# one), so the daily run reads ids straight from the page and merges any new
# or changed players into this file. Seeding from the season totals page fills
# the cache up front.
#
# Usage (from the project root):
#   Rscript R/players.R          # seed from the current season's totals page
#   Rscript R/players.R 2027     # seed from a specific season

PLAYERS_PATH <- "data/players.csv"

player_url <- function(player_id) {
  sprintf("%s/players/%s/%s.html", BREF_BASE_URL, substr(player_id, 1, 1), player_id)
}

empty_players <- function() {
  tibble::tibble(
    player_id = character(),
    player_name = character(),
    team = character(),
    player_url = character(),
    last_seen = as.Date(character())
  )
}

read_players <- function(path = PLAYERS_PATH) {
  if (!file.exists(path)) return(empty_players())
  readr::read_csv(
    path,
    col_types = readr::cols(last_seen = readr::col_date(), .default = readr::col_character())
  )
}

write_players <- function(players, path = PLAYERS_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(players, path, na = "")
  invisible(players)
}

# Every player listed in a parsed box score (including DNPs), tagged with
# their team and the game date.
extract_players <- function(box) {
  dplyr::bind_rows(lapply(list(box$visitor, box$home), function(team) {
    rows <- team$basic[!team$basic$is_total, ]
    tibble::tibble(
      player_id = rows$player_id,
      player_name = rows$player,
      team = team$abbr,
      last_seen = box$game_date
    )
  }))
}

# Players from the season totals page. Players traded mid-season get a
# combined row ("2TM", "3TM", ...) followed by one row per team in the order
# they played, so the last team row is their current team.
parse_season_players <- function(html) {
  doc <- read_bref_html(html)
  table <- find_bref_table(doc, "totals_stats")
  if (is.null(table)) stop("Table 'totals_stats' not found on season totals page")

  rows <- rvest::html_elements(table, "tbody tr:not(.thead)")
  name_cells <- rvest::html_element(rows, "td[data-stat='name_display']")
  team_cells <- rvest::html_element(rows, "td[data-stat='team_name_abbr']")
  rows <- tibble::tibble(
    player_id = rvest::html_attr(name_cells, "data-append-csv"),
    player_name = trimws(rvest::html_text(name_cells)),
    team = trimws(rvest::html_text(team_cells))
  )

  rows |>
    dplyr::filter(!is.na(player_id), !grepl("^[0-9]+TM$", team)) |>
    dplyr::group_by(player_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(last_seen = as.Date(NA))
}

# Adds new players and updates name/team for known ones. Newer information
# wins: rows in `new` replace matching ids in `cache`, except that a missing
# last_seen in `new` (e.g. from seeding) never erases a known date.
merge_players <- function(cache, new) {
  new <- new |>
    dplyr::distinct(player_id, .keep_all = TRUE) |>
    dplyr::mutate(player_url = player_url(player_id))

  previous <- dplyr::select(cache, "player_id", previous_seen = "last_seen")
  new <- new |>
    dplyr::left_join(previous, by = "player_id") |>
    dplyr::mutate(last_seen = dplyr::coalesce(last_seen, previous_seen)) |>
    dplyr::select(-"previous_seen")

  dplyr::bind_rows(
    dplyr::anti_join(cache, new, by = "player_id"),
    new
  ) |>
    dplyr::select(dplyr::all_of(names(empty_players()))) |>
    dplyr::arrange(player_id)
}

# Merges new players into the cache file. Returns the ids that were not in
# the cache before, for logging.
update_players <- function(new, path = PLAYERS_PATH) {
  cache <- read_players(path)
  added <- setdiff(new$player_id, cache$player_id)
  merged <- merge_players(cache, new)
  if (!identical(merged, cache)) write_players(merged, path)
  invisible(added)
}

seed_players <- function(season, path = PLAYERS_PATH) {
  players <- parse_season_players(bref_get(sprintf("/leagues/NBA_%d_totals.html", season)))
  added <- update_players(players, path)
  log_info("Seeded ", nrow(players), " players (", length(added), " new) into ", path)
  invisible(added)
}

if (sys.nframe() == 0L) {
  source("R/utils.R")
  args <- commandArgs(trailingOnly = TRUE)
  season <- if (length(args) > 0) as.integer(args[[1]]) else season_for_date(today_et())
  seed_players(season)
}
