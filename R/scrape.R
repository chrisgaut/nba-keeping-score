# Fetches and parses Basketball Reference box score pages.
#
# parse_box_score() returns a list:
#   game_id, game_date, start_time, arena
#   visitor, home     each a list(abbr, name, pts, record, basic, advanced)
#   line_score        one row per team: team, 1, 2, 3, 4, [1OT, 2OT, ...], T
#   four_factors      one row per team: team, pace, efg_pct, tov_pct, ...
#
# Stat values are kept as the text Basketball Reference displays (".522",
# "43:38", ...) because they only get rendered into the email. Column names are
# the site's data-stat names (fg, fg3_pct, usg_pct, ...).

fetch_box_score <- function(game_id) {
  bref_get(sprintf("/boxscores/%s.html", game_id))
}

parse_box_score <- function(html, game_id) {
  doc <- read_bref_html(html)

  line_score <- parse_team_table(require_table(doc, "line_score", game_id))
  four_factors <- parse_team_table(require_table(doc, "four_factors", game_id)) |>
    dplyr::rename(team = "team_id")

  # Line score rows are visitor first, home second, matching the scorebox.
  abbrs <- line_score$team
  if (length(abbrs) != 2) {
    stop("Expected 2 teams in line score for ", game_id, ", found ", length(abbrs))
  }
  scorebox <- parse_scorebox(doc$page)

  team <- function(i) {
    abbr <- abbrs[[i]]
    list(
      abbr = abbr,
      name = scorebox$name[[i]],
      pts = scorebox$pts[[i]],
      record = scorebox$record[[i]],
      basic = parse_player_table(require_table(doc, sprintf("box-%s-game-basic", abbr), game_id)),
      advanced = parse_player_table(require_table(doc, sprintf("box-%s-game-advanced", abbr), game_id))
    )
  }

  meta <- parse_scorebox_meta(doc$page)

  list(
    game_id = game_id,
    game_date = as.Date(substr(game_id, 1, 8), format = "%Y%m%d"),
    start_time = meta$start_time,
    arena = meta$arena,
    visitor = team(1),
    home = team(2),
    line_score = line_score,
    four_factors = four_factors
  )
}

require_table <- function(doc, id, game_id) {
  table <- find_bref_table(doc, id)
  if (is.null(table)) stop("Table '", id, "' not found on box score ", game_id)
  table
}

# Text of each th/td in a row, named by data-stat.
row_cells <- function(row) {
  cells <- rvest::html_elements(row, "th, td")
  stats::setNames(as.list(trimws(rvest::html_text(cells))), rvest::html_attr(cells, "data-stat"))
}

# Small team-level tables (line score, four factors): one row per team.
parse_team_table <- function(table) {
  rows <- rvest::html_elements(table, "tbody tr:not(.thead)")
  dplyr::bind_rows(lapply(rows, row_cells))
}

# Player box score tables (basic and advanced). Starters come before the
# "Reserves" header row; DNP-style rows have a single "reason" cell; team
# totals live in <tfoot>.
parse_player_table <- function(table) {
  rows <- rvest::html_elements(table, "tbody tr")
  stat_names <- rvest::html_attr(rvest::html_elements(table, "thead tr:last-child th"), "data-stat")

  starter <- TRUE
  parsed <- list()
  for (row in rows) {
    if (grepl("thead", rvest::html_attr(row, "class") %||% "")) {
      starter <- FALSE
      next
    }
    th <- rvest::html_element(row, "th")
    cells <- row_cells(row)
    parsed[[length(parsed) + 1]] <- c(
      list(
        player_id = rvest::html_attr(th, "data-append-csv"),
        starter = starter,
        reason = cells$reason %||% NA_character_
      ),
      cells[setdiff(names(cells), "reason")]
    )
  }

  totals_row <- rvest::html_element(table, "tfoot tr")
  totals <- if (inherits(totals_row, "xml_missing")) {
    list()
  } else {
    list(c(list(player_id = NA_character_, starter = NA, reason = NA_character_), row_cells(totals_row)))
  }

  out <- dplyr::bind_rows(c(parsed, totals))
  out$is_total <- c(rep(FALSE, length(parsed)), rep(TRUE, length(totals)))

  # Fixed column order: identifiers first, then stats in the site's order.
  # DNP rows have NA for every stat.
  for (col in setdiff(stat_names, names(out))) out[[col]] <- NA_character_
  dplyr::relocate(out, "player", "player_id", "starter", "is_total", "reason", dplyr::all_of(setdiff(stat_names, "player")))
}

parse_scorebox <- function(page) {
  teams <- rvest::html_elements(page, "div.scorebox > div.scorebox_team")
  list(
    name = trimws(rvest::html_text(rvest::html_element(teams, "strong a"))),
    pts = trimws(rvest::html_text(rvest::html_element(teams, "div.score"))),
    # The win-loss record is the bare <div> right after the score.
    record = trimws(rvest::html_text(rvest::html_element(teams, xpath = "./div[@class='scores']/following-sibling::div[1]")))
  )
}

parse_scorebox_meta <- function(page) {
  lines <- trimws(rvest::html_text(rvest::html_elements(page, "div.scorebox_meta > div")))
  list(
    start_time = lines[1] %||% NA_character_,
    arena = lines[2] %||% NA_character_
  )
}
