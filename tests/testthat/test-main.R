# run_daily() is exercised offline: the schedule comes from the saved October
# page, and box scores come from fixtures instead of the network. Of the 8
# games on 2026-10-24, fixtures exist for WAS @ CHI and NOP @ DEN.
fixture_fetch <- function(game_id) {
  files <- list.files(testthat::test_path("fixtures"), pattern = paste0("^boxscore-", game_id))
  if (length(files) == 0) stop("HTTP 404 Not Found")
  read_fixture(files[[1]])
}

local_pipeline <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  schedule_path <- file.path(dir, "schedule.csv")
  readr::write_csv(parse_schedule_page(read_fixture("schedule-2027-october.html.gz")), schedule_path, na = "")
  list(
    schedule_path = schedule_path,
    players_path = file.path(dir, "players.csv"),
    output_dir = file.path(dir, "output")
  )
}

run_offline <- function(paths, date) {
  run_daily(
    date, send = FALSE, refresh_schedule = FALSE, fetch = fixture_fetch,
    schedule_path = paths$schedule_path, players_path = paths$players_path, output_dir = paths$output_dir
  )
}

test_that("parse_args handles defaults, --date, and --dry-run", {
  expect_equal(parse_args(character())$date, yesterday_et())
  expect_false(parse_args(character())$dry_run)

  opts <- parse_args(c("--date", "2026-10-24", "--dry-run"))
  expect_equal(opts$date, as.Date("2026-10-24"))
  expect_true(opts$dry_run)

  expect_error(parse_args(c("--date", "10/24/2026")), "YYYY-MM-DD")
  expect_error(parse_args("--send-to-everyone"), "Unknown argument")
})

test_that("a night with some failed games emails the rest with a note", {
  paths <- local_pipeline()
  result <- run_offline(paths, "2026-10-24")

  expect_equal(result$status, "partial")
  expect_equal(result$games, 2)
  expect_length(result$failures, 6)
  expect_equal(result$subject, "NBA box scores: Sat Oct 24 (2 games)")

  html <- paste(readLines(result$preview_path, encoding = "UTF-8"), collapse = "\n")
  expect_equal(basename(result$preview_path), "email-2026-10-24.html")
  expect_match(html, "Could not load 6 games", fixed = TRUE)
  expect_match(html, "HTTP 404 Not Found", fixed = TRUE)
  # Tip-off order: WAS @ CHI (8:00p) before NOP @ DEN (9:00p).
  expect_lt(regexpr("Washington Wizards", html, fixed = TRUE), regexpr("New Orleans Pelicans", html, fixed = TRUE))
})

test_that("the run records every player in the cache", {
  paths <- local_pipeline()
  run_offline(paths, "2026-10-24")

  players <- read_players(paths$players_path)
  expect_true(all(c("jokicni01", "nembhry01") %in% players$player_id))
  expect_setequal(unique(players$team), c("WAS", "CHI", "NOP", "DEN"))
  expect_true(all(players$last_seen == as.Date("2026-10-24")))
})

test_that("a night without games sends nothing", {
  paths <- local_pipeline()
  result <- run_offline(paths, "2026-11-15")
  expect_equal(result$status, "no_games")
  expect_false(dir.exists(paths$output_dir))
})

test_that("a night where no game loads is an error", {
  paths <- local_pipeline()
  # 11 games on 2026-10-21, none with fixtures.
  expect_error(run_offline(paths, "2026-10-21"), "No box scores could be loaded for 2026-10-21")
  expect_false(file.exists(paths$players_path))
})

test_that("the alert email explains the failure", {
  html <- build_alert_html("2026-10-21", "No box scores could be loaded <for> 2026-10-21")
  expect_match(html, "NBA box scores failed &middot; Wednesday, October 21, 2026", fixed = TRUE)
  expect_match(html, "loaded &lt;for&gt; 2026-10-21", fixed = TRUE)
  expect_equal(alert_subject("2026-10-21"), "NBA box scores FAILED: Wed Oct 21")
})
