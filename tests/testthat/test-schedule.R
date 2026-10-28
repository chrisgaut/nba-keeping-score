schedule_html <- read_fixture("schedule-2027-october.html.gz")

test_that("month links are discovered in season order", {
  paths <- schedule_month_paths(schedule_html, 2027)
  expect_equal(paths[1], "/leagues/NBA_2027_games-october.html")
  expect_true("/leagues/NBA_2027_games-april.html" %in% paths)
  expect_false(anyDuplicated(paths) > 0)
})

test_that("schedule rows parse for played and upcoming games", {
  schedule <- parse_schedule_page(schedule_html)

  expect_equal(nrow(schedule), 88)
  expect_false(anyNA(schedule$game_id))
  expect_false(anyDuplicated(schedule$game_id) > 0)

  opener <- schedule[schedule$game_id == "202610200DET", ]
  expect_equal(opener$game_date, as.Date("2026-10-20"))
  expect_equal(opener$visitor_abbr, "BOS")
  expect_equal(opener$home_abbr, "DET")
  expect_equal(opener$home_name, "Detroit Pistons")
  expect_equal(opener$status, "final")
  expect_equal(opener$box_url, "https://www.basketball-reference.com/boxscores/202610200DET.html")

  upcoming <- schedule[schedule$game_id == "202610310HOU", ]
  expect_equal(upcoming$status, "scheduled")
  expect_equal(upcoming$visitor_abbr, "OKC")
})

test_that("games_on filters to a single date", {
  schedule <- parse_schedule_page(schedule_html)
  expect_equal(nrow(games_on(schedule, "2026-10-20")), 3)
  expect_equal(nrow(games_on(schedule, "2026-11-15")), 0)
})

test_that("schedule round-trips through CSV", {
  path <- withr::local_tempfile(fileext = ".csv")
  schedule <- parse_schedule_page(schedule_html)
  readr::write_csv(schedule, path, na = "")
  back <- read_schedule(path)
  expect_equal(back$game_id, schedule$game_id)
  expect_s3_class(back$game_date, "Date")
})
