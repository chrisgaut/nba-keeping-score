box <- parse_box_score(read_fixture("boxscore-202610240DEN-ot.html.gz"), "202610240DEN")

test_that("player_url builds Basketball Reference player links", {
  expect_equal(player_url("jokicni01"), "https://www.basketball-reference.com/players/j/jokicni01.html")
  expect_equal(player_url(c("joeis01", "murraja01")), c(
    "https://www.basketball-reference.com/players/j/joeis01.html",
    "https://www.basketball-reference.com/players/m/murraja01.html"
  ))
})

test_that("extract_players lists every player in a box score, DNPs included", {
  players <- extract_players(box)

  expect_equal(nrow(players), sum(!box$visitor$basic$is_total) + sum(!box$home$basic$is_total))
  expect_false(anyNA(players$player_id))
  expect_setequal(unique(players$team), c("NOP", "DEN"))
  expect_true(all(players$last_seen == as.Date("2026-10-24")))
  expect_true("nembhry01" %in% players$player_id) # Did Not Play
  expect_false("Team Totals" %in% players$player_name)
})

test_that("parse_season_players reads the season totals page", {
  players <- parse_season_players(read_fixture("totals-2027.html.gz"))

  expect_equal(nrow(players), 441)
  expect_false(anyNA(players$player_id))
  expect_false(anyDuplicated(players$player_id) > 0)
  expect_equal(players$team[players$player_id == "jokicni01"], "DEN")
  expect_true(all(is.na(players$last_seen)))
})

test_that("traded players keep their latest team, not the combined row", {
  html <- '<table id="totals_stats"><tbody>
    <tr><td data-stat="name_display" data-append-csv="tradepl01">Traded Player</td><td data-stat="team_name_abbr">2TM</td></tr>
    <tr><td data-stat="name_display" data-append-csv="tradepl01">Traded Player</td><td data-stat="team_name_abbr">BOS</td></tr>
    <tr><td data-stat="name_display" data-append-csv="tradepl01">Traded Player</td><td data-stat="team_name_abbr">LAL</td></tr>
    <tr class="norank"><td data-stat="name_display">League Average</td><td data-stat="team_name_abbr"></td></tr>
  </tbody></table>'

  players <- parse_season_players(html)
  expect_equal(nrow(players), 1)
  expect_equal(players$team, "LAL")
})

test_that("merge_players adds new players and updates known ones", {
  cache <- merge_players(empty_players(), tibble::tibble(
    player_id = c("aaaaaa01", "bbbbbb01"),
    player_name = c("Player A", "Player B"),
    team = c("BOS", "NYK"),
    last_seen = as.Date(c("2026-10-20", "2026-10-20"))
  ))

  merged <- merge_players(cache, tibble::tibble(
    player_id = c("bbbbbb01", "cccccc01"),
    player_name = c("Player B", "Player C"),
    team = c("MIA", "DEN"),
    last_seen = as.Date(c("2026-10-24", "2026-10-24"))
  ))

  expect_equal(merged$player_id, c("aaaaaa01", "bbbbbb01", "cccccc01"))
  expect_equal(merged$team, c("BOS", "MIA", "DEN"))
  expect_equal(merged$last_seen, as.Date(c("2026-10-20", "2026-10-24", "2026-10-24")))
  expect_equal(merged$player_url[3], player_url("cccccc01"))
})

test_that("seeding never erases a known last_seen date", {
  cache <- merge_players(empty_players(), extract_players(box))
  reseeded <- merge_players(cache, tibble::tibble(
    player_id = "jokicni01", player_name = "Nikola Jokić", team = "DEN", last_seen = as.Date(NA)
  ))
  expect_equal(reseeded$last_seen[reseeded$player_id == "jokicni01"], as.Date("2026-10-24"))
})

test_that("update_players writes the cache file and reports new ids", {
  path <- withr::local_tempfile(fileext = ".csv")

  added <- update_players(extract_players(box), path)
  expect_equal(length(added), nrow(extract_players(box)))
  expect_true(file.exists(path))

  # Running again with the same players adds nothing and keeps the file intact.
  expect_length(update_players(extract_players(box), path), 0)
  back <- read_players(path)
  expect_equal(nrow(back), length(added))
  expect_s3_class(back$last_seen, "Date")
})
