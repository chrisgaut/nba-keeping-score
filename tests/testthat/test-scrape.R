regulation <- parse_box_score(read_fixture("boxscore-202610200DET.html.gz"), "202610200DET")
overtime <- parse_box_score(read_fixture("boxscore-202610240DEN-ot.html.gz"), "202610240DEN")

test_that("game metadata and teams come from the scorebox", {
  expect_equal(regulation$game_date, as.Date("2026-10-20"))
  expect_equal(regulation$arena, "Little Caesars Arena, Detroit, Michigan")

  expect_equal(regulation$visitor$abbr, "BOS")
  expect_equal(regulation$visitor$name, "Boston Celtics")
  expect_equal(regulation$visitor$pts, "113")
  expect_equal(regulation$home$abbr, "DET")
  expect_equal(regulation$home$pts, "121")
  expect_equal(overtime$visitor$record, "0-3")
})

test_that("commented-out line score is found and handles overtime", {
  expect_equal(names(regulation$line_score), c("team", "1", "2", "3", "4", "T"))
  expect_equal(regulation$line_score$team, c("BOS", "DET"))
  expect_equal(regulation$line_score$T, c("113", "121"))

  expect_equal(names(overtime$line_score), c("team", "1", "2", "3", "4", "1OT", "T"))
  expect_equal(overtime$line_score$`1OT`, c("9", "15"))
})

test_that("commented-out four factors are parsed", {
  ff <- regulation$four_factors
  expect_equal(names(ff), c("team", "pace", "efg_pct", "tov_pct", "orb_pct", "ft_rate", "off_rtg"))
  expect_equal(ff$team, c("BOS", "DET"))
  expect_equal(ff$off_rtg, c("115.4", "123.5"))
})

test_that("basic box score splits starters, reserves, DNPs, and totals", {
  den <- overtime$home$basic

  expect_equal(sum(den$starter, na.rm = TRUE), 5)
  expect_equal(den$player[1], "Nikola Jokić")
  expect_equal(den$player_id[1], "jokicni01")
  expect_equal(den$mp[1], "43:38")

  dnp <- den[!is.na(den$reason), ]
  expect_equal(dnp$player, c("DaRon Holmes", "Ryan Nembhard", "Zeke Nnaji"))
  expect_true(all(is.na(dnp$pts)))

  totals <- den[den$is_total, ]
  expect_equal(nrow(totals), 1)
  expect_equal(totals$player, "Team Totals")
  expect_equal(totals$pts, overtime$home$pts)

  # Every player row (not totals) carries an id for hyperlinking.
  expect_false(anyNA(den$player_id[!den$is_total]))
  expect_false("Reserves" %in% den$player)
})

test_that("advanced box score has the advanced columns", {
  adv <- regulation$home$advanced
  expect_true(all(c("ts_pct", "usg_pct", "off_rtg", "def_rtg", "bpm") %in% names(adv)))
  expect_equal(adv$player_id[1], "cunnica01")
  expect_equal(adv$ts_pct[1], ".545")
  expect_equal(sum(adv$is_total), 1)
})

test_that("a missing table raises a clear error", {
  html <- "<html><body><table id='other'></table></body></html>"
  expect_error(parse_box_score(html, "202610200DET"), "line_score")
})
