# Two games from the same night (2026-10-24), in start-time order.
regulation <- parse_box_score(read_fixture("boxscore-202610240CHI.html.gz"), "202610240CHI")
overtime <- parse_box_score(read_fixture("boxscore-202610240DEN-ot.html.gz"), "202610240DEN")
html <- build_email_html(list(regulation, overtime))

count_matches <- function(pattern, x) {
  lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))
}

test_that("html_escape escapes markup characters", {
  expect_equal(html_escape("A & B <i>\"x\"</i>"), "A &amp; B &lt;i&gt;&quot;x&quot;&lt;/i&gt;")
})

test_that("the email is dated from its games", {
  expect_equal(email_date(list(regulation, overtime)), as.Date("2026-10-24"))
  expect_match(html, "<h1>NBA box scores &middot; Saturday, October 24, 2026</h1>", fixed = TRUE)
})

test_that("games from different dates cannot share an email", {
  other_night <- parse_box_score(read_fixture("boxscore-202610200DET.html.gz"), "202610200DET")
  expect_error(build_email_html(list(other_night, overtime)), "same date.*2026-10-20, 2026-10-24")
  expect_error(email_subject(list(other_night, overtime)), "same date")
})

test_that("game headings show teams, scores, overtime, time, and arena", {
  expect_match(html, "<h2>Washington Wizards 124 @ Chicago Bulls 120</h2>", fixed = TRUE)
  expect_match(html, "<h2>New Orleans Pelicans 135 @ Denver Nuggets 141 (OT)</h2>", fixed = TRUE)
  expect_match(html, "<p>9:00 PM &middot; Ball Arena, Denver, Colorado</p>", fixed = TRUE)
})

test_that("game headings do not link to the box score page", {
  expect_false(grepl("/boxscores/", html, fixed = TRUE))
  expect_false(grepl(">Box score<", html, fixed = TRUE))
})

test_that("games appear in the order given, with no summary before them", {
  first_game <- regexpr("<h2>", html, fixed = TRUE)
  expect_lt(regexpr("Washington Wizards 124", html, fixed = TRUE), regexpr("New Orleans Pelicans 135", html, fixed = TRUE))
  expect_false(grepl("<table", substr(html, 1, first_game), fixed = TRUE))
})

test_that("overtime_label counts overtime periods", {
  expect_equal(overtime_label(regulation$line_score), "")
  expect_equal(overtime_label(overtime$line_score), "OT")
  expect_equal(overtime_label(tibble::tibble(team = "A", `1OT` = "1", `2OT` = "2", T = "3")), "2OT")
})

test_that("each game renders line score, four factors, and one box score per team", {
  game <- render_game(regulation)
  expect_equal(count_matches("<table>", game), 4)
  expect_match(game, "<th>FT/FGA</th>", fixed = TRUE)
  expect_match(game, "<th>+/-</th>", fixed = TRUE)
  expect_false(grepl("USG%", game, fixed = TRUE))
  expect_match(render_game(overtime), "<th>OT</th>", fixed = TRUE)
})

test_that("player names link to their Basketball Reference pages", {
  link <- sprintf("<a href=\"%s\">Nikola Jokić</a>", player_url("jokicni01"))
  expect_equal(count_matches(link, html), 1)
  expect_equal(count_matches(">Nikola Jokić<", html), 1)
  expect_false(grepl("<a href=\"[^\"]*\">Team Totals", html))
})

test_that("player tables mark reserves, DNPs, and totals", {
  table <- render_player_table(overtime$home$basic)
  expect_equal(count_matches("<tr class=\"r\">", table), 1)
  expect_equal(count_matches("<td class=\"d\" colspan=\"21\">Did Not Play</td>", table), 3)
  expect_equal(count_matches("<tr class=\"t\">", table), 1)
})

test_that("subject names the date and game count", {
  expect_equal(email_subject(list(regulation, overtime)), "NBA box scores: Sat Oct 24 (2 games)")
  expect_equal(email_subject(list(overtime)), "NBA box scores: Sat Oct 24 (1 game)")
})

test_that("MIME message encodes subject and body as UTF-8 base64", {
  message <- build_mime_message(html, "Jokić night", "me@example.com", "me@example.com")
  parts <- strsplit(message, "\r\n\r\n", fixed = TRUE)[[1]]
  headers <- parts[1]

  expect_match(headers, "From: NBA Keeping Score <me@example.com>", fixed = TRUE)
  expect_match(headers, "Content-Type: text/html; charset=UTF-8", fixed = TRUE)
  subject <- sub(".*Subject: =\\?UTF-8\\?B\\?([^?]+)\\?=.*", "\\1", headers)
  expect_equal(rawToChar(openssl::base64_decode(subject)), enc2native("Jokić night"))

  body <- rawToChar(openssl::base64_decode(gsub("\r\n", "", parts[2], fixed = TRUE)))
  Encoding(body) <- "UTF-8"
  expect_equal(body, html)
})

test_that("send_email refuses to run without credentials", {
  withr::local_envvar(GMAIL_USER = "", GMAIL_APP_PASSWORD = "")
  expect_error(send_email(html, "subject"), "GMAIL_USER and GMAIL_APP_PASSWORD")
})
