# Builds the nightly box score email and sends it through Gmail SMTP.
#
# The email is one HTML document with a section per game, in the order the
# games are passed in. A full night runs well past Gmail's ~102 KB display
# limit, so Gmail shows "[Message clipped] View entire message" and the rest
# is one tap away. To fit as much as possible before that point, styling lives
# in a single <style> block (Gmail supports element and class selectors)
# rather than inline on every cell.
#
# Sending needs these environment variables (GitHub secrets in Actions, or a
# git-ignored .Renviron file locally):
#   GMAIL_USER          the Gmail address to send from
#   GMAIL_APP_PASSWORD  a Google app password (not the account password)
#   EMAIL_TO            optional; defaults to GMAIL_USER

EMAIL_CSS <- paste(
  "body{font-family:Arial,Helvetica,sans-serif;color:#222;margin:0;padding:8px}",
  "h1{font-size:18px;margin:0 0 12px}",
  "h2{font-size:16px;margin:24px 0 2px;padding-top:12px;border-top:2px solid #222}",
  "h3{font-size:13px;margin:12px 0 4px}",
  "p{font-size:12px;color:#666;margin:0 0 8px}",
  "a{color:#1a5fb4;text-decoration:none}",
  "div{overflow-x:auto}",
  "table{border-collapse:collapse;font-size:12px;margin:0 0 8px}",
  "th{background:#eee;padding:2px 5px;text-align:right;white-space:nowrap}",
  "td{padding:2px 5px;text-align:right;border-bottom:1px solid #eee;white-space:nowrap}",
  ".l{text-align:left}",
  ".r td{background:#f7f7f7;font-size:11px;color:#666}",
  ".t td{font-weight:bold;border-top:1px solid #999}",
  ".d{color:#999;text-align:left}",
  sep = "\n"
)

# Display headers for Basketball Reference data-stat names. Anything not
# listed falls back to its data-stat name.
STAT_LABELS <- c(
  mp = "MP", fg = "FG", fga = "FGA", fg_pct = "FG%", fg3 = "3P", fg3a = "3PA",
  fg3_pct = "3P%", ft = "FT", fta = "FTA", ft_pct = "FT%", orb = "ORB",
  drb = "DRB", trb = "TRB", ast = "AST", stl = "STL", blk = "BLK", tov = "TOV",
  pf = "PF", pts = "PTS", game_score = "GmSc", plus_minus = "+/-",
  pace = "Pace", efg_pct = "eFG%", tov_pct = "TOV%", orb_pct = "ORB%",
  ft_rate = "FT/FGA", off_rtg = "ORtg", `1OT` = "OT"
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

stat_label <- function(stat) {
  label <- STAT_LABELS[stat]
  ifelse(is.na(label), stat, label)
}

# Line score and four factors: a team column followed by stat columns.
render_team_table <- function(df) {
  stats <- setdiff(names(df), "team")
  header <- paste0("<tr><th></th>", paste0("<th>", html_escape(stat_label(stats)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    values <- html_escape(unlist(df[i, stats], use.names = FALSE))
    paste0("<tr><td class=\"l\">", html_escape(df$team[[i]]), "</td>", paste0("<td>", values, "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0("<div><table>", header, paste(rows, collapse = ""), "</table></div>")
}

# Player box score table, with each name linked to the player's Basketball
# Reference page.
render_player_table <- function(df) {
  stats <- setdiff(names(df), c("player", "player_id", "starter", "is_total", "reason"))
  header <- paste0("<tr><th class=\"l\">Starters</th>", paste0("<th>", html_escape(stat_label(stats)), "</th>", collapse = ""), "</tr>")
  reserves_row <- sprintf("<tr class=\"r\"><td class=\"l\" colspan=\"%d\">Reserves</td></tr>", length(stats) + 1)

  rows <- character()
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    if (isFALSE(row$starter) && (i == 1 || isTRUE(df$starter[[i - 1]]))) rows <- c(rows, reserves_row)

    name <- html_escape(row$player)
    if (!is.na(row$player_id)) {
      name <- sprintf("<a href=\"%s\">%s</a>", player_url(row$player_id), name)
    }
    name_cell <- paste0("<td class=\"l\">", name, "</td>")

    if (!is.na(row$reason)) {
      rows <- c(rows, sprintf("<tr>%s<td class=\"d\" colspan=\"%d\">%s</td></tr>", name_cell, length(stats), html_escape(row$reason)))
      next
    }

    values <- unlist(row[stats], use.names = FALSE)
    values[is.na(values)] <- ""
    cells <- paste0(name_cell, paste0("<td>", html_escape(values), "</td>", collapse = ""))
    rows <- c(rows, if (isTRUE(row$is_total)) paste0("<tr class=\"t\">", cells, "</tr>") else paste0("<tr>", cells, "</tr>"))
  }

  paste0("<div><table>", header, paste(rows, collapse = ""), "</table></div>")
}

# "OT", "2OT", ... from the line score's overtime columns, or "" in regulation.
overtime_label <- function(line_score) {
  n_ot <- sum(grepl("OT$", names(line_score)))
  if (n_ot == 0) "" else if (n_ot == 1) "OT" else paste0(n_ot, "OT")
}

render_game <- function(box) {
  ot <- overtime_label(box$line_score)
  title <- sprintf(
    "%s %s @ %s %s%s",
    html_escape(box$visitor$name), html_escape(box$visitor$pts),
    html_escape(box$home$name), html_escape(box$home$pts),
    if (nzchar(ot)) paste0(" (", ot, ")") else ""
  )
  # "9:00 PM, October 24, 2026" -> "9:00 PM"; the date is in the subject.
  start_time <- sub(",.*$", "", box$start_time)
  meta <- paste(html_escape(c(start_time, box$arena)), collapse = " &middot; ")

  team_section <- function(team) {
    paste0(
      "<h3>", html_escape(team$name), " (", html_escape(team$record), ")</h3>",
      render_player_table(team$basic)
    )
  }

  paste0(
    "<h2>", title, "</h2>",
    "<p>", meta, "</p>",
    render_team_table(box$line_score),
    render_team_table(box$four_factors),
    team_section(box$visitor),
    team_section(box$home)
  )
}

email_subject <- function(boxes) {
  n_games <- length(boxes)
  sprintf("NBA box scores: %s (%d %s)", format(email_date(boxes), "%a %b %d"), n_games, if (n_games == 1) "game" else "games")
}

# The email's date comes from the games themselves, and every game must be
# from the same night, so a heading can never misdate a game.
email_date <- function(boxes) {
  dates <- unique(do.call(c, lapply(boxes, `[[`, "game_date")))
  if (length(dates) != 1) {
    stop("All games in one email must be from the same date; got ", paste(format(dates), collapse = ", "))
  }
  dates
}

build_email_html <- function(boxes) {
  heading <- format(email_date(boxes), "%A, %B %d, %Y")
  paste0(
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<style>", EMAIL_CSS, "</style></head><body>",
    "<h1>NBA box scores &middot; ", html_escape(heading), "</h1>",
    paste(vapply(boxes, render_game, character(1)), collapse = ""),
    "</body></html>"
  )
}

# A complete MIME message. Subject and body are base64-encoded so player names
# with accents (Jokić, Dončić) survive any mail server.
build_mime_message <- function(html, subject, from, to) {
  encode <- function(x) openssl::base64_encode(charToRaw(enc2utf8(x)))
  date <- withr::with_locale(c(LC_TIME = "C"), format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z"))
  body <- openssl::base64_encode(charToRaw(enc2utf8(html)), linebreaks = TRUE)

  paste(
    sprintf("From: NBA Keeping Score <%s>", from),
    sprintf("To: %s", to),
    sprintf("Subject: =?UTF-8?B?%s?=", encode(subject)),
    sprintf("Date: %s", date),
    "MIME-Version: 1.0",
    "Content-Type: text/html; charset=UTF-8",
    "Content-Transfer-Encoding: base64",
    "",
    gsub("\n", "\r\n", trimws(body), fixed = TRUE),
    sep = "\r\n"
  )
}

send_email <- function(html, subject,
                       from = Sys.getenv("GMAIL_USER"),
                       password = Sys.getenv("GMAIL_APP_PASSWORD"),
                       to = Sys.getenv("EMAIL_TO", from)) {
  if (!nzchar(from) || !nzchar(password)) {
    stop("GMAIL_USER and GMAIL_APP_PASSWORD must be set to send email")
  }
  if (!nzchar(to)) to <- from

  message <- build_mime_message(html, subject, from, to)
  log_info("Sending '", subject, "' (", round(nchar(html, "bytes") / 1024), " KB)")
  curl::send_mail(
    mail_from = from,
    mail_rcpt = to,
    message = message,
    smtp_server = "smtps://smtp.gmail.com:465",
    use_ssl = "force",
    verbose = FALSE,
    username = from,
    password = password
  )
  invisible(TRUE)
}

# Writes the email HTML to a file so it can be opened in a browser; the daily
# workflow also uploads it for debugging.
write_email_preview <- function(html, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, path, useBytes = TRUE)
  invisible(path)
}
