# Daily entry point: emails the box scores of one night's games.
#
# Usage (from the project root):
#   Rscript R/main.R                         # last night (Eastern time)
#   Rscript R/main.R --date 2026-10-24       # a specific night
#   Rscript R/main.R --date 2026-10-24 --dry-run   # build the email, don't send
#
# Steps: refresh this month's schedule -> find the night's games -> fetch and
# parse each box score -> update data/players.csv -> build the email -> save a
# copy to output/ -> send. A game that fails to load is noted in the email;
# if no game loads (or anything else fails), an alert email is sent instead and
# the script exits with an error so the GitHub Actions run shows as failed.

parse_args <- function(args) {
  opts <- list(date = yesterday_et(), dry_run = FALSE)
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--dry-run") {
      opts$dry_run <- TRUE
    } else if (arg == "--date" && i < length(args)) {
      i <- i + 1
      opts$date <- as.Date(args[[i]], optional = TRUE)
      if (is.na(opts$date)) stop("--date must be YYYY-MM-DD, got '", args[[i]], "'")
    } else {
      stop("Unknown argument '", arg, "'. Usage: Rscript R/main.R [--date YYYY-MM-DD] [--dry-run]")
    }
    i <- i + 1
  }
  opts
}

# Fetches and parses every game, collecting failures instead of stopping, so
# one bad page doesn't cost the rest of the night's box scores.
load_box_scores <- function(games, fetch = fetch_box_score) {
  boxes <- list()
  failures <- character()
  for (i in seq_len(nrow(games))) {
    game <- games[i, ]
    label <- sprintf("%s @ %s", game$visitor_abbr, game$home_abbr)
    result <- tryCatch(
      parse_box_score(fetch(game$game_id), game$game_id),
      error = function(e) {
        log_info("Could not load ", label, " (", game$game_id, "): ", conditionMessage(e))
        failures[[length(failures) + 1]] <<- sprintf("%s: %s", label, conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) boxes[[length(boxes) + 1]] <- result
  }
  list(boxes = boxes, failures = failures)
}

run_daily <- function(date = yesterday_et(),
                      send = TRUE,
                      refresh_schedule = TRUE,
                      fetch = fetch_box_score,
                      schedule_path = SCHEDULE_PATH,
                      players_path = PLAYERS_PATH,
                      output_dir = "output") {
  date <- as.Date(date)
  log_info("Box scores for ", format(date, "%A, %B %d, %Y"))

  if (refresh_schedule) {
    # A failed refresh isn't fatal: the cached schedule is usually right.
    tryCatch(
      refresh_schedule_month(date, schedule_path),
      error = function(e) log_info("Schedule refresh failed, using cached schedule: ", conditionMessage(e))
    )
  }

  games <- games_on(read_schedule(schedule_path), date)
  games <- games[order(start_minutes(games$start_time_et), games$game_id), ]
  if (nrow(games) == 0) {
    log_info("No games on ", format(date), "; nothing to send")
    return(invisible(list(status = "no_games")))
  }
  log_info(nrow(games), " games scheduled")

  loaded <- load_box_scores(games, fetch)
  if (length(loaded$boxes) == 0) {
    stop("No box scores could be loaded for ", format(date), ":\n", paste(loaded$failures, collapse = "\n"))
  }

  added <- update_players(dplyr::bind_rows(lapply(loaded$boxes, extract_players)), players_path)
  if (length(added) > 0) log_info("Added ", length(added), " new players: ", paste(added, collapse = ", "))

  html <- build_email_html(loaded$boxes, loaded$failures)
  subject <- email_subject(loaded$boxes)
  preview_path <- write_email_preview(html, file.path(output_dir, sprintf("email-%s.html", format(date))))
  log_info("Saved email to ", preview_path)

  if (send) send_email(html, subject) else log_info("Dry run; not sending '", subject, "'")

  invisible(list(
    status = if (length(loaded$failures) > 0) "partial" else "ok",
    subject = subject,
    games = length(loaded$boxes),
    failures = loaded$failures,
    preview_path = preview_path
  ))
}

if (sys.nframe() == 0L) {
  for (file in c("utils", "schedule", "scrape", "players", "email")) {
    source(file.path("R", paste0(file, ".R")))
  }
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  tryCatch(
    run_daily(opts$date, send = !opts$dry_run),
    error = function(e) {
      log_info("FAILED: ", conditionMessage(e))
      if (!opts$dry_run) {
        tryCatch(
          send_email(build_alert_html(opts$date, conditionMessage(e)), alert_subject(opts$date)),
          error = function(e2) log_info("Alert email also failed: ", conditionMessage(e2))
        )
      }
      quit(status = 1)
    }
  )
}
