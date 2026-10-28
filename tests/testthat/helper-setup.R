project_root <- normalizePath(testthat::test_path("..", ".."))
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  if (basename(file) != "main.R") source(file)
}

# Fixtures are real Basketball Reference pages saved (gzipped) so the parsers
# can be tested without hitting the site.
read_fixture <- function(name) {
  con <- gzfile(testthat::test_path("fixtures", name))
  on.exit(close(con))
  paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
