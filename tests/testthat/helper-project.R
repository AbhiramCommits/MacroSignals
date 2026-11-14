# Test helpers: locate the project, source the R implementation, build a
# throwaway vintage store from committed fixtures.

ms_project_root <- function() {
  p <- normalizePath(getwd(), mustWork = TRUE)
  if (file.exists(file.path(p, "DESCRIPTION"))) return(p)
  normalizePath(file.path(p, "..", ".."), mustWork = TRUE)
}

MS_ROOT <- ms_project_root()

for (f in list.files(file.path(MS_ROOT, "R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

ms_fixtures <- function() file.path(MS_ROOT, "inst", "fixtures")

ms_new_db <- function(fixtures = ms_fixtures()) {
  path <- tempfile(fileext = ".sqlite")
  build_fixture_db(path, fixtures, file.path(MS_ROOT, "sql", "schema.sql"))
  con <- db_connect(path)
  list(con = con, path = path)
}

ms_close_db <- function(handle) {
  try(DBI::dbDisconnect(handle$con), silent = TRUE)
  unlink(handle$path)
  invisible(NULL)
}
