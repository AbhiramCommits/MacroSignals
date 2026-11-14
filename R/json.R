# Minimal JSON writer. The pipeline only emits a small, known run summary, so a
# dependency-free encoder keeps the stack exactly as specified.

json_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\r", "\\\\r", x)
  x <- gsub("\t", "\\\\t", x)
  x
}

json_scalar <- function(x) {
  if (is.null(x)) return("null")
  if (length(x) != 1) return(json_encode(unname(x)))
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) {
    if (!is.finite(x)) return("null")
    return(format(x, digits = 12, scientific = FALSE, trim = TRUE))
  }
  paste0("\"", json_escape(as.character(x)), "\"")
}

json_encode <- function(x, indent = 0L) {
  pad <- strrep("  ", indent)
  pad2 <- strrep("  ", indent + 1L)
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      as.list(x[i, , drop = FALSE])
    })
    return(json_encode(rows, indent))
  }
  if (is.list(x)) {
    if (!is.null(names(x)) && all(nzchar(names(x)))) {
      if (length(x) == 0) return("{}")
      items <- vapply(seq_along(x), function(i) {
        paste0(pad2, "\"", json_escape(names(x)[[i]]), "\": ",
               json_encode(x[[i]], indent + 1L))
      }, character(1))
      return(paste0("{\n", paste(items, collapse = ",\n"), "\n", pad, "}"))
    }
    if (length(x) == 0) return("[]")
    items <- vapply(x, function(el) {
      paste0(pad2, json_encode(el, indent + 1L))
    }, character(1))
    return(paste0("[\n", paste(items, collapse = ",\n"), "\n", pad, "]"))
  }
  if (length(x) > 1) {
    items <- vapply(x, function(el) json_encode(el, indent + 1L), character(1))
    return(paste0("[\n", paste(paste0(pad2, items), collapse = ",\n"),
                  "\n", pad, "]"))
  }
  json_scalar(x)
}

write_json <- function(x, path) {
  ensure_dir(dirname(path))
  writeLines(json_encode(x), con = path, useBytes = TRUE)
  invisible(path)
}
