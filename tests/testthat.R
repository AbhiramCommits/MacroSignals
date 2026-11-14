library(testthat)

root <- normalizePath(".")
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(file.path(root, "..", ".."))
}

for (f in list.files(file.path(root, "R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

test_dir(file.path(root, "tests", "testthat"), reporter = "summary")
