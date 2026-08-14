if (!exists("new_exercise", mode = "function", inherits = TRUE)) {
  rdir <- normalizePath(file.path(testthat::test_path(), "..", "..", "R"))
  for (f in sort(list.files(rdir, pattern = "[.]R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv(), keep.source = TRUE)
  }
}
