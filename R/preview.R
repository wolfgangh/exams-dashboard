# Render HTML preview and Moodle / PDF exports via exams.

ensure_pandoc <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
    return(invisible(TRUE))
  }
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/x64",
    "C:/Program Files/RStudio/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc",
    "C:/Program Files/Pandoc"
  )
  candidates <- candidates[nzchar(candidates)]
  for (d in candidates) {
    exe <- if (grepl("pandoc\\.exe$", d, ignore.case = TRUE)) d else file.path(d, "pandoc.exe")
    if (file.exists(exe) && requireNamespace("rmarkdown", quietly = TRUE)) {
      rmarkdown::find_pandoc(dir = dirname(exe))
      if (rmarkdown::pandoc_available()) return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

preview_exercise <- function(ex, seed = 1L, dir = tempfile("examsstudio-preview-")) {
  ensure_pandoc()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  slug <- file_slug(ex$meta$name)
  rmd_path <- file.path(dir, paste0(slug, ".Rmd"))
  write_exercise(ex, rmd_path)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  set.seed(as.integer(seed))
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  exams::exams2html(
    file = basename(rmd_path),
    n = 1L,
    dir = dir,
    edir = dir,
    name = "preview",
    mathjax = TRUE,
    quiet = TRUE
  )
  html <- list.files(dir, pattern = "preview.*\\.html$", full.names = TRUE)
  if (!length(html)) {
    html <- list.files(dir, pattern = "\\.html$", full.names = TRUE)
  }
  if (!length(html)) stop("HTML-Vorschau konnte nicht erzeugt werden.", call. = FALSE)
  list(html = html[[1]], dir = dir, rmd = rmd_path)
}

export_moodle <- function(ex, dir, n = 1L, name = NULL) {
  ensure_pandoc()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  slug <- file_slug(name %||% ex$meta$name)
  rmd_path <- file.path(dir, paste0(slug, ".Rmd"))
  write_exercise(ex, rmd_path)
  exams::exams2moodle(
    file = basename(rmd_path),
    n = as.integer(n),
    dir = dir,
    edir = dir,
    name = slug,
    quiet = TRUE
  )
  zips <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)
  xmls <- list.files(dir, pattern = "\\.xml$", full.names = TRUE)
  list(zip = if (length(zips)) zips[[1]] else NA_character_,
       xml = if (length(xmls)) xmls[[1]] else NA_character_,
       rmd = rmd_path)
}

export_pdf <- function(ex, dir, n = 1L, name = NULL) {
  ensure_pandoc()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  slug <- file_slug(name %||% ex$meta$name)
  rmd_path <- file.path(dir, paste0(slug, ".Rmd"))
  write_exercise(ex, rmd_path)
  exams::exams2pdf(
    file = basename(rmd_path),
    n = as.integer(n),
    dir = dir,
    edir = dir,
    name = slug,
    quiet = TRUE
  )
  pdfs <- list.files(dir, pattern = "\\.pdf$", full.names = TRUE)
  if (!length(pdfs)) stop("PDF konnte nicht erzeugt werden (LaTeX/tinytex pruefen).", call. = FALSE)
  list(pdf = pdfs[[1]], rmd = rmd_path)
}
