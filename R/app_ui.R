app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B4F72",
    secondary = "#5D6D7E",
    success = "#1E8449",
    warning = "#B9770E",
    danger = "#922B21",
    font_scale = 0.92
  )
}

app_ui <- function() {
  bslib::page_fillable(
    theme = app_theme(),
    padding = 0,
    gap = 0,
    title = "R/exams Studio",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "workspace.css")
    ),
    shiny::div(
      class = "ws-shell",
      shiny::div(
        class = "ws-top",
        shiny::span(class = "ws-brand", "R/exams Studio"),
        shiny::span(
          class = "ws-build",
          title = "Nach Änderungen an R-Dateien die App neu starten — www/ gilt sofort",
          paste0(
            "Build ",
            format(tryCatch(file.info("R/app_server.R")$mtime, error = function(e) Sys.time()), "%H:%M")
          )
        ),
        shiny::div(
          class = "ws-field",
          shiny::textInput("meta_title", "Aufgabentitel", value = "Summe zweier Zahlen", width = "220px")
        ),
        shiny::div(
          class = "ws-field",
          title = "Punkte, die diese Aufgabe in Moodle/PDF zählt",
          shiny::numericInput("meta_points", "Punkte", value = 1, min = 0, step = 0.5, width = "90px")
        ),
        shiny::div(
          class = "ws-field",
          title = "Dezimal- und Tausendertrennzeichen in der Anzeige",
          shiny::selectInput(
            "locale", "Zahlenformat",
            choices = c("EU (1.234,56)" = "EU", "US (1,234.56)" = "US"),
            selected = "EU", width = "150px"
          )
        ),
        shiny::div(
          class = "ws-field",
          title = "Fertige Beispielaufgabe in die Fläche laden",
          shiny::selectInput(
            "template", "Beispiel laden",
            choices = c(
              "Numerisch" = "num",
              "Einfachauswahl" = "schoice",
              "Mehrfachauswahl" = "mchoice",
              "Lückentext (CLOZE)" = "cloze"
            ),
            width = "180px"
          )
        ),
        shiny::div(
          class = "ws-field ws-field-btn",
          shiny::tags$span(class = "ws-field-label", "\u00a0"),
          shiny::actionButton("apply_template", "Beispiel übernehmen", class = "btn-sm btn-light")
        ),
        shiny::div(
          class = "ws-field",
          title = "Vorhandene Studio-Rmd öffnen",
          shiny::fileInput("load_rmd", "Datei öffnen", accept = c(".Rmd", ".rmd", ".txt"), buttonLabel = "Wählen…", placeholder = "keine")
        ),
        shiny::div(
          class = "ws-field ws-field-btn",
          shiny::tags$span(class = "ws-field-label", "Export"),
          shiny::div(
            class = "ws-export",
            shiny::downloadButton("dl_rmd", "Rmd", class = "btn-sm btn-light", title = "Aufgabe als R/exams-Rmd speichern"),
            shiny::downloadButton("dl_moodle", "Moodle", class = "btn-sm btn-warning", title = "Moodle-XML/ZIP erzeugen"),
            shiny::downloadButton("dl_pdf", "PDF", class = "btn-sm btn-light", title = "PDF erzeugen (LaTeX nötig)")
          )
        )
      ),
      shiny::div(
        class = "ws-body",
        shiny::div(
          class = "ws-palette",
          shiny::div(class = "ws-kicker", "Einfügen (ziehen oder klicken)"),
          shiny::uiOutput("palette_ui")
        ),
        shiny::div(
          class = "ws-main",
          shiny::div(class = "ws-kicker", "Aufgabe — ziehen oder klicken fügt ein · Chip wählen, × (Hover) oder Entf löscht"),
          shiny::uiOutput("canvas_ui")
        ),
        shiny::div(
          class = "ws-side",
          shiny::uiOutput("inspector_ui"),
          shiny::uiOutput("health_ui")
        )
      ),
      shiny::tags$script(src = "dnd.js")
    )
  )
}
