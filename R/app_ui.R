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
      shiny::tags$link(rel = "stylesheet", href = "workspace.css"),
      shiny::tags$script(src = "dnd.js")
    ),
    shiny::div(
      class = "ws-shell",
      shiny::div(
        class = "ws-top",
        shiny::span(class = "ws-brand", "R/exams Studio"),
        shiny::textInput("meta_title", NULL, value = "Summe zweier Zahlen", width = "220px"),
        shiny::numericInput("meta_points", NULL, value = 1, min = 0, step = 0.5, width = "80px"),
        shiny::selectInput(
          "locale", NULL,
          choices = c("EU 1.234,56" = "EU", "US 1,234.56" = "US"),
          selected = "EU", width = "140px"
        ),
        shiny::selectInput(
          "template", NULL,
          choices = c(
            "Vorlage: Numerisch" = "num",
            "Vorlage: Einfachauswahl" = "schoice",
            "Vorlage: Mehrfachauswahl" = "mchoice",
            "Vorlage: CLOZE" = "cloze"
          ),
          width = "190px"
        ),
        shiny::actionButton("apply_template", "Laden", class = "btn-sm btn-light"),
        shiny::fileInput("load_rmd", NULL, accept = c(".Rmd", ".rmd", ".txt"), buttonLabel = "Rmd…", placeholder = ""),
        shiny::downloadButton("dl_rmd", "Rmd", class = "btn-sm btn-light"),
        shiny::downloadButton("dl_moodle", "Moodle", class = "btn-sm btn-warning"),
        shiny::downloadButton("dl_pdf", "PDF", class = "btn-sm btn-light")
      ),
      shiny::div(
        class = "ws-body",
        shiny::div(
          class = "ws-palette",
          shiny::div(class = "ws-kicker", "Ziehen"),
          shiny::uiOutput("palette_ui")
        ),
        shiny::div(
          class = "ws-main",
          shiny::div(class = "ws-kicker", "Aufgabe — ablegen und anklicken"),
          shiny::uiOutput("canvas_ui")
        ),
        shiny::div(
          class = "ws-side",
          shiny::uiOutput("inspector_ui"),
          shiny::uiOutput("health_ui")
        )
      )
    )
  )
}
