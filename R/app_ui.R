app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B4F72",
    secondary = "#5D6D7E",
    success = "#1E8449",
    info = "#1A5276",
    warning = "#B9770E",
    danger = "#922B21",
    font_scale = 0.96
  )
}

app_ui <- function() {
  bslib::page_navbar(
    title = "R/exams Studio",
    theme = app_theme(),
    id = "main_nav",
    window_title = "R/exams Studio",
    fillable = TRUE,
    header = shiny::tags$head(
      shiny::tags$script(src = "insert.js"),
      shiny::tags$style(shiny::HTML("
        .studio-card { margin-bottom: 1rem; }
        .status-ok { color: #1e8449; font-weight: 600; }
        .status-rule { color: #b9770e; font-weight: 600; }
        .status-implausible { color: #922b21; font-weight: 600; }
        .status-error { color: #6c3483; font-weight: 600; }
        .help-muted { color: #5d6d7e; font-size: 0.92rem; }
        .item-well { background: #f8f9fb; border: 1px solid #d5d8dc; border-radius: 8px; padding: 0.9rem; margin-bottom: 0.8rem; }
        iframe.preview-frame { border: 1px solid #d5d8dc; border-radius: 8px; background: white; }
        .navbar { box-shadow: 0 1px 0 rgba(0,0,0,.08); }
        .insert-bar { display: flex; flex-wrap: wrap; gap: 0.45rem; align-items: flex-end; margin-bottom: 0.6rem; }
        .insert-bar .form-group { margin-bottom: 0; }
        .live-frame { min-height: 280px; }
        .choice-row { background: #fff; border: 1px solid #e5e8eb; border-radius: 6px; padding: 0.45rem 0.55rem; margin-bottom: 0.35rem; }
      "))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::selectInput(
      "locale", NULL,
      choices = c("Zahlenformat EU (1.234,56)" = "EU", "Zahlenformat US (1,234.56)" = "US"),
      selected = "EU",
      width = "240px"
    )),
    bslib::nav_panel(
      "Aufgabe",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 340,
          shiny::h5("Vorlage"),
          shiny::selectInput(
            "template", "Startvorlage",
            choices = c(
              "Numerisch (Summe)" = "num",
              "Einfachauswahl" = "schoice",
              "Mehrfachauswahl" = "mchoice",
              "Lueckentext (CLOZE)" = "cloze"
            )
          ),
          shiny::actionButton("apply_template", "Vorlage laden", class = "btn-outline-primary w-100"),
          shiny::hr(),
          shiny::h5("Datei"),
          shiny::fileInput("load_rmd", "Rmd laden", accept = c(".Rmd", ".rmd", ".txt")),
          shiny::downloadButton("dl_rmd", "Rmd speichern", class = "btn-primary w-100"),
          shiny::hr(),
          shiny::p(
            class = "help-muted",
            "Text schreiben, Werte und Lücken über die Knöpfe einfügen.",
            " Formeln ohne Dollarzeichen eingeben — die App setzt die Formelumgebung."
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            class = "studio-card",
            bslib::card_header("1. Fragetext schreiben"),
            bslib::card_body(
              shiny::textInput("meta_name", "Interner Name", value = "summe"),
              shiny::textInput("meta_title", "Titel (sichtbar)", value = "Summe zweier Zahlen"),
              shiny::radioButtons(
                "meta_type", "Aufgabentyp",
                choices = c(
                  "Numerisch" = "num",
                  "Einfachauswahl" = "schoice",
                  "Mehrfachauswahl" = "mchoice",
                  "Lückentext (CLOZE)" = "cloze"
                ),
                selected = "num",
                inline = TRUE
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("meta_points", "Punkte", value = 1, min = 0, step = 0.5)),
                shiny::column(6, shiny::textInput("meta_section", "Abschnitt / Thema", value = ""))
              ),
              shiny::div(
                class = "insert-bar",
                shiny::div(
                  shiny::selectInput("insert_var_name", "Wert einfügen", choices = c("a" = "a"), width = "160px"),
                  shiny::actionButton("insert_var", "Wert", class = "btn-sm btn-outline-primary", icon = shiny::icon("hashtag"))
                ),
                shiny::div(
                  shiny::textInput("insert_math_text", "Formel (ohne $ … $)", value = "", placeholder = "z. B. A_0 oder a + b", width = "220px"),
                  shiny::actionButton("insert_math", "Formel", class = "btn-sm btn-outline-primary", icon = shiny::icon("square-root-variable"))
                ),
                shiny::div(
                  shiny::selectInput(
                    "insert_gap_type", "Lücke einfügen",
                    choices = c("Zahleneingabe" = "num", "Einfachauswahl" = "schoice", "Mehrfachauswahl" = "mchoice"),
                    width = "180px"
                  ),
                  shiny::actionButton("insert_gap", "Lücke", class = "btn-sm btn-primary", icon = shiny::icon("i-cursor"))
                )
              ),
              shiny::textAreaInput(
                "question", NULL,
                rows = 8, width = "100%",
                value = "Berechnen Sie $⟨a⟩ + ⟨b⟩$."
              ),
              shiny::p(
                class = "help-muted",
                "Cursor in den Text setzen, dann Wert, Formel oder Lücke einfügen.",
                " Die blauen Chips in der Vorschau sind gezogene Zahlen, die Kästen sind Eingaben."
              ),
              shiny::textAreaInput(
                "solution_text", "Lösungsweg (nach der Abgabe sichtbar)",
                rows = 4, width = "100%",
                value = "Addieren Sie die beiden Zahlen: $⟨a⟩ + ⟨b⟩ = ⟨sol⟩$."
              )
            )
          ),
          bslib::card(
            class = "studio-card",
            bslib::card_header("2. So sieht die Aufgabe aus"),
            bslib::card_body(
              shiny::uiOutput("live_preview_ui")
            )
          )
        ),
        bslib::card(
          class = "studio-card",
          bslib::card_header("3. Lücken zuordnen"),
          bslib::card_body(
            shiny::p(
              class = "help-muted",
              "Jeder Lücke eine Lösung zuweisen — per Variablenliste, nicht als R-Code.",
              " Prozent: Studierende geben 12,35 ein, intern wird mit 0,1235 gerechnet."
            ),
            shiny::uiOutput("items_ui")
          )
        )
      )
    ),
    bslib::nav_panel(
      "Variablen",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 320,
          shiny::p(
            class = "help-muted",
            "Unabhaengige Variablen bekommen einen Bereich und eine Schrittweite.",
            " Abgeleitete Variablen (z. B. sol = a + b) sind Formeln in R-Schreibweise."
          ),
          shiny::selectInput(
            "new_var_kind", "Neue Variable",
            choices = c(
              "Ganzzahl" = "integer",
              "Dezimalzahl" = "numeric",
              "Feste Liste" = "set",
              "Formel (abgeleitet)" = "derived"
            )
          ),
          shiny::actionButton("add_var", "Variable hinzufuegen", class = "btn-primary w-100")
        ),
        shiny::uiOutput("vars_ui")
      )
    ),
    bslib::nav_panel(
      "Regeln",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 320,
          shiny::p(
            class = "help-muted",
            "Eine Regel ist ein R-Ausdruck, der TRUE ergeben muss, z. B. a > b oder a %% 2 == 0.",
            " Ungueltige Kombinationen werden im naechsten Reiter rot markiert und beim Export verworfen."
          ),
          shiny::selectInput("rule_preset", "Schnellregel", choices = c(
            "Selbst schreiben" = "",
            "a groesser als b" = "a > b",
            "a ungleich b" = "a != b",
            "a gerade" = "a %% 2 == 0",
            "a ungerade" = "a %% 2 == 1",
            "Betrag a-b mindestens 1" = "abs(a - b) >= 1"
          )),
          shiny::actionButton("add_rule", "Regel hinzufuegen", class = "btn-primary w-100")
        ),
        shiny::uiOutput("rules_ui")
      )
    ),
    bslib::nav_panel(
      "Kombinationen",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::numericInput("max_rows", "Max. gepruefte Kombinationen", value = 4000, min = 50, max = 20000, step = 50),
          shiny::uiOutput("axis_ui"),
          shiny::actionButton("run_grid", "Kombinationen pruefen", class = "btn-primary w-100"),
          shiny::hr(),
          shiny::verbatimTextOutput("grid_summary")
        ),
        bslib::layout_columns(
          col_widths = 12,
          shiny::uiOutput("grid_boxes"),
          bslib::card(
            bslib::card_header("Werteraum"),
            bslib::card_body(plotly::plotlyOutput("grid_plot", height = "420px"))
          ),
          bslib::card(
            bslib::card_header("Tabelle"),
            bslib::card_body(DT::DTOutput("grid_table"))
          )
        )
      )
    ),
    bslib::nav_panel(
      "Vorschau",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::numericInput("preview_seed", "Zufalls-Seed", value = 1, min = 1, step = 1),
          shiny::actionButton("do_preview", "Vorschau erzeugen", class = "btn-primary w-100"),
          shiny::p(
            class = "help-muted",
            "Die Vorschau rendert die Aufgabe mit dem echten R/exams-Paket (HTML + MathJax)."
          ),
          shiny::verbatimTextOutput("preview_status")
        ),
        shiny::uiOutput("preview_ui")
      )
    ),
    bslib::nav_panel(
      "Export",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Moodle"),
          bslib::card_body(
            shiny::numericInput("moodle_n", "Anzahl Zufallsvarianten", value = 5, min = 1, max = 100),
            shiny::downloadButton("dl_moodle", "Moodle-XML / ZIP", class = "btn-primary"),
            shiny::p(class = "help-muted", "Erzeugt exams2moodle() inkl. CLOZE-Feldern.")
          )
        ),
        bslib::card(
          bslib::card_header("PDF"),
          bslib::card_body(
            shiny::numericInput("pdf_n", "Anzahl Varianten", value = 1, min = 1, max = 20),
            shiny::downloadButton("dl_pdf", "PDF erzeugen", class = "btn-primary"),
            shiny::p(class = "help-muted", "Benötigt eine LaTeX-Installation (tinytex). Die Bildschirmvorschau bleibt HTML.")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Erzeugtes Rmd (moderne Syntax)"),
        bslib::card_body(shiny::verbatimTextOutput("rmd_preview"))
      )
    )
  )
}
