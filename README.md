# R/exams Studio

Visuelles Dashboard zum Zusammenstellen dynamischer [R/exams](https://www.R-exams.org/)-Aufgaben für Moodle und PDF.

Autorinnen und Autoren brauchen kein R und kein exams-Wissen: Variablen, Wertebereiche, Plausibilitätsregeln und Lücken werden über Formulare gepflegt. Die App schreibt modernes Rmd mit `add_cloze()` und `format_metainfo()`.

## Start

Voraussetzungen: R ≥ 4.1, Pakete `exams` (≥ 2.4), `shiny`, `bslib`, `plotly`, `DT`, `yaml`, `jsonlite`, `knitr`, `rmarkdown`.

```r
shiny::runApp("D:/GitLab/apps/exams")
```

Oder im Projektordner:

```r
shiny::runApp()
```

## Funktionen

| Bereich | Inhalt |
| --- | --- |
| Aufgabe | `num`, `schoice`, `mchoice`, `cloze` (Mischformen); Markdown + `$Formeln$` |
| Variablen | Bereich, Schrittweite, Werteliste oder Formel (`sol = a + b`) |
| Regeln | R-Ausdruck muss `TRUE` sein, z. B. `a > b` |
| Kombinationen | Gitter, Ampel, Scatter, Tabelle |
| Vorschau | echtes `exams2html` / MathJax |
| Export | Rmd, Moodle (`exams2moodle`), PDF (`exams2pdf`) |
| Lokalisierung | EU `1.234,56` / US `1,234.56` |

Im Editor: Werte und Lücken über die Knöpfe einfügen (`⟨a⟩`, `〔Lücke 1〕`). Formeln ohne `$…$` schreiben. Die rechte Seite zeigt sofort die Studierendenansicht.

Rmd-Dateien aus diesem Studio lassen sich wieder laden (Modell steckt als Kommentar in der Datei).

## Tests

```r
testthat::test_dir("tests/testthat")
```
