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

## Bedienung

Eine Fläche, drei Spalten:

- **Links:** Palette — Werte, Lücken, Formel, Bedingung ziehen oder anklicken
- **Mitte:** Aufgabe als Text mit Chips; Chip anklicken wählt, × oder Entf löscht
- **Rechts:** nur die Felder des gewählten Objekts, Live-Vorschau, Ampel für den Werteraum

Nach Änderungen an Dateien unter `R/` die App neu starten (`www/` gilt ohne Neustart).

Kopfzeile: Titel, Punkte, EU/US, Vorlage, Rmd/Moodle/PDF.

Export nutzt `add_cloze()` / `format_metainfo()`. Keine `##ANSWER##`-Tags.

Rmd-Dateien aus diesem Studio lassen sich wieder laden (Modell steckt als Kommentar in der Datei).

## Tests

```r
testthat::test_dir("tests/testthat")
```
