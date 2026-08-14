# Launch: shiny::runApp() or examsstudio::run_app()
options(shiny.autoload.r = FALSE)
r_files <- sort(list.files("R", pattern = "[.][Rr]$", full.names = TRUE))
for (f in r_files) {
  sys.source(f, envir = environment(), keep.source = TRUE)
}
shiny::shinyApp(ui = app_ui(), server = app_server)
