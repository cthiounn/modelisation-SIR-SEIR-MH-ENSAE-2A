runApp <- function(){
  appDir <- system.file("app", package = "myshinyapp")
  shiny::runApp(appDir, display.mode = "normal")
}