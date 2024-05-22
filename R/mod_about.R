#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::h1("NHP Evidence Map"),
    bs4Dash::box(
      title = "About",
      width = 12,
      collapsible = FALSE,
      tags$img(src = "www/nhp_logo.png", style = "height: 150px;"),
      shiny::HTML("&nbsp;&nbsp;&nbsp;"),
      tags$img(
        src = "www/tsu_logo_black_screen_transparent.png",
        style = "height: 150px;"
      ),
      shiny::HTML("<br/><br/>"),
      md_file_to_html("app", "text", "about.md")
    )
  )
}
