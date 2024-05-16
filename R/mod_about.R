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
    shiny::fluidRow(
      shiny::h1("NHP Evidence Map"),
      bs4Dash::box(
        title = "About",
        width = 12,
        collapsible = FALSE,
        shiny::htmlOutput(ns("about_html"))
      )
    )
  )
}

#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$about_html <- renderUI({ wrangle_about_text(dat) })
  })
}