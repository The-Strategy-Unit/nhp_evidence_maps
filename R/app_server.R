#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_taxonomy_server("taxonomy_1")
  mod_summary_table_server("summary_table")
  mod_search_server("search")
}
