#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  evidence_data <- get_data()
  
  mod_taxonomy_server("taxonomy_1")
  mod_summary_table_server("summary_table", evidence_data)
  mod_search_server("search", evidence_data)
  
}
