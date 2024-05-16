#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  evidence_list <- get_pinned_data()
  evidence_data <- get_evidence_data(evidence_list)
  taxonomy_raw_data <- evidence_list[["About this map"]]
  
  mod_summary_table_server("summary_table", evidence_data)
  mod_search_server("search", evidence_data)
  mod_taxonomy_server("taxonomy_1", taxonomy_raw_data)
  
}
