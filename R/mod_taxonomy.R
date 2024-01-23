#' taxonomy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

taxonomy_raw <- readxl::read_xlsx("data-raw/evidence_map_data.xlsx",
                              sheet = "Taxonomy")

mechanisms <- taxonomy_raw[2:8,] |> 
  janitor::row_to_names(1) |> 
  dplyr::rename("Notes?" = 5) |> 
  gt::gt(rowname_col = "Mechanism") |> 
  gt::tab_stubhead(label = "Mechanism") |> 
  gt::tab_options(table.font.size = "12px",
                  column_labels.font.size = "14px",
                  column_labels.font.weight = "bold",
                  stub.font.size = "14px")

conditions <- taxonomy_raw[11:28,1] |> 
  janitor::row_to_names(1)

mod_taxonomy_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Mechanisms"),
    gt::gt_output(ns("mechanisms_gt")),
    h3("Conditions"),
    shiny::htmlOutput(ns("conditions_txt"))
 
  )
}
    
#' taxonomy Server Functions
#'
#' @noRd 
mod_taxonomy_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$mechanisms_gt <- gt::render_gt(mechanisms)
    
    output$conditions_txt <- shiny::renderUI({
      HTML(paste(conditions$`Health conditions`,
                  collapse = 
                  '<br>
                  <br>
                  <font size = "2">
                  [Placeholder text]
                  </font>   
                  <hr>
                 <br>'))
      })
    
  })
}
    
## To be copied in the UI
# mod_taxonomy_ui("taxonomy_1")
    
## To be copied in the server
# mod_taxonomy_server("taxonomy_1")
