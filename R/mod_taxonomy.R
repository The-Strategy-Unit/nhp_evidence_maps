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

board <- pins::board_connect()
pin_list <- pins::pin_read(board, "matt.dray/nhp_evidence_map")

# taxonomy_raw <- readxl::read_xlsx("data-raw/evidence_map_data.xlsx",
#                                   sheet = "Taxonomy")

taxonomy_raw <- pin_list[["About this map"]]

fun_make_gt <- function(x){
  x |> 
    gt::gt(rowname_col = names(x[1])) |> 
    gt::tab_stubhead(label = names(x[1])) |> 
    gt::tab_options(table.font.size = "12px",
                    column_labels.font.size = "14px",
                    column_labels.font.weight = "bold",
                    stub.font.size = "14px")
}

mechanisms <- taxonomy_raw[9:15,] |> 
  janitor::row_to_names(1) |> 
  dplyr::rename("Notes" = 5) |> 
  fun_make_gt()

setting <- taxonomy_raw[17:26, 1:2] |> 
  janitor::row_to_names(1) |> 
  dplyr::rename("Description" = 2) |> 
  fun_make_gt()


evidence_type <- taxonomy_raw[28:30, 1:2] |> 
  janitor::row_to_names(1) |> 
  dplyr::rename("Description" = 2) |> 
  fun_make_gt()


outcomes <- taxonomy_raw[32:37, 1:2]  |> 
  janitor::row_to_names(1) |> 
  dplyr::rename("Description" = 2) |> 
  fun_make_gt()

effect <- taxonomy_raw[39:43, 1:2] |> 
  janitor::row_to_names(1) |> 
  fun_make_gt()

mod_taxonomy_ui <- function(id){
  ns <- NS(id)
  tagList(
    #h3("Mechanisms"),
    gt::gt_output(ns("mechanisms_gt")),
    gt::gt_output(ns("setting_gt")),
    gt::gt_output(ns("evidence_type_gt")),
    gt::gt_output(ns("outcomes_gt")),
    gt::gt_output(ns("effect_gt")),
    
    #h3("Setting"),
    #h3("Evidence Type")
    #shiny::htmlOutput(ns("conditions_txt"))
 
  )
}
    
#' taxonomy Server Functions
#'
#' @noRd 
mod_taxonomy_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$mechanisms_gt <- gt::render_gt(mechanisms)
    
    output$setting_gt <- gt::render_gt(setting)
    
    output$evidence_type_gt <- gt::render_gt(evidence_type)
    
    output$outcomes_gt <- gt::render_gt(outcomes)
    
    output$effect_gt <- gt::render_gt(effect)
    
    # output$conditions_txt <- shiny::renderUI({
    #   HTML(paste(conditions$`Health conditions`,
    #               collapse = 
    #               '<br>
    #               <br>
    #               <font size = "2">
    #               [Placeholder text]
    #               </font>   
    #               <hr>
    #              <br>'))
    #   })
    
  })
}
    
## To be copied in the UI
# mod_taxonomy_ui("taxonomy_1")
    
## To be copied in the server
# mod_taxonomy_server("taxonomy_1")
