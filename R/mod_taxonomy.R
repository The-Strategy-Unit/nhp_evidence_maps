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
mod_taxonomy_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Mechanisms",
      width = 12,
      gt::gt_output(ns("mechanisms_gt"))
    ),
    bs4Dash::box(
      title = "Setting",
      width = 12,
      gt::gt_output(ns("setting_gt"))
    ),
    bs4Dash::box(
      title = "Evidence type",
      width = 12,
      gt::gt_output(ns("evidence_type_gt"))
    ),
    bs4Dash::box(
      title = "Outcomes",
      width = 12,
      gt::gt_output(ns("outcomes_gt"))
    ),
    bs4Dash::box(
      title = "Effect",
      width = 12,
      gt::gt_output(ns("effect_gt"))
    )
  )
}

#' taxonomy Server Functions
#'
#' @noRd 
mod_taxonomy_server <- function(id, taxonomy_raw_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$mechanisms_gt <- taxonomy_raw_data |> 
      extract_taxonomy_table(9:15, 1:5, "Notes" = 5) |> 
      create_taxonomy_gt() |> 
      gt::render_gt(align = "left")
    
    output$setting_gt <- taxonomy_raw_data |> 
      extract_taxonomy_table(17:26, 1:2, "Description" = 2) |> 
      create_taxonomy_gt() |> 
      gt::render_gt(align = "left")
    
    output$evidence_type_gt <- taxonomy_raw_data |> 
      extract_taxonomy_table(28:30, 1:2, "Description" = 2) |> 
      create_taxonomy_gt() |> 
      gt::render_gt(align = "left")
    
    output$outcomes_gt <- taxonomy_raw_data |> 
      extract_taxonomy_table(32:37, 1:2, "Description" = 2) |> 
      create_taxonomy_gt() |> 
      gt::render_gt(align = "left")
    
    output$effect_gt <- taxonomy_raw_data |> 
      extract_taxonomy_table(39:43, 1:2, "Description" = 2) |> 
      create_taxonomy_gt() |> 
      gt::render_gt(align = "left")
    
  })
}

## To be copied in the UI
# mod_taxonomy_ui("taxonomy_1")

## To be copied in the server
# mod_taxonomy_server("taxonomy_1")
