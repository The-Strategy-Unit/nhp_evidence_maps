#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

data <- evidence_maps::my_dataset


search_data <- evidence_maps::my_dataset |> 
  dplyr::select(Authors, Title, `Publication year`, Link) |>
  dplyr::group_by(Authors, Title, `Publication year`, Link) |> 
  dplyr::summarise(tmp = dplyr::n()) |> 
  dplyr::select(-tmp) |>
  dplyr::ungroup() |> 
  dplyr::mutate(
    id = dplyr::row_number())
    
  
  
data_string_split <- search_data |>
  dplyr::mutate(search_string = paste(Authors, Title, `Publication year`)) |>
  dplyr::select(id, search_string) |>
  tidyr::separate_longer_delim(search_string, delim = " ") |>
  dplyr::mutate(search_string = stringr::str_remove_all(search_string,"[^A-Za-z]"),
                search_string = stringr::str_to_lower(search_string)) |>
  dplyr::filter(!search_string == "")


mod_search_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  shiny::tagList(
    shiny::numericInput(ns('dist'),
                      label = 'Search string distance',
                     value = 1),
    shinyWidgets::searchInput(ns("search"),
                              label = "Search Evidence",
                              placeholder = "Search..",
                              btnSearch = icon("magnifying-glass"),
                              btnReset = icon("xmark")),
    shiny::verbatimTextOutput(ns('debug'), placeholder = T),
    DT::DTOutput(ns("search_table"))
    
  )
}
    
#' search Server Functions
#'
#' @noRd 
mod_search_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dist_val <- reactive(input$dist)
    
    output$debug <- shiny::renderPrint(dist_val())

    observe({input$search_search

      req(dist_val)

      s <- stringr::str_split(input$search, " ")[[1]] |>
        stringr::str_remove_all("[^A-Za-z]") |>
        stringr::str_to_lower() |>
        purrr::discard(~.x == "")

      
      f <- \(text) purrr::some(s, ~min(stringdist::stringdist(.x, text)) <= dist_val())

      matches <- unique(data_string_split[purrr::map_lgl(data_string_split[["search_string"]], f),]$id)

      d <- search_data |> dplyr::filter(id %in% matches) |>
        dplyr::select(-id)
      
      output$search_table <- DT::renderDT(d,
                                          options = list(
                                            #dom = "t",
                                            #ordering = F
                                          ),
                                          filter = "top",
                                          escape = F,
                                          rownames = F,
                                          selection = "none")


    })
    
    
    observe({input$search_reset
      
      output$search_table <- DT::renderDT(search_data,
                                          options = list(
                                            dom = "ltip",
                                            
                                            ordering = T
                                          ),
                                          filter = "top",
                                          escape = F,
                                          rownames = F,
                                          selection = "none")
      })
    
    
}
)}
    
## To be copied in the UI
# mod_search_ui("search_1")
    
## To be copied in the server
# mod_search_server("search_1")
