#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
  
map_choices <- c("Type of evidence" = "typeOfEvidence",
                 "Mechanism",
                 "Outcome" = "outcome")

mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    bs4Dash::box(title = "Choose evidence map parameters",
                 width = 12,
                 collapsible = F,
      shiny::fluidRow(
        shiny::column(width = 3,
                      shiny::selectInput(ns("yearSelect"),
                                         label = "Select Year",
                                         choices = c(
                                           "All Years",
                                           stringr::str_sort(unique(evidence_maps::my_dataset$`Publication year`),
                                                             decreasing = T
                                           )
                                         ),
                                         multiple = T,
                                         selected = "All Years"
                      )
        ),
        shiny::column(width = 3,
                      shiny::selectInput(ns("varSelect"),
                                         label = "Additional Variables",
                                         choices = c(
                                           "Choose additional variables" = "",
                                           "Country of study",
                                           "Study design",
                                           "Effect",
                                           "Setting",
                                           "Population"),
                                         selected = "",
                                         multiple = T
                      )
        ),
        
        shiny::column(width = 3, 
                      shiny::selectInput(ns("mapRow"), 
                                         "Row category",
                                         choices = map_choices,
                                         selected = "Mechanism"
                      )
        ),
        shiny::column(width = 3,
                      shiny::selectInput(ns("mapCol"),
                                         "Column category",
                                         choices = map_choices,
                                         selected = "Type of evidence"
                      )
        )
      )
    ),
    bs4Dash::box(title = "Evidence map (click a cell and scroll down to see the actual evidence  
                 below)",
                 width = 12,

    shiny::fluidRow(
      column(width = 8, DT::DTOutput(ns("evidenceMap"))),
      column(width = 4, shiny::plotOutput(ns("waffle")))
    )
    ),
    bs4Dash::box(title = "Evidence that you have selected from the map",
                 width = 12, 
                 DT::DTOutput(ns("selectedTable"))
    )
  )
}

#' summary_table Server Functions
#'
#' @noRd
mod_summary_table_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- shiny::reactive({
      shiny::req(map_row_cat(), map_col_cat())
      if( "outcome" %in% 
          c(map_row_cat(), map_col_cat())){
      evidence_maps::my_dataset |> 
        dplyr::filter(outcome_recorded == T)
      }else({
        evidence_maps::my_dataset})
    })
    
   
    # row and col map ----
    
    ## reactives ----
    map_row_cat <- shiny::reactive(input$mapRow) 
    map_col_cat <- shiny::reactive(input$mapCol)
    
    ## update selectinput ---- 
    
    shiny::observe({
      shiny::updateSelectInput(session, 
                               "mapRow",
                               choices = 
                                 map_choices[map_choices != map_col_cat()],
                               selected = map_row_cat()
                               )
      shiny::updateSelectInput(session, 
                               "mapCol",
                               choices = 
                                 map_choices[map_choices != map_row_cat()],
                               selected = map_col_cat()
      )
    })
    
    
    # setup ----
    evidence_map_skeleton <- reactive({
      req(map_row_cat(), map_col_cat())
     data() |> 
       dplyr::count(.data[[map_row_cat()]], .data[[map_col_cat()]])|>
      dplyr::select(-n)
    })
    selectedYear <- shiny::reactive(input$yearSelect)
    selectedVars <- shiny::reactive(input$varSelect)

    selectedCell <- reactiveValues(
      row = 1,
      col = 1
    )

    # summary_tab_data ----
    #dataframe to be used in tab 
    summary_tab_data <- reactive({
      req(selectedYear())
      data() |>
        dplyr::filter(`Publication year` %in% selectedYear() | 
                        "All Years" %in% selectedYear()) |>
        dplyr::select(tidyselect::any_of(c(
          map_row_cat(),
          map_col_cat(),
          "Authors",
          "Title",
          "Publication year",
          "Link",
          input$varSelect
        ))) |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(input$varSelect),
            stringr::str_to_sentence
          )
        ) |> 
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(c("Country of study",
                                 "Study design",
                                 "Effect",
                                 "Setting")),
            forcats::as_factor
          )
        ) |> 
         dplyr::distinct()
    })


    # flush cell selection, potential bug ----
    # observe({
    #   shiny::req(selectedVars(), selectedYear())
    #   # flush selected cell index
    #   selectedCell$row <- NULL
    #   selectedCell$col <- NULL
    # })




    # evidence map ----
    evidence_map_data <- reactive({
      evidence_map_skeleton() |>
        dplyr::left_join(
          summary_tab_data() |>
            dplyr::count(.data[[map_row_cat()]], 
                         .data[[map_col_cat()]])
            # dplyr::select(map_row_cat(), map_col_cat()) |>
            # dplyr::group_by(map_row_cat(), map_col_cat()) |>
            # dplyr::summarise(count = dplyr::n())
        ) |>
        tidyr::pivot_wider(
          names_from = map_col_cat(),
          values_from = n
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(id = dplyr::row_number())
    })

    output$evidenceMap <- DT::renderDT(
      evidence_map_data() |>
        dplyr::select(-id),
      options = list(
        dom = "t",
        ordering = F
      ),
      selection = list(
        mode = "single",
        target = "cell"
      ),
      rownames = F
    )


    # selected cell ----
    shiny::observe({
     # index <- shiny::req(input$evidenceMap_cells_selected)

      #row <- shiny::req(input$evidenceMap_cells_selected)[[1]]
      #col <- shiny::req(input$evidenceMap_cells_selected)[[2]] + 1

      selectedCell$row <- evidence_map_data()[[map_row_cat()]][shiny::req(input$evidenceMap_cells_selected)[[1]]]
      selectedCell$col <- names(evidence_map_data()[shiny::req(input$evidenceMap_cells_selected)[[2]] + 1])
    })


    # # waffle ----


    output$waffle <- shiny::renderPlot(res = 80, 
                                       {
      shiny::req(input$evidenceMap_cells_selected)
      
      group_val <- map_col_cat()
      
      filtered_waffle_data <- summary_tab_data() |>
        dplyr::select(map_row_cat(), map_col_cat()) |>
        dplyr::filter(.data[[map_row_cat()]] == selectedCell$row) 

      shiny::req(filtered_waffle_data)
      filtered_waffle_data |>
        ggwaffle::waffle_iron(mapping = paste0(map_col_cat()),
                              rows = floor(sqrt(nrow(filtered_waffle_data))))|>
        dplyr::mutate(selected = ifelse(group == selectedCell$col, T, F)) |> 
        ggplot2::ggplot(ggplot2::aes(x, y, fill = group)) +
        ggwaffle::geom_waffle() +
        ggwaffle::geom_waffle(
          data = ~dplyr::filter(.x, 
                                selected == T),
          colour = "black",
          show.legend = F) +
        ggplot2::coord_equal() +
        viridis::scale_fill_viridis(discrete = T) +
        ggwaffle::theme_waffle() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 14),
          panel.border = ggplot2::element_rect(colour = "grey70",
                                               fill = NA)
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1
                                                     ))
        
    })

    # 
    output$selectedTable <- summary_tab_data() |>
      dplyr::filter(
        .data[[map_row_cat()]] == selectedCell$row,
        .data[[map_col_cat()]] == selectedCell$col
      ) |>
      dplyr::select(-map_row_cat(), -map_col_cat()) |>
      DT::renderDT(
        options = list(
          #dom = "t",
          #ordering = F
          ),
        filter = "top",
        rownames = F,
        escape = F,
        selection = "none"
      )
  })
}

## To be copied in the UI
# mod_summary_table_ui("summary_table_1")

## To be copied in the server
# mod_summary_table_server("summary_table_1")
