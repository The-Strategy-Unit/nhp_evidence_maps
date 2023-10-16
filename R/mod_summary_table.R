#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

data <- readRDS('inst/app/data/tmp_data.rds') |> 
  dplyr::mutate(
    id = dplyr::row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
  )


mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::selectInput(ns("yearSelect"), 
                       label = "Select Year", 
                       choices = c("All Years", 
                                   stringr::str_sort(unique(data$`Publication year`),
                                                 decreasing = T))),
    shiny::selectInput(ns("varSelect"),
                       label = "Additional Variables",
                       choices = c("Choose additional variables" = "",
                                   "Country of study",
                                   "Study design",
                                   "Effect",
                                   "Setting",
                                   "Demographic",
                                   "No variable selected" = "no_value"
                                   ),
                       selected = "",
                       multiple = T),
      shiny::fluidRow(
        column(width = 8, DT::DTOutput(ns("summary"))),
        column(width = 4, shiny::plotOutput(ns("waffle")))
      ),
    shiny::verbatimTextOutput(ns("debug")),
    shiny::verbatimTextOutput(ns("debugVarSelect")),
    DT::DTOutput(ns("debugData")),
    DT::DTOutput(ns("selectedTable"))
    )
}

#' summary_table Server Functions
#'
#' @noRd
mod_summary_table_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selectedYear <- reactive(input$yearSelect)
    
    
    output$debug <- shiny::renderPrint(selectedYear())
    
    

    
    summary_tab_data <- reactive({
      shiny::req(selectedYear())
      data |>
        dplyr::filter(`Publication year` == selectedYear() | selectedYear() == "All Years") |>
        dplyr::select(any_of(c("Mechanism",
                               "Type of evidence",
                               "Citation",
                               "Publication year",
                               "Link",
                               input$varSelect)))|>
        dplyr::distinct()
    })
    
    
    evidence_map_data <- reactive({
      shiny::req(summary_tab_data())
      summary_tab_data() |>
        dplyr::select(Mechanism, `Type of evidence`) |>
        dplyr::group_by(Mechanism, `Type of evidence`) |>
        dplyr::summarise(count = dplyr::n()) |>
        tidyr::pivot_wider(names_from = `Type of evidence`, values_from = count) |>
        dplyr::ungroup() |>
        dplyr::mutate(id = dplyr::row_number())
    })
    
    
    waffle_data <- reactive({
      shiny::req(summary_tab_data())
      summary_tab_data() |>
        dplyr::select(Mechanism, `Type of evidence`)
    })

    output$summary <- DT::renderDT(
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

    
    selected_tab_data <- reactive({
      summary_tab_data()
    })

    shiny::observe({
      shiny::req(evidence_map_data())
      index <- shiny::req(input$summary_cells_selected)

      row <- index[[1]]
      col <- index[[2]] + 1

      row_selected <- evidence_map_data()$Mechanism[row]
      col_selected <- names(evidence_map_data()[col])

      summary_selected <- summary_tab_data() |> 
        dplyr::filter(
          Mechanism == row_selected,
          `Type of evidence` == col_selected
          ) |> 
        dplyr::select(-Mechanism, -`Type of evidence`)

      output$selectedTable <- DT::renderDT(summary_selected,
                                           options = list(
                                             dom = "t",
                                             ordering = F
                                             ),
                                           rownames = F,
                                           escape = F,
                                           selection = "none"
                                           )
      
      output$waffle <- shiny::renderPlot({
        
        filtered_waffle_data <- waffle_data()|>
          dplyr::filter(Mechanism == row_selected) |> 
          ggwaffle::waffle_iron(
            ggwaffle::aes_d(group = 'Type of evidence')) |> 
          dplyr::mutate(selected = ifelse(group == col_selected, T, F))
        
        shiny::req(filtered_waffle_data)
        filtered_waffle_data |> 
          ggplot2::ggplot(ggplot2::aes(x, y, fill = group))+
          ggwaffle::geom_waffle()+
          ggwaffle::geom_waffle(data = filtered_waffle_data |> 
                                  dplyr::filter(selected == T),
                                colour = 'blue',
                                show.legend = F)+
          ggplot2::coord_equal()+
          viridis::scale_fill_viridis(discrete = T)+
          ggwaffle::theme_waffle()+
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         legend.position = 'top',
                         legend.title = ggplot2::element_blank())+
          ggplot2::guides(fill = ggplot2::guide_legend(nrow=2, byrow=T))
      })
      
    })
  })
}

## To be copied in the UI
# mod_summary_table_ui("summary_table_1")

## To be copied in the server
# mod_summary_table_server("summary_table_1")
