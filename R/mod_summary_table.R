#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

data <- evidence_maps::my_dataset |> 
  dplyr::mutate(
    id = dplyr::row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
    )

  # readRDS("data/my_dataset.rda") |>


mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::selectInput(ns("yearSelect"),
      label = "Select Year",
      choices = c(
        "All Years",
        stringr::str_sort(unique(data$`Publication year`),
          decreasing = T
        )
      ),
      multiple = T,
      selected = "All Years"
    ),
    shiny::selectInput(ns("varSelect"),
      label = "Additional Variables",
      choices = c(
        "Choose additional variables" = "",
        "Country of study",
        "Study design",
        "Effect",
        "Setting",
        "Demographic",
        "No variable selected" = "no_value"
      ),
      selected = "",
      multiple = T
    ),
    shiny::verbatimTextOutput(ns("debug")),
    shiny::verbatimTextOutput(ns("debugVarSelect")),
    shiny::verbatimTextOutput(ns("inputValsDebug")),
    shiny::fluidRow(
      column(width = 8, DT::DTOutput(ns("evidenceMap"))),
      column(width = 4, shiny::plotOutput(ns("waffle")))
    ),
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

    shiny::observe({
      output$debug <- shiny::renderPrint(as.character(shiny::req(input$evidenceMap_cells_selected)))
    })
  
    output$inputValsDebug <- shiny::renderPrint(selectedYear())
    # setup ----
    evidence_map_skeleton <- data |>
      dplyr::select(Mechanism, `Type of evidence`) |>
      dplyr::group_by(Mechanism, `Type of evidence`) |>
      dplyr::summarise(count = dplyr::n()) |>
      dplyr::select(-count)

    selectedYear <- reactive(input$yearSelect)
    selectedVars <- reactive(input$varSelect)

    selectedCell <- reactiveValues(
      row = 1,
      col = 1
    )

    # dataframe to be used in tab ----
    summary_tab_data <- reactive({
      req(selectedYear())
      data |>
        dplyr::filter(`Publication year` %in% selectedYear() | 
                        "All Years" %in% selectedYear()) |>
        dplyr::select(tidyselect::any_of(c(
          "Mechanism",
          "Type of evidence",
          "Citation",
          "Publication year",
          "Link",
          input$varSelect
        ))) |>
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
      evidence_map_skeleton |>
        dplyr::left_join(
          summary_tab_data() |>
            dplyr::select(Mechanism, `Type of evidence`) |>
            dplyr::group_by(Mechanism, `Type of evidence`) |>
            dplyr::summarise(count = dplyr::n())
        ) |>
        tidyr::pivot_wider(
          names_from = `Type of evidence`,
          values_from = count
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

      selectedCell$row <- evidence_map_data()$Mechanism[shiny::req(input$evidenceMap_cells_selected)[[1]]]
      selectedCell$col <- names(evidence_map_data()[shiny::req(input$evidenceMap_cells_selected)[[2]] + 1])
    })


    # waffle ----


    output$waffle <- shiny::renderPlot({
      shiny::req(input$evidenceMap_cells_selected)
      filtered_waffle_data <- summary_tab_data() |>
        dplyr::select(Mechanism, `Type of evidence`) |>
        dplyr::filter(Mechanism == selectedCell$row) |>
        ggwaffle::waffle_iron(
          ggwaffle::aes_d(group = "Type of evidence")
        ) |>
        dplyr::mutate(selected = ifelse(group == selectedCell$col, T, F))

      shiny::req(filtered_waffle_data)
      filtered_waffle_data |>
        ggplot2::ggplot(ggplot2::aes(x, y, fill = group)) +
        ggwaffle::geom_waffle() +
        ggwaffle::geom_waffle(
          data = filtered_waffle_data |>
            dplyr::filter(selected == T),
          colour = "blue",
          show.legend = F
        ) +
        ggplot2::coord_equal() +
        viridis::scale_fill_viridis(discrete = T) +
        ggwaffle::theme_waffle() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "top",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = T))
    })


    output$selectedTable <- summary_tab_data() |>
      dplyr::filter(
        Mechanism == selectedCell$row,
        `Type of evidence` == selectedCell$col
      ) |>
      dplyr::select(-Mechanism, -`Type of evidence`) |>
      DT::renderDT(
        options = list(
          #dom = "t",
          ordering = F
        ),
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
