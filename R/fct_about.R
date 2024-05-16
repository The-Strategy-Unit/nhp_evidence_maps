#' about 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
wrangle_about_text <- function(dat) {
  
  dat |>
    dplyr::slice(5) |>  dplyr::pull(1) |>  # assumes text is in this location
    stringr::str_split_1("(\\r\\n){1,}") |>  # assumes paragraph breaks
    stringr::str_squish() |> 
    paste(collapse = "<br/><br/>") |> 
    shiny::HTML()  # prefer renderUI() for easier paragraph handling
  
}