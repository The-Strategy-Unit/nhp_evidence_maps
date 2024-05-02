#' data 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_data <- function(pin_name = "matt.dray/nhp_evidence_map_data") {
  
  pinned_data <- get_pinned_data(pin_name)
  
  wrangled_studies <- wrangle_studies(pinned_data)
  wrangled_reviews <- wrangle_reviews(pinned_data)
  
  wrangled_data <- dplyr::bind_rows(wrangled_studies, wrangled_reviews)
  
  wrangled_data |> 
    dplyr::mutate(
      `Type of evidence` = stringr::str_to_title(`Type of evidence`)
    ) |> 
    dplyr::mutate(
      id = dplyr::row_number(),
      Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")) |> 
    dplyr::rename(typeOfEvidence = `Type of evidence`)
  
}

get_pinned_data <- function(pin_name = "matt.dray/nhp_evidence_map_data") {
  board <- pins::board_connect()
  pins::pin_read(board, pin_name)
}

wrangle_studies <- function(pinned_data) {
  
  studies_raw <- pinned_data[["Studies"]]
  
  studies_selected <- studies_raw |>
    dplyr::select(
      "Unique ref no",
      "Authors",
      "Publication year",
      "Title",
      "Journal",
      "Abstract",
      "Country of study",
      "Link" = "Link to full text",
      "Type of evidence",
      "Study design",
      "Mechanism",
      "Population",
      "Setting" = `...19`,
      38:47,
      "Effect" = "Effect\r\n(statistical rather than clinical significance?)"
    ) 
  
  outcomes_reported_i <- which(colnames(studies_selected) == "Outcomes reported")
  
  studies_outcome_idxs <- tibble::tibble(
    start_col = seq(outcomes_reported_i, outcomes_reported_i + 9, 2),
    end_col = seq(outcomes_reported_i + 1, outcomes_reported_i + 9, 2)
  )
  
  nested_studies <- purrr::map2(
    studies_outcome_idxs[["start_col"]],
    studies_outcome_idxs[["end_col"]],
    \(.x, .y) nest_studies(studies_selected, .x, .y)
  ) |> 
    purrr::list_rbind() |> 
    dplyr::filter(!is.na(`Unique ref no`))
  
  studies_selected |> 
    dplyr::filter(!is.na(.data$`Unique ref no`) & !is.na(.data$Authors)) |> 
    dplyr::select(-c(`Outcomes reported`:`...47`)) |> 
    dplyr::left_join(
      nested_studies, 
      by = "Unique ref no",
      relationship = "many-to-many")
  
}

nest_studies <- function(studies, start_col, end_col) {
  
  tmp <- studies |> 
    dplyr::select("Unique ref no", .env$start_col, .env$end_col)
  
  tmp_new <- tmp
  
  tmp_new[2, 2] <- paste(tmp[[1, 2]], tmp[2, 2], sep = ".")
  tmp_new[2, 3] <- paste(tmp[[1, 2]], tmp[2, 3], sep = ".")
  
  names(tmp_new)[2:3] <- tmp_new[2,2:3]
  
  tmp_new <- tmp_new[-(1:2), ]
  
  tmp_new |> 
    dplyr::mutate(
      outcome_recorded = dplyr::if_else(!is.na(tmp_new[[2]]), TRUE, FALSE)
    ) |> 
    tidyr::pivot_longer(
      -c(`Unique ref no`, outcome_recorded), 
      names_to = "outcome_info",
      values_to = "outcome_value"
    ) |> 
    dplyr::mutate(
      outcome = stringr::str_extract(outcome_info, ".*(?=\\.)"),
      outcome_info = stringr::str_extract(outcome_info, "(?<=\\.).*")) |> 
    dplyr::group_by(`Unique ref no`, outcome, outcome_recorded) |> 
    tidyr::nest() |> 
    dplyr::ungroup()
  
}

wrangle_reviews <- function(pinned_data) {
  
  reviews_raw <- pinned_data[["Reviews"]]
  
  reviews_selected <- reviews_raw |>
    dplyr::select(
      "Unique ref no",
      "Authors",
      "Publication year",
      "Title",
      "Journal",
      "Abstract",
      "Country of study",
      "Link" = "Link to full text",
      "Type of evidence",
      "Study design",
      "Mechanism",
      "Population",
      "Setting" = `...19`,
      37:46,
      "Effect" = "Effect\r\n(statistical rather than clinical significance?)"
    )
  
  outcomes_reported_i <- which(colnames(reviews_selected) == "Outcomes reported")
  
  reviews_outcome_idxs <- tibble::tibble(
    start_col = seq(outcomes_reported_i, outcomes_reported_i + 9, 2),
    end_col = seq(outcomes_reported_i + 1, outcomes_reported_i + 9, 2)) 
  
  nested_reviews <- purrr::map2(
    reviews_outcome_idxs[["start_col"]],
    reviews_outcome_idxs[["end_col"]],
    \(.x, .y) nest_reviews(reviews_selected, .x, .y)
  ) |> 
    purrr::list_rbind() |> 
    dplyr::filter(!is.na(`Unique ref no`))
  
  reviews_selected |> 
    dplyr::filter(!is.na(.data$`Unique ref no`) & !is.na(.data$Authors)) |> 
    dplyr::select(-c(`Outcomes reported`:`...46`)) |> 
    dplyr::left_join(
      nested_reviews, 
      by = "Unique ref no",
      relationship = "many-to-many"
    )
  
}

nest_reviews <- function(reviews, start_col, end_col) {
  
  tmp <- reviews |> 
    dplyr::select("Unique ref no", .env$start_col, .env$end_col)
  
  tmp_new <- tmp
  
  tmp_new[2, 2] <- paste(tmp[[1, 2]], tmp[2, 2], sep = ".")
  tmp_new[2, 3] <- paste(tmp[[1, 2]], tmp[2, 3], sep = ".")
  
  names(tmp_new)[2:3] <- tmp_new[2, 2:3]
  
  tmp_new <- tmp_new[-(1:2), ]
  
  tmp_new |> 
    dplyr::mutate(
      outcome_recorded = dplyr::if_else(!is.na(dplyr::across(2)), TRUE, FALSE)
    ) |> 
    tidyr::pivot_longer(
      -c(`Unique ref no`, outcome_recorded), 
      names_to = "outcome_info",
      values_to = "outcome_value"
    ) |> 
    dplyr::mutate(
      outcome = stringr::str_extract(outcome_info, ".*(?=\\.)"),
      outcome_info = stringr::str_extract(outcome_info, "(?<=\\.).*")
    ) |> 
    dplyr::group_by(`Unique ref no`, outcome, outcome_recorded) |> 
    tidyr::nest() |> 
    dplyr::ungroup()
  
}
