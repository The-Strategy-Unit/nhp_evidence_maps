## code to prepare `my_dataset` dataset goes here

filepath <- "data-raw/first_release_data_v1.xlsx"

# Studies ----
studies <- readxl::read_xlsx(filepath,
                             sheet = "Studies") |> 
  dplyr::rename("Link" = `Link to full text`,
                "Setting" = `...19`,
                "Effect" = `Effect\r\n(statistical rather than clinical significance?)`) |> 
  dplyr::select(`Unique ref no`,
                `Authors`,
                `Publication year`,
                `Title`,
                `Journal`,
                `Abstract`,
                `Country of study`,
                `Link`,
                `Type of evidence`,
                `Study design`,
                `Mechanism`,
                `Population`,
                `Setting`,
                38:47,
                 `Effect`) 

studies_outcome_idxs <- tibble::tibble(start_col = seq(which(colnames(studies) == "Outcomes reported"),
                                                    which(colnames(studies) == "Outcomes reported") + 9,
                                                    2),
                                    end_col = seq(which(colnames(studies) == "Outcomes reported") + 1,
                                                  which(colnames(studies) == "Outcomes reported") + 9,
                                                  2)) 

fn_nest_studies <- function(start_col, end_col){
  
tmp <- studies |> 
  dplyr::select(`Unique ref no`, start_col, end_col)

tmp_new <- tmp

tmp_new[2,2] <- paste(tmp[[1,2]], tmp[2,2], sep = ".")
tmp_new[2,3] <- paste(tmp[[1,2]], tmp[2,3], sep = ".")

names(tmp_new)[2:3] <- tmp_new[2,2:3]

tmp_new <- tmp_new[-(1:2),]

tmp_new |> dplyr::mutate(outcome_recorded = ifelse(!is.na(tmp_new[[2]]), T, F)) |> 
  tidyr::pivot_longer(-c(`Unique ref no`, outcome_recorded), 
                        names_to = "outcome_info",
                        values_to = "outcome_value") |> 
  dplyr::mutate(outcome = stringr::str_extract(outcome_info,
                                               ".*(?=\\.)"),
                outcome_info = stringr::str_extract(outcome_info,
                                                    "(?<=\\.).*")) |> 
  dplyr::group_by(`Unique ref no`, outcome, outcome_recorded) |> 
  tidyr::nest() |> 
  dplyr::ungroup()
 
}

nested_studies <- purrr::map2(studies_outcome_idxs$start_col,
                               studies_outcome_idxs$end_col,
                               fn_nest_studies) |> 
  purrr::list_rbind() |> 
  dplyr::filter(!is.na(`Unique ref no`))


studies_final <- studies |> 
  dplyr::filter(!is.na(`Unique ref no`) &
                  !is.na(`Authors`)) |> 
  dplyr::select(-c(`Outcomes reported`:`...47`)) |> 
  dplyr::left_join(nested_studies, 
                   by = "Unique ref no",
                   relationship = "many-to-many") #|> 
  #dplyr::mutate(evidence_category = "study")
  
# Reviews ---- 

reviews <- readxl::read_xlsx(filepath,
                             sheet = "Reviews") |> 
  dplyr::rename("Link" = `Link to full text`,
                "Setting" = `...19`,
                "Effect" = `Effect\r\n(statistical rather than clinical significance?)`) |> 
  dplyr::select(`Unique ref no`,
                `Authors`,
                `Publication year`,
                `Title`,
                `Journal`,
                `Abstract`,
                `Country of study`,
                `Link`,
                `Type of evidence`,
                `Study design`,
                `Mechanism`,
                `Population`,
                `Setting`,
                37:46,
                `Effect`)

reviews_outcome_idxs <- tibble::tibble(start_col = seq(which(colnames(reviews) == "Outcomes reported"),
                                                       which(colnames(reviews) == "Outcomes reported") + 9,
                                                       2),
                                       end_col = seq(which(colnames(reviews) == "Outcomes reported") + 1,
                                                     which(colnames(reviews) == "Outcomes reported") + 9,
                                                     2)) 

fn_nest_reviews <- function(start_col, end_col){
  
  tmp <- reviews |> 
    dplyr::select(`Unique ref no`, start_col, end_col)
  
  tmp_new <- tmp
  
  tmp_new[2,2] <- paste(tmp[[1,2]], tmp[2,2], sep = ".")
  tmp_new[2,3] <- paste(tmp[[1,2]], tmp[2,3], sep = ".")
  
  names(tmp_new)[2:3] <- tmp_new[2,2:3]
  
  tmp_new <- tmp_new[-(1:2),]
  
  tmp_new |> dplyr::mutate(outcome_recorded = ifelse(!is.na(dplyr::across(2)), T, F)) |> 
    tidyr::pivot_longer(-c(`Unique ref no`, outcome_recorded), 
                        names_to = "outcome_info",
                        values_to = "outcome_value") |> 
    dplyr::mutate(outcome = stringr::str_extract(outcome_info,
                                                 ".*(?=\\.)"),
                  outcome_info = stringr::str_extract(outcome_info,
                                                      "(?<=\\.).*")) |> 
    dplyr::group_by(`Unique ref no`, outcome, outcome_recorded) |> 
    tidyr::nest() |> 
    dplyr::ungroup()
  
}

nested_reviews <- purrr::map2(reviews_outcome_idxs$start_col,
                              reviews_outcome_idxs$end_col,
                              fn_nest_reviews) |> 
  purrr::list_rbind() |> 
  dplyr::filter(!is.na(`Unique ref no`))

reviews_final <- reviews |> 
  dplyr::filter(!is.na(`Unique ref no`) &
                  !is.na(`Authors`)) |> 
  dplyr::select(-c(`Outcomes reported`:`...46`)) |> 
  dplyr::left_join(nested_studies, 
                   by = "Unique ref no",
                   relationship = "many-to-many") #|> 
  #dplyr::mutate(evidence_category = "review")

my_dataset <- dplyr::bind_rows(studies_final,
                        reviews_final) |> 
  dplyr::mutate(`Type of evidence` = stringr::str_to_title(`Type of evidence`)) |> 
  dplyr::mutate(
    id = dplyr::row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")) |> 
  dplyr::rename(typeOfEvidence = `Type of evidence`#,
                #"Study design" = evidence_category
                )

  

usethis::use_data(my_dataset, overwrite = TRUE)
