## code to prepare `my_dataset` dataset goes here

tmp_data <- readRDS("inst/app/data/tmp_data.rds")

evidence_types <- c('primary', 'tertiary', 'grey')

create_sample_data <- function(y){
  
  tmp_data |> 
    dplyr::mutate(`Type of evidence` = y,
                  `Unique ref no` = stringr::str_replace(`Unique ref no`,
                                                         '[:alpha:]',
                                                         substring(y,1,1))
    )
}

my_dataset <- purrr::map(evidence_types, create_sample_data) |> 
  purrr::list_rbind() |> 
  rbind(tmp_data) |> 
  dplyr::select(primary, secondary, tertiary, grey)

usethis::use_data(my_dataset, overwrite = TRUE)
