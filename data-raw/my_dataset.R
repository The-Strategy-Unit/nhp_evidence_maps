## code to prepare `my_dataset` dataset goes here

tmp_data <- readRDS("inst/app/data/tmp_data.rds")

evidence_types <- c('primary', 'tertiary', 'grey')
                                 

create_sample_data <- function(x){
  
  tmp_data |> 
    dplyr::mutate(`Type of evidence` = x,
                  `Unique ref no` = stringr::str_replace(`Unique ref no`,
                                                         '[:alpha:]',
                                                         substring(x,1,1)),
                  Mechanism = rep(c('?', 'Avoid', 'Substitute'), 
                                  length.out = nrow(tmp_data)),
                  Citation = paste('sample_data', `Unique ref no`, Citation)
    )
}

my_dataset <- purrr::map(evidence_types, create_sample_data) |> 
  purrr::list_rbind() |> 
  rbind(tmp_data) 

usethis::use_data(my_dataset, overwrite = TRUE)
