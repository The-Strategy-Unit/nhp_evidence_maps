

syntheses <- readxl::read_xlsx('inst/app/data/230808 test map.xlsx', 
                               sheet = 'Syntheses') |> 
  dplyr::select(1:9, 42) |> 
  dplyr::rename('Effect' = 10,
                'Link' = 6) |> 
  dplyr::filter(stringr::str_detect(`Unique ref no`, '[:number:]'))


studies <- readxl::read_xlsx('inst/app/data/230808 test map.xlsx', 
                             sheet = 'Studies') |> 
  dplyr::select(1:9, 41) |> 
  dplyr::rename('Effect' = 10) |>
  dplyr::filter(stringr::str_detect(`Unique ref no`, '[:number:]'))


#saveRDS(syntheses, 'inst/app/data/tmp_data.rds')




