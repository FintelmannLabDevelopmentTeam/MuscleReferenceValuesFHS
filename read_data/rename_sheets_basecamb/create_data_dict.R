require(haven)
require(dplyr)

paths <-
  readr::read_csv('read_data/rename_sheets_basecamb/paths.csv') %>%
  dplyr::pull(path, key)

sheets <- c(
  'offspringexam9omni1exam4',
  'gen3omni2exam3',
  'omni1exam3'
)

for(sheet in sheets){
  filepath <- paths[[sheet]]
  data <- haven::read_sas(filepath, NULL)
  
  cols <- colnames(data)
  
  data_dict <- data.frame(old_column_name = cols, new_data_type = '', new_column_name = '', coding = '')
  dd_path <- paste('read_data/rename_sheets_basecamb/dd_', sheet, '.csv', sep='')
  
  write.csv(data_dict, dd_path)
  }
