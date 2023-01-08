require(readr)
require(haven)
require(dplyr)
require(basecamb)

read_sas_and_apply_data_dict <- function(sas_path, csv_name){
  #'Read SAS sheet and apply data dictionary specified in csv to it.
  #'
  #'@param sas_path character. The path to the SAS sheet.
  #'@param csv_name character. The name of the data dictionary csv for renaming.
  #'
  #'@return The renamed datasheet from the SAS file.
  df <- haven::read_sas(sas_path, NULL)
  rename_sheet <- readr::read_csv(paste('read_data/rename_sheets_basecamb/', csv_name, sep=''))
  return(basecamb::apply_data_dictionary(df, rename_sheet))
  
}