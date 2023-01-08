require(tidyverse)
require(haven)
source('util/util.R')


add_nearest_labs <- function(df,
                             labs_data_path,
                             labs2_data_path)
{
  #' Add the lab values to the dataframe from the blood draw closest to CT examination.
  #'
  #' @param df data.frame The dataframe to which to add the values.
  #' @param labs_data_path character The path to the first labs file.
  #' @param labs2_data_path character The path to the second labs file.
  #'
  #'@return The dataframe with lab values.
  
  labs1 <- haven::read_sas(labs_data_path, NULL)
  labs1 <- factorize_IDTYPE(labs1)
  
  labs2 <- haven::read_sas(labs2_data_path, NULL)
  labs2 <- factorize_IDTYPE(labs2)
  
  setdiff(colnames(labs1), colnames(labs2))
  #drop these to enable rbind
  c <- intersect(colnames(labs1), colnames(labs2))
  
  labs <- rbind(labs1[, c], labs2[, c])
  
  #Apparently only one entry per patient
  labs %>% distinct(ID, IDTYPE) %>% count()
  
  #And data only available for Offspring, Gen3, Omni 1, but not Omni 2
  labs %>% group_by(IDTYPE) %>%
    count()
  
  #Additional data exists in exam sheets.
  #Will have NAs. Using left_join.
  df_w_labs <- df %>% left_join(labs, by = c('ID', 'IDTYPE'))
  
  return(df_w_labs)
}
