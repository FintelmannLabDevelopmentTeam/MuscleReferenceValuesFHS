require(haven)
require(tidyverse)
source('util/util.R')


add_stroke_tia_data <- function(df, 
                             stroke_data_path,
                             use_types =F,
                             remove_intermediary_columns = T
){
  #' Add stroke or TIA data to a dataframe, if the event happened before the CT scan.
  #' 
  #' @param df data.frame The dataframe to which to add the stroke/tia data
  #'@param use_types logical Whether to use the detailed labels of the stroke/TIA events. If false, just keep as binary category.
  #'@param stroke_data_path character The path to the stroke/tia datasheet.
  #'@param remove_intermediary_columns Whether to remove intermediary columns created to identify events pre CT.
  #'
  #'@return The datasheet with the added stroke/TIA data.
  stroke_outcomes <- haven::read_sas(stroke_data_path) %>%
    dplyr::rename(ID = id, IDTYPE = idtype) %>%
    dplyr::select(-sr_LAC_ABI)
  stroke_outcomes <- factorize_IDTYPE(stroke_outcomes)
  
  stroke_outcomes$stroketiadate <- as.Date(stroke_outcomes$stroketiadate)
  #either date of stroke/tia or censoring (last check without stroke)
  
    stroke_factor_labels <- 
      c(
        'Definite CVA at exam 1, but questionable type',
        'ABI (Atherothrombotic Infarction of brain)',
        'TIA (Transient Ischemic Attack)',
        'Cerebral embolism',
        'Intracerebral hemorrhage',
        'Subarachnoid hemorrhage',
        'Other CVA',
        'CVA, definite CVA, type unknown',
        'TIA with positive imaging',
        'Questionable CVA at exam 1'
        )
  
  
  stroke_outcomes$stroketia_type <-
    factor(
      stroke_outcomes$stroketia_type,
      labels=stroke_factor_labels
    )
  freq <- stroke_outcomes %>% group_by(stroketia_type) %>% count()
  #Store which factors we have for later:
  #see what max. number of strokes is per patient
  stroke_outcomes %>% group_by(ID, IDTYPE) %>% count() %>% arrange(desc(n))
  #max 1, so no need to widen
  
  
  
  #Now join the dataframes and combine all these datapoints into one column per participant.
  df_w_stroke_events <- df %>% left_join(stroke_outcomes, by= c('ID', 'IDTYPE'))
  
  study_dates <- df_w_stroke_events$study_date
  
  #Keep only those stroke and TIA events before CT date
  df_w_stroke_events$STROKE_TIA_Hx <- 
    df_w_stroke_events$stroke_tia & 
    df_w_stroke_events$stroketiadate < study_dates
  #NA the types for non kept stroke/tias
  df_w_stroke_events[!df_w_stroke_events$STROKE_TIA_Hx, 'stroketia_type'] <- NA
  
  label(df_w_stroke_events$STROKE_TIA_Hx) <- 'Added through stroke tia outcome sheet. See Add_Stroke_TIA_data.R'
  label(df_w_stroke_events$stroketia_type) <- 'Added through stroke tia outcome sheet. See Add_Stroke_TIA_data.R'
  label(df_w_stroke_events$stroketiadate) <- 'Added through stroke tia outcome sheet. See Add_Stroke_TIA_data.R'
  
  df_w_stroke_events <- df_w_stroke_events %>%
    dplyr::select(-stroke_tia)
  
  return(df_w_stroke_events)
}

