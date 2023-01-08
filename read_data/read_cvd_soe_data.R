require(haven)
require(tidyverse)
source('util/util.R')

add_cvd_soe_data <- function(df, 
                             cvd_data_path,
                             use_full_labels =F,
                             remove_intermediary_columns = T,
                             simplify = T
                             ){
  #' Add cardiovascular disease series of event data to a dataframe, if the event happened before the CT scan.
  #' 
  #'@param df data.frame The dataframe to which to add the CVD SOE data
  #'@param cvd_data_path character The path to the CVD SOE datasheet.
  #'@param use_full_labels logical Whether to use the detailed labels of the CVD events. If false, groups them into categories.
  #'@param remove_intermediary_columns Whether to remove intermediary columns created to identify events pre CT.
  #'@param simplify logical Whether events should be combined into one column each only if happened before CT.
  #'
  #'@return The datasheet with the added CVD SOE data.
  soe_cv_outcomes <- haven::read_sas(cvd_data_path)
  soe_cv_outcomes <- factorize_IDTYPE(soe_cv_outcomes)
  
  soe_cv_outcomes$DATE <- as.Date(soe_cv_outcomes$DATE)
  
  if(use_full_labels){
    cv_factor_labels <- 
      c(
        'MI recognized, with diagnostic ECG',
        'MI recognized, without diagnostic ECG, with enzymes and history',
        'MI recognized, without diagnostic ECG, with autopsy evidence, new 85
event (see also code 9)',
        'MI unrecognized, silent',
        'MI unrecognized, not silent',
        'AP, first episode only',
        'CI, definite by both history and ECG',
        'Questionable MI at exam 1',
        'Acute MI by autopsy, previously coded as 1 or 2',
        'Definite CVA at exam 1, but questionable type',
        'ABI (Atherothrombotic Infarction of brain)',
        'TIA (Transient Ischemic Attack) only the 1st TIA is coded',
        'Cerebral embolism',
        'Intracerebral hemorrhage',
        'Subarachnoid hemorrhage',
        'Other CVA',
        'CVA, definite CVA, type unknown',
        'TIA with positive imaging',
        'Questionable CVA at exam 1',
        'Death, CHD sudden, within 1 hour',
        'Death, CHD, 1-23 hours, non sudden',
        'Death, CHD, 24-47 hours, non sudden',
        'Death, CHD, 48 hours or more, non sudden',
        'Death, CVA',
        'Death, other CVD',
        'Death, Cancer',
        'Death, other causes',
        'Death, cause unknown',
        'IC, first episode only',
        'IC, questionable IC at exam 1',
        'CHF, not hospitalized, diagnosed on basis of on exam or MD notes',
        'CHF, hospitalized',
        'CHF, questionable CHF at exam 1'
      )
  }else{
    cv_factor_labels <-
      c(
        'Myocardial_Infarction_Hx',
        'Myocardial_Infarction_Hx',
        'Myocardial_Infarction_Hx',
        'Myocardial_Infarction_Hx',
        'Myocardial_Infarction_Hx',
        'Angina_Pectoris_Hx',
        'Coronary_Insufficiency',
        'Myocardial_Infarction_Hx',
        'Myocardial_Infarction_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Stroke_TIA_CVA_ABI_Intracranial_Hemorrhage_Hx',
        'Death',
        'Death',
        'Death',
        'Death',
        'Death',
        'Death',
        'Death',
        'Death',
        'Death',
        'IC_Intermittent_Claudication_Hx',
        'IC_Intermittent_Claudication_Hx',
        'CHF_Congestive_Heart_Failure_Hx',
        'CHF_Congestive_Heart_Failure_Hx',
        'CHF_Congestive_Heart_Failure_Hx'
      )
  }
  
  soe_cv_outcomes$EVENT <-
    factor(
      soe_cv_outcomes$EVENT,
      labels=cv_factor_labels
    )
  freq <- soe_cv_outcomes %>% group_by(EVENT) %>% count()
  #Store which factors we have for later:
  new_columns <- unique(cv_factor_labels)
  #see what max. number of cvds is per patient
  soe_cv_outcomes %>% group_by(ID, IDTYPE) %>% count() %>% arrange(desc(n))
  #one participant had 25(!) Cardiovascular events
  
  #Create widened dataframe within dataframe
  soe_cv_outcomes <- soe_cv_outcomes %>% group_by(ID, IDTYPE) %>% 
    summarize(
      CVD_event = paste(EVENT, collapse = "---"),
      CVD_event_date = paste(DATE, collapse = '---')
              ) %>%
    separate(
      CVD_event,
      sep = '---',
      into = get_numbered_vector('CVD_event', 25)
    ) %>%
    separate(
      CVD_event_date,
      sep='---',
      into=get_numbered_vector('CVD_event_date', 25)
    )
  
  #Now join the dataframes and combine all these datapoints into one column per participant.
  df_w_cvd_events <- df %>% left_join(soe_cv_outcomes, by= c('ID', 'IDTYPE'))
  
  #If all events should be returned without simplifying, we are done here.
  if(!simplify){
    return(df_w_cvd_events)
  }
  
  study_dates <- df_w_cvd_events$study_date
  
  #Initialize outcome columns as False
  for(column in new_columns){
    df_w_cvd_events[,column] <- FALSE
    label(df_w_cvd_events[[column]]) <- 'Added through CVD SOE sheet. see read_cvd_soe_data.R'
    }
  
  
  for(i in 1:25){
    event_colname <- paste('CVD_event', i, sep='_')
    event_col <- df_w_cvd_events[[event_colname]]
    event_date_colname <- paste('CVD_event_date', i, sep='_')
    event_date_col <- df_w_cvd_events[[event_date_colname]]
    
    #Add values to summarizing cvd event columns, if they occured to a participant at any point before the CT scan.
    
  
    for(column in new_columns){
      df_w_cvd_events[
        !(is.na(event_col) | is.na(event_date_col)) & #If data available
        (study_dates > event_date_col) & #if event happened before CT
          (event_col == column), #If event is the one that belongs in the event column
        column
      ] <- TRUE #set the event column to true
    }
    #a good example is participant 39 in the dataframe
  }
  
  #Drop the intermediary cvd event and event date columns
  if(remove_intermediary_columns){
    df_w_cvd_events <- df_w_cvd_events %>% dplyr::select(-contains('CVD_event_'))
  }
  
  return(df_w_cvd_events)
}

