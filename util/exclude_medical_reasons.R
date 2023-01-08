require(dplyr)


exclude_medical_reasons <- function(df, exclusion_criteria = c('exclude_cancer')){
  #'Read and filter a dataframe based on medical questions
  #'
  #'@description Discard patients based on medical characteristics and describe the process.
  #'
  #'@param df data.frame The dataframe to be cleaned.
  #'@param exclusion_criteria The columns based on which conditions should be excluded.
  #'
  #'@return That filtered dataframe.
  
  
  
  #Cancer: create column 'exclude_cancer'
  df <- exclude_participants_with_cancer_hx(df, exclude_nonmalignant_nonmelanomous_skincancer = FALSE)
  
  #define standard exclusion criteria, if no others are delivered as arguments
  if(length(exclusion_criteria) == 0){
  exclusion_criteria <- c('exclude_cancer',
                          'Diabetes',
                          'Thyroid_Abnormality','Gallbladder_Disease',
                          'Parkinson_meds','Heart_Valve_Surgery',
                          'Is_Pregnant','Abdominal_Aorta_Surgery',
                          'Coronary_Bypass_Surgery','Thoracic_Aorta_Surgery',
                          'Femoral_Lower_Extremity_Surgery','CHF_Congestive_Heart_Failure',
                          'Heart_Failure_Hospitalization','Myocardial_Infarction',
                          'Pulmonary_Embolus', 'Parkinson',
                          'Lung_Fibrosis')}
  
  
  
  excluded_df <- df
  exclusion_summary <- NA
  
  #Exclude all cases
  for(exclusion_criterion in exclusion_criteria){
    
    #Exclude patients, describing the numbers of exclusions
    exclusion_results <- exclude_based_on_column(excluded_df, exclusion_criterion)
    excluded_df <- exclusion_results[['df_after_exclusion']]
    
    #Merge all exclusion summaries into one big frame.
    if(is.na(exclusion_summary)){ #Initialize upon first iteration
      exclusion_summary <- exclusion_results[['excl_summary']]
      rename_later <- exclusion_criterion
        
    }
    else{ #And merge with newly added exclusion summaries
      exclusion_summary <- exclusion_summary %>% left_join(exclusion_results[['excl_summary']], by=c('vertebral_level'), suffix=c('', paste('_', exclusion_criterion, sep=''))) 
    }
  }
  
  #Rename also the columns of the first exclusion summary.
  col_names <- colnames(exclusion_summary)
  col_names[col_names == 'n_studies_dropped'] <- paste('n_studies_dropped', rename_later, sep='_')
  col_names[col_names == 'unique_patients_excluded'] <- paste('unique_patients_excluded', rename_later, sep='_')
  colnames(exclusion_summary) <- col_names
  
  #Summarize entire exclusion
  
  n_patients_pre_exclusion <- length(unique(df$patient_id))
  n_patients_post_exclusion <- length(unique(excluded_df$patient_id))
  n_patients_dropped <- n_patients_pre_exclusion - n_patients_post_exclusion
  n_scans_dropped <- nrow(df) - nrow(excluded_df)
  
  scans_dropped_per_vert_level <- df %>% group_by(vertebral_level) %>% count() %>%
    left_join((excluded_df %>%
                group_by(vertebral_level) %>%
                count()), by =c('vertebral_level'), suffix =c('_pre', '_post')) %>% mutate(n_scans_dropped = n_pre-n_post)
  
  print(exclusion_summary)
  print(scans_dropped_per_vert_level)
  print(paste(n_patients_dropped, 'patients and', n_scans_dropped  ,'measurements from any vertebral level were dropped due to medical reasons.'))
  
  
  return(excluded_df)
}

exclude_based_on_column <- function(df, colname){
  #' Exclude patients based on columns and give meaningful information aboout the process.
  #' 
  #' @param df data.frame The dataframe
  #'@param colname The name of the column with exclusion reasons.
  #'
  #'@return A list of the dataframe after exclusion, together with an exclusion summary.
  
  #Get column corresponding to the condition
  column <- df[[colname]]
  
  #Calculate, how many will be excluded.
  n_excluded <- sum(column)
  
  #Exclude, where the condition is TRUE.
  excluded_df <- df[!column, ] #These will be excluded
  exclusion_df <- df[column, ] #This is the dataframe after exclusion
  
  #Summarize the exclusion, by listing how many studies were dropped and how many participants
  exclusion_summary <- exclusion_df %>% group_by(vertebral_level) %>% count()  %>%
    left_join(exclusion_df %>% group_by(vertebral_level) %>% distinct(ID, IDTYPE) %>% count() %>% dplyr::rename(unique_patients_excluded = n)) %>%
    dplyr::rename(n_studies_dropped = n)
  
  n_excluded_patients <- length(unique(exclusion_df$patient_id))
  
  print(paste(n_excluded, 'measurements from any vertebral level from', n_excluded_patients, 'patients were excluded due to having', colname))
  print(exclusion_summary)
  return(list(df_after_exclusion = excluded_df, excl_summary = exclusion_summary))
}


exclude_participants_with_cancer_hx <- function(df,
                                                exclude_nonmalignant_nonmelanomous_skincancer = FALSE
                                        
){
  #' Exclude Participants with any cancer history prior to CT, except certain types.
  #' 
  #' @param df The dataframe
  #' @param exclude_nonmalignant_nonmelanomous_skincancer Whether to also exclude non-malignant, non-melanomous skin cancer.
  #' 
  
  
  get_excludable_cancer <- function(row, exclude_nonmalignant_nonmelanomous_skincancer=F){
    #'Identify if any cancer, that should be excluded, can be found in the row of a dataframe.
    
    cancer_list <- row[['cancer_data']]
    #previously stored as date, will now be converted into number of date as stored in date format.
    study_date <- as.numeric(row[['study_date']])
    
    # Loop over all cancers this patient might have had.
    for (i in 1:7){ #7 was the max. amount of cancers any participant had
      #Extract date, topography and behavior for potential cancer
      cancer_date <- as.numeric(as.Date(cancer_list[[paste('confirmed_cancer_date', i, sep='_')]]))
      cancer_topography <- cancer_list[[paste('topography_confirmed_cancer', i, sep='_')]]
      cancer_behavior <- cancer_list[[paste('behavior_confirmed_cancer', i, sep='_')]]
      cancer_histology <- cancer_list[[paste('histology_confirmed_cancer', i, sep='_')]]
      
      
      #If participant did not have further cancers, return False
      if(any(is.null(cancer_date), is.null(cancer_topography), is.null(cancer_behavior))){
        return(FALSE)
      }
      if(any(is.na(cancer_date), is.na(cancer_topography), is.na(cancer_behavior))){
        return(FALSE)
      }
      
      is_exception <- FALSE
      
      #Check if cancers are non-malignant, non-melanomous skin cancers
      if(!exclude_nonmalignant_nonmelanomous_skincancer){
        topo <- 'skin (excludes skin of labia majora, skin of vulva, skin of penis and skin of scrotum)'
                                               
        if(cancer_topography == topo & 
           !(cancer_behavior == 'malignant, primary site') &
           !(as.numeric(cancer_histology) < 8720 | as.numeric(cancer_histology) > 8790)){
          is_exception <- TRUE
        }
      }
      
    
      #Assume that cancer should be excluded, if diagnosed before CT and no exception made.
      if(!is_exception & study_date >= cancer_date){
        return(TRUE)
      }
    }
    #If none of the cancers caused exclusion, return False
    return(FALSE)
    
  }
  
  df$exclude_cancer <- apply(df[,c('cancer_data', 'study_date')], 1, get_excludable_cancer, exclude_nonmalignant_nonmelanomous_skincancer)
  return(df)
}


exclude_cancer_within_n_days_of_CT <- function(df, n=90, no_exclusion_cancer_types=c(), exclude_benign = TRUE){
  #' Exclude participants based on cancer within n days, filtering for specific unimportant cancers.
  #' 
  #'@param df data.frame The dataframe to filter.
  #'@param n integer The number of days surrounding an exam, within which a cancer diagnosis leads to exclusion.
  #'@param no_exclusion_cancer_types factor Types of cancers that will not lead to exclusion.
  #'@param exclude_benign Whether benign tumors will also lead to exclusion.
  #'
  #'@return The dataframe with additional cancer_exclusion flag.
  
  get_excludable_cancer <- function(row){
   
    cancer_list <- row[['cancer_data']]
    study_date <- as.numeric(row[['study_date']])
    
    # Loop over all cancers this patient might have had.
    for (i in 1:7){
      #Extract date, topography and behavior for potential cancer
      cancer_date <- as.numeric(as.Date(cancer_list[[paste('confirmed_cancer_date', i, sep='_')]]))
      cancer_topography <- cancer_list[[paste('topography_confirmed_cancer', i, sep='_')]]
      cancer_behavior <- cancer_list[[paste('behavior_confirmed_cancer', i, sep='_')]]
      
      
      
      #If patient did not have further cancers, return False
      if(any(is.null(cancer_date), is.null(cancer_topography), is.null(cancer_behavior))){
        return(FALSE)
      }
      if(any(is.na(cancer_date), is.na(cancer_topography), is.na(cancer_behavior))){
        return(FALSE)
      }
     
      
      #If benigns should not be included, and behavior is benign, skip next step.
      if(exclude_benign | cancer_behavior != 'benign'){
        #Check if cancer occured within timeframe of n days
        if(abs(cancer_date-study_date) < n){
          #Check if cancer was on list that should not be excluded
          if(!(cancer_topography %in% no_exclusion_cancer_types)){
            return(TRUE)
          }
        }
      }
    }
    #Return False, if no cancer led to exclusion
    return(FALSE)

    
  }
  
  df$exclude_cancer <- apply(df[,c('cancer_data', 'study_date')], 1, get_excludable_cancer)
  return(df)
}


