require(dplyr)
require(readr)
require(lubridate)

read_filter_assessed_df <- function(path_to_assessed_df, factorize_ID = TRUE, drop_cols = TRUE){
  #'Read and filter assessed_df, the output of the QA.
  #'
  #'@description Keep only relevant columns and create ID and IDTYPE fields.
  #'
  #'@param path_to_assessed_df character. The path to the assessed_df.csv
  #'@param factorize_ID bool whether to factorize IDTYPES to corresponding FHS cohorts already.
  #'@param drop_cols bool Whether to drop irrelevant columns.
  #'
  #'@return That filtered dataframe.
  
  
  assessed_df <- readr::read_csv(path_to_assessed_df)
  
  #Extract IDTYPE and ID from MRN
  assessed_df <- assessed_df  %>%
    mutate(IDTYPE = patient_id %/% 10000, ID = patient_id%%10000)
  
  #Drop irrelevant columns
  if(drop_cols){
    assessed_df <- assessed_df %>%
      dplyr::select(-num_valid_series, -FoundImageData, -ExceptionEncountered, -ExceptionMessage, -NumSeriesSelected, 
            -contains('contrast_bolus'), -search_term, -auto_quality_score, -index_input,
            -contains('_nr_of_pixels')) 
  
    #Identify cases with manual edits where box was not ticked (image paths were saved with suffix _edited and dicoms with _new if manually reselected)
    assessed_df <- assessed_df %>%
      mutate(segmentation_edited_yes = (segmentation_edited_yes | grepl('_edited', split_img_path_in_working_dir)), 
             level_selection_edited_yes = (level_selection_edited_yes | grepl('_new', ImageDataLocation)))}
    
  #Turn study date to Date
  assessed_df <- assessed_df %>% mutate(study_date = lubridate::ymd(study_date))
  
  if(factorize_ID){
  #Factorize Cohorts
  assessed_df$IDTYPE <- factor(assessed_df$IDTYPE, levels = c(1, 3, 7, 72), 
                               labels = c('Offspring (Gen 2)', 'Gen 3', "Omni 1", "Omni 2"))}
  
  #Factorize slice thicknesses
  assessed_df$slice_thickness_mm <- as.factor(round(assessed_df$slice_thickness_mm*2)/2)
  
  #Correct order of vertebral levels as factor
  assessed_df$vertebral_level <- factor(assessed_df$vertebral_level, levels=c('T5', 'T8', 'T10', 'L3'))
  
  

  return(assessed_df)
}

get_questions <- function(){
  #'Get questions that were answered during Framingham QA.
  #'
  #'@return A list of questions, with names 'questions', 'exclusion_questions', 'muscle_exclusion_questions', 'sat_exclusion_questions'
questions <- c('iv_contrast',
               'oral_contrast',
               'level_unavailable',
               'segmentation_impossible',
               'lowdose',
               'artifacts',
               'edema',
               'arms_down_unremovable',
               'touching_gantry',
               'reconstruction_filter',
               'phantom_unremovable',
               'segmentation_edited',
               'level_selection_edited', 
               'incomplete_muscle_fov',
               'incomplete_sat_fov')
exclusion_questions <- c('level_unavailable', 
                         'segmentation_impossible',
                         'lowdose',
                         'artifacts',
                         'edema',
                         'arms_down_unremovable',
                         'touching_gantry',
                         'reconstruction_filter',
                         'phantom_unremovable',
                         'incomplete_muscle_fov')#incomplete muscle FOV is here as a temporary solution to facilitate pure muscle analysis. If inspecting SAT in the future, remove this.
muscle_exclusion_questions <- c('incomplete_muscle_fov')
sat_exclusion_questions <- c('incomplete_sat_fov')

 return(list(questions=questions, exclusion_questions=exclusion_questions, muscle_exclusion_questions=muscle_exclusion_questions, sat_exclusion_questions=sat_exclusion_questions))
}


summarize_qa <- function(df, questions, exclusion_questions, muscle_exclusion_questions, sat_exclusion_questions){
  #Report summary of how many boxes were ticked during QA.
  options <- c('_yes', '_no', '_unselected')
  lvls <- c('L3', 'T10', 'T8', 'T5')
  summary_sheet <- data.frame(lvl = lvls)
    question_colnames <- c(paste(questions, options[1], sep=''), paste(questions, options[2], sep=''), paste(questions, options[3], sep= ''))
    
    for(q in question_colnames){
      for(l in lvls){
        summary_sheet[summary_sheet$lvl == l, q] <- sum(df[df$vertebral_level == l, ][[q]])
      }
    }  
    print(summary_sheet)  
  
  
  return(summary_sheet)
}


exclude_rows_based_on_questions <- function(df, questions, exclusion_questions, muscle_exclusion_questions, sat_exclusion_questions, report_summary=TRUE){
  #'Filter dataframe based on questions and exclusion questions.
  #'
  #'@description Filters the assessed_df regarding how questions were answered.
  #'Describes the filtering process in numbers.
  #'For muscle and SAT exclusion only, the relevant numbers will be set to NA.
  #'
  #'@param df data.frame. The dataframe to be filtered based on QA questions.
  #'@param questions chr. The vector with questions that were answered during QA.
  #'@param exclusion_questions chr. The vector with questions that will lead to removal of an entire row from the dataset.
  #'@param muscle_exclusion_questions chr. The vector with questions that will lead to exclusion of only muscle values from that row.
  #'@param sat_exclusion_questions chr. The vector with questions that will lead to exclusion of SAT measures only.
  #'@param report_summary logical. Whether to print a summary of all data.
  print(questions)
  original_df <- df
  options <- c('_yes', '_no', '_unselected')
  prefix = ""
  
  #Report summary of how many boxes were ticked during QA.
  if (report_summary){
    for(question in questions){
      
      for(option in options){
        question_col <- paste(question, option, sep='')
        n_selected <- sum(pull(original_df, question_col))
        
        print(paste('During the QA,', n_selected, 'rows were selected as', question_col))
      }
    }
  }
  
  for(question in questions){
    
    #Find rows, where yes was selected and report on them for original dataframe and the one where data was previously dropped.
    yes_question_col <- paste(question, 'yes', sep='_')
    
    n_dropped_original <- sum(pull(original_df, yes_question_col))
    print(paste('Overall,', n_dropped_original, 'rows were selected during QA as', yes_question_col, '.'))
    
    
    if(question %in% exclusion_questions){
      
      
      #Remove rows where exclusion was selected in yes_question_col
      drop_col <- pull(df, yes_question_col)
      df <- df[!drop_col,]
      n_dropped <- sum(drop_col)
      print(paste(prefix, n_dropped, 'rows were dropped because of', yes_question_col, '.'))
      prefix = "After previous drops, additional"
      
    }
    
    if(question %in% muscle_exclusion_questions){
      drop_col <- pull(df, yes_question_col)
      n_dropped <- sum(drop_col)
      # set muscle measures to NA, if question was answered yes.
      df <- df %>%
        mutate(muscle_area_cm2 = ifelse(.data[[yes_question_col]], NA, muscle_area_cm2), 
               muscle_std_hu = ifelse(.data[[yes_question_col]], NA, muscle_std_hu), 
               muscle_mean_hu = ifelse(.data[[yes_question_col]], NA, muscle_mean_hu), 
               muscle_iqr_hu = ifelse(.data[[yes_question_col]], NA, muscle_iqr_hu), 
               muscle_median_hu = ifelse(.data[[yes_question_col]], NA, muscle_median_hu))
      #Can check that this works using something like (df %>% filter(.data[[yes_question_col]]) %>% select(contains('muscle')))[1,] 
      
      print(paste(prefix, n_dropped, 'muscle entries were dropped because of', yes_question_col, '.'))
      prefix = "After previous drops, additional"
      }
    
    if(question %in% sat_exclusion_questions){
      drop_col <- pull(df, yes_question_col)
      n_dropped <- sum(drop_col)
      # set muscle measures to NA, if question was answered yes.
      df <- df %>%
        mutate(subcutaneous_fat_area_cm2 = ifelse(.data[[yes_question_col]], NA, subcutaneous_fat_area_cm2), 
               subcutaneous_fat_std_hu = ifelse(.data[[yes_question_col]], NA, subcutaneous_fat_std_hu), 
               subcutaneous_fat_mean_hu = ifelse(.data[[yes_question_col]], NA, subcutaneous_fat_mean_hu), 
               subcutaneous_fat_iqr_hu = ifelse(.data[[yes_question_col]], NA, subcutaneous_fat_iqr_hu), 
               subcutaneous_fat_median_hu = ifelse(.data[[yes_question_col]], NA, subcutaneous_fat_median_hu))
      #Can check that this works using something like (df %>% filter(.data[[yes_question_col]]) %>% select(contains('muscle')))[1,] 
      
      print(paste(prefix, n_dropped, 'SAT entries were dropped because of', yes_question_col, '.'))
      prefix = "After previous drops, additional"
    }
    
  }
  
  #Make sure that only scans with slice thickness of 2.5mm for the thorax and 5mm for the abdomen are chosen.
  #Otherwise, assume that no suitable scan could be found within the selection.
  df <- df %>% 
    filter(
      (vertebral_level == 'L3' & slice_thickness_mm == '5') | 
        (vertebral_level %in% c('T5', 'T8', 'T10') & slice_thickness_mm == '2.5')
    )
  return(df)
}



reduce_to_one_study_per_participant <- function(df, prefer_CT=2){
  #' After QA-related exclusion, the dataframe may still hold more than one row 
  #' of data per participant and vertebral level.
  #' This function decides on only one row of data per vertebral level and participant,
  #' preferring rows with measurements taken during prefer_CT
  #' 
  #' @param df data.frame The dataframe.
  #' @param prefer_CT numeric Which CT timepoint should be preferred.
  
  #Which participants have more than 1 row of data per vertebral level:
  df_temp <- df %>% left_join(df %>% group_by(ID, IDTYPE, vertebral_level) %>%
    count() %>% dplyr::rename(n_rows_lvl = n),
    by=c('ID', 'IDTYPE', 'vertebral_level'))
  
  #How many rows are affected:
  print('Participants had n rows per vertebral level: (e.g., n_rows_level == 2 means that n participants had 2 measurements available for that vertebral level')
  print(df_temp %>% group_by(n_rows_lvl, vertebral_level) %>% count() %>%
    mutate(n = n/n_rows_lvl))
  print('The amount of measurements dropped on each vertebral level equals n in the row where n_rows_level == 2')
  
  #In cases with 2 measurements, prefer CT number 'prefer_CT':
  df_temp <- df_temp %>% filter(n_rows_lvl == 1 | #Keep, if only one measurement available for vertebral level and participant, OR
                      (CT_number == prefer_CT) ) #if CT number is equal to the preferred CT (CT number 2)
  
  #Make sure that there is now only one measurement per participant and vertebral level:
  n_measurements_per_part_and_lvl <- (df_temp %>% group_by(vertebral_level, ID, IDTYPE) %>%
    count())$n
  print(paste(sum(n_measurements_per_part_and_lvl > 1), 'participants still have more than one measurement per vertebral level. Anything but 0 is reason to worry!'))
  if(sum(n_measurements_per_part_and_lvl > 1)!=0){
    print('Unacceptable. Returning NULL')
    return(NULL)
  }
  
  return(df_temp %>% dplyr::select(-n_rows_lvl))
}



