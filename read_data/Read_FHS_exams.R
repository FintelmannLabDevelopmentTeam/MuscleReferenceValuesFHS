#' Read Framingham Exam Data
#'
#'
require(tidyverse)
source('read_data/read_sas_and_apply_data_dict.R')
source('util/util.R')



read_fhs_cancer <- function(factorize_ID = TRUE) {
  #'Read in FHS cancer data.
  #'
  #'@description Reads the FHS cancer data sheet (confirmed dates, topography, grades, histo, behavior, ... from EMR).
  #'Factorizes data.
  #'Recombines data such that there will be 7 rows per cancer topography, behavior, and date, instead of multiple rows per patient.
  #'Nests cancer data.
  #'
  #'@param factorize_ID logical Whether the IDTYPE should be factorized.
  #'
  #'@return The tidied cancer datasheet.
  
  #read cancer data
  paths <-
    readr::read_csv('read_data/rename_sheets_basecamb/paths.csv') %>%
    dplyr::pull(path, key)
  cancer_pure <-
    read_sas_and_apply_data_dict(paths[['cancer']], 'dd_cancer.csv')
  
  #Label factor data
  cancer_pure$topography_confirmed_cancer <-
    factor(
      cancer_pure$topography_confirmed_cancer,
      labels = c(
        'tongue',
        'major salivary gland',
        'gum',
        'floor of mouth',
        'other mouth parts',
        'oropharynx',
        'nasopharynx',
        'hypopharynx',
        'pharynx and ill-defined sites in lip, oral cavity and pharynx',
        'esophagus',
        'stomach',
        'small intestine',
        'colon',
        'rectum, rectosigmoid junction, anal canal and anus, not otherwise specified',
        'liver and intrahepatic bile ducts',
        'gallblader and extrahepatic bile ducts',
        'pancreas',
        'retroperitoneum and peritoneum',
        'other and ill defined sites within digestive organs and peritoneum',
        'nasal cavities, accessory sinuses, middle ear and inner ear',
        'larynx',
        'trachea bronchus and lung',
        'pleura',
        'thymus heart mediastinum',
        'other and ill defined sites within respiratory system and intrathoracic organs',
        'hematopoietic and reticuloendothelial system',
        'bones, joints and articular cartilage',
        'connective, subcutaneous and other soft tissues',
        'skin (excludes skin of labia majora, skin of vulva, skin of penis and skin of scrotum)',
        'female breast (excludes skin of breast)',
        'male breast (excludes skin of breast)',
        'uterus, not otherwise specified',
        'cervix uteri',
        'corpus uteri or endometrium',
        'ovary, fallopian tube and broad ligament',
        'other and unspecified female genital organs',
        'prostate gland',
        'testis',
        'penis and other male genital organs',
        'urinary bladder',
        'kidney and other urinary organs',
        'eye and lacrimal gland',
        'brain',
        'other and unspecified parts of the nervous systems (excludes peripheral nerves, sympathetic and parasympathetic nerves and ganglia)',
        'thyroid gland',
        'other endocrine glands',
        'lymph nodes',
        'unknown primary site'
      )
    )
  if (factorize_ID) {
    cancer_pure$IDTYPE <-
      factor(
        cancer_pure$IDTYPE,
        levels = c(1, 3, 7, 72),
        labels = c('Offspring (Gen 2)', 'Gen 3', "Omni 1", "Omni 2")
      )
  }
  
  
  cancer_pure$diagnosis_type_confirmed_cancer <-
    factor(
      cancer_pure$diagnosis_type_confirmed_cancer,
      labels = c(
        'microscopically diagnosed',
        'clinically diagnosed',
        'death certificate only'
      )
    )
  
  
  cancer_pure$behavior_confirmed_cancer <-
    factor(
      cancer_pure$behavior_confirmed_cancer,
      labels = c('carcinoma in situ, non-invasive', 'malignant, primary site')
    )
  
  
  cancer_pure$grade_confirmed_cancer <-
    factor(
      cancer_pure$grade_confirmed_cancer,
      labels = c(
        'Grade 1',
        'Grade 2',
        'Grade 3',
        'Grade 4',
        'not determined, stated, or applicable'
      )
    )
  
  #Reduce cancer data to one row per patient, separating several entries per patient with ---
  cancer <-
    cancer_pure %>% group_by(ID, IDTYPE) %>% summarize(
      confirmed_cancer_date = paste(confirmed_cancer_date, collapse = "---"),
      diagnosis_type_confirmed_cancer = paste(diagnosis_type_confirmed_cancer, collapse =
                                                '---'),
      topography_confirmed_cancer =
        paste(topography_confirmed_cancer, collapse = '---'),
      behavior_confirmed_cancer =
        paste(behavior_confirmed_cancer, collapse = '---'),
      histology_confirmed_cancer = paste(histology_code_confirmed_cancer, collapse =
                                           '---'),
      behavior_confirmed_cancer = paste(behavior_confirmed_cancer, collapse =
                                          '---'),
      grade_confirmed_cancer = paste(grade_confirmed_cancer, collapse =
                                       '---'),
      ICD_coding_book = paste(ICD_coding_book, collapse = '---')
    )
  
  # Now turn the collapsed cells into 7 columns (max 7 cancers per patient in this dataset)
  cancer <-
    cancer %>% separate(
      topography_confirmed_cancer,
      sep = '---',
      into = get_numbered_vector('topography_confirmed_cancer', 7)
    ) %>%
    separate(
      confirmed_cancer_date,
      sep = '---',
      into = get_numbered_vector('confirmed_cancer_date', 7)
    ) %>%
    separate(
      behavior_confirmed_cancer,
      sep = '---',
      into = get_numbered_vector('behavior_confirmed_cancer', 7)
    ) %>%
    separate(
      histology_confirmed_cancer,
      sep = '---',
      into = get_numbered_vector('histology_confirmed_cancer', 7)
    )
  
  #Nest cancer dataframe:
  cancer <-
    cancer %>% group_by(ID, IDTYPE) %>% nest() %>% dplyr::rename(cancer_data = data) %>% ungroup()
  
  return(cancer)
}

identify_closest_exam_to_CT <- function(df) {
  #'Add columns for exam close to CT date absolutely, post CT, and pre CT.
  #'
  #'@description Will link date of CT to an exam.
  #'
  #'@param df data.frame The dataframe.
  
  
  #We have access to certain exam numbers per cohort.
  #For any CT scan, we will determine the exam sheet of interest from the FHS cohort number
  #and then select the exam for this patient that was the closest previous to the CT scan.
  
  
  #Get all exam dates of any patient in a list
  date_ns <- 1:32
  prefix <- 'Date_e'
  
  dates <-
    lapply(date_ns, function(x) {
      df[[paste(prefix, x, sep = '')]]
    })
  
  #Get CT dates
  study_dates <- df$study_date
  
  #Initialize the distance in days to be very far away from 0. Any date closer to the study date will easily overwrite this.
  current_min_pre_ct <- rep(-10000, length(dates[[1]]))
  current_min_post_ct <- rep(10000, length(dates[[1]]))
  current_min_abs <- rep(10000, length(dates[[1]]))
  
  #Initialize closest exam pre ct to always be exam 1, since it will always be the earliest. Set closest absolute exam and closest exam post CT to undetermined.
  closest_exam_post_ct <- rep('not determined', length(dates[[1]]))
  closest_exam_pre_ct <-
    rep(1, length(dates[[1]])) #If a scan was before scan 1, still select 1.
  closest_exam_absolute <- rep(1, length(dates[[1]]))
  
  #Iterate over all exam dates and see if one was closer pre or post the exam, or in absolute terms, than the previously closest.
  for (i in 1:length(dates)) {
    date_a <- dates[[i]]
    #Difference from exam date to CT study date.
    diff_a <- date_a - study_dates
    
    #See if closer to CT date than current minimum (if before CT date).
    smaller_min <- diff_a >= current_min_pre_ct & diff_a <= 0
    smaller_min <- replace_na(smaller_min, FALSE)
    #Update minima and label of closest pre CT exam number.
    current_min_pre_ct[smaller_min] <- diff_a[smaller_min]
    closest_exam_pre_ct[smaller_min] <- i
    
    #See if closer to CT date than current minimum (if past CT date).
    smaller_min <- diff_a <= current_min_post_ct & diff_a >= 0
    smaller_min <- replace_na(smaller_min, FALSE)
    #Update minima and label of closest post CT exam number.
    current_min_post_ct[smaller_min] <- diff_a[smaller_min]
    closest_exam_post_ct[smaller_min] <- i
    
    #And finally in absolute values
    smaller_min <- abs(diff_a) <= current_min_abs
    smaller_min <- replace_na(smaller_min, FALSE)
    current_min_abs[smaller_min] <- abs(diff_a[smaller_min])
    closest_exam_absolute[smaller_min] <- i
  }
  
  #Fill in the cases where min was never overriden with distance to first exam:
  current_min_pre_ct[current_min_pre_ct == -10000] = dates[[1]][current_min_pre_ct ==
                                                                  -10000] - study_dates[current_min_pre_ct == -10000]
  
  #Append calculated closest exam timepoints and their time distance to the dataframe
  df$closest_exam_n_pre_ct <- closest_exam_pre_ct
  df$closest_exam_time_days_pre_ct <- current_min_pre_ct
  
  df$closest_exam_n_post_ct <- closest_exam_post_ct
  df$closest_exam_time_days_post_ct <- current_min_post_ct
  
  
  df$closest_exam_n_absolute <- closest_exam_absolute
  df$closest_exam_time_days_absolute <- current_min_abs
  
  
  return(df)
}

read_exam_data <- function() {
  #'Read in FHS Exam sheets.
  #'
  #'@return The exam sheets in a list.
  
  print("Reading in Exam data.")
  
  paths <-
    readr::read_csv('read_data/rename_sheets_basecamb/paths.csv') %>%
    dplyr::pull(path, key)
  
  # Exam 1 Generation 3 (Dates 2002-2005) (IDTYPE 3, 4095 observations)
  gen3exam1 <-
    read_sas_and_apply_data_dict(paths[['gen3exam1']], 'dd_gen3exam1.csv')
  gen3exam1 <- factorize_IDTYPE(gen3exam1)
  
  #' Gen 3 and Omni 2 Exam 2 (2008 - 2011) (IDTYPE 2 with 68 obs, 3 with 3411 obs, 72 with 321 obs)
  gen3omni2exam2 <-
    read_sas_and_apply_data_dict(paths[['gen3omni2exam2']], 'dd_gen3omni2exam2.csv')
  gen3omni2exam2 <- factorize_IDTYPE(gen3omni2exam2)
  
  # Offspring Exam 7 (1998-2001) (3539 of IDTYPE 1)
  offspringexam7 <-
    read_sas_and_apply_data_dict(paths[['offspringexam7']], "dd_offspringexam7.csv")
  offspringexam7 <- factorize_IDTYPE(offspringexam7)
  
  # Offspring Exam 8 (2005-2008) (3021 of IDTYPE 1)
  offspringexam8 <-
    read_sas_and_apply_data_dict(paths[['offspringexam8']], 'dd_offspringexam8.csv')
  offspringexam8 <- factorize_IDTYPE(offspringexam8)
  
  #Offspring Exam 9, Omni 1 Exam 4 (2430 Offspring, 301 Omni 1):
  offspringexam9omni1exam4 <-
    read_sas_and_apply_data_dict(paths[['offspringexam9omni1exam4']], 'dd_offspringexam9omni1exam4.csv')
  offspringexam9omni1exam4 <-
    factorize_IDTYPE(offspringexam9omni1exam4)
  
  #Gen 3 and Omni 2 Exam 3 (IDTYPE 2 (Offspring Spouse) with 56 observations, 3 (gen 3) with 3171, 72 (omni 2) with 294)
  gen3omni2exam3 <-
    read_sas_and_apply_data_dict(paths[['gen3omni2exam3']], 'dd_gen3omni2exam3.csv')
  gen3omni2exam3 <- factorize_IDTYPE(gen3omni2exam3)
  
  #Omni 1 Exam 3 (298 Omni 1 participants)
  omni1exam3 <-
    read_sas_and_apply_data_dict(paths[['omni1exam3']], 'dd_omni1exam3.csv')
  omni1exam3 <- factorize_IDTYPE(omni1exam3)
  
  FHS_exams <-
    list(
      gen3exam1 = gen3exam1,
      gen3omni2exam2 = gen3omni2exam2,
      offspringexam7 = offspringexam7,
      offspringexam8 = offspringexam8,
      offspringexam9omni1exam4 = offspringexam9omni1exam4,
      gen3omni2exam3 = gen3omni2exam3,
      omni1exam3 = omni1exam3
    )
  
  return(FHS_exams)
}


get_datasheet_closest_exam <-
  function(df,
           select_closest = c('after', 'previous', 'absolute')) {
    #'Identify closest exam 'after', 'previous', or in 'absolute' terms to CT.
    #'
    #'@param df data.frame The dataframe with CT data
    #'@param select_closest 'after', 'previous', or 'absolute'
    #'
    #'@return A vector that can be placed as new column holding the name of the correct exam sheet.
    
    #Identify correct column with exam numbers
    if (select_closest == 'after') {
      closest_exam_no <- df$closest_exam_n_post_ct
      
      #some of the exams post CT scan will not exist, in these cases select numbers of previous exams.
      sel_vector <- closest_exam_no == 'not determined'
      closest_exam_no[sel_vector] <-
        df$closest_exam_n_pre_ct[sel_vector]
    }
    if (select_closest == 'previous') {
      closest_exam_no <- df$closest_exam_n_pre_ct
    }
    if (select_closest == 'absolute') {
      closest_exam_no <- df$closest_exam_n_absolute
    }
    closest_exam_no <- as.character(closest_exam_no)
    
    #This will be the final return vector holding exam names for each row.
    return_vec <- closest_exam_no
    
    #Prepare selection vectors
    offspring2_sel_vec <- df$IDTYPE == 'Offspring (Gen 2)'
    gen3_sel_vec <- df$IDTYPE == 'Gen 3'
    omni1_sel_vec <- df$IDTYPE == 'Omni 1'
    omni2_sel_vec <- df$IDTYPE == 'Omni 2'
    
    #Get offspring 2 exams
    
    return_vec[offspring2_sel_vec &
                 closest_exam_no == '7'] <- 'offspringexam7'
    return_vec[offspring2_sel_vec &
                 closest_exam_no == '8'] <- 'offspringexam8'
    return_vec[offspring2_sel_vec &
                 closest_exam_no == '9'] <-
      'offspringexam9omni1exam4'
    #few participant will require 1,4,5, or 6.
    #see describe_required_datasheets() function
    #will default them to exam 7 here:
    return_vec[offspring2_sel_vec &
                 !(closest_exam_no %in% as.character(7:9))] <-
      'offspringexam7'
    
    #Get Gen 3 Exams
    return_vec[gen3_sel_vec & closest_exam_no == '1'] <- 'gen3exam1'
    return_vec[gen3_sel_vec &
                 closest_exam_no == '2'] <- 'gen3omni2exam2'
    return_vec[gen3_sel_vec &
                 closest_exam_no == '3'] <- 'gen3omni2exam3'
    #should not happen, but will default to exam 3.
    return_vec[gen3_sel_vec &
                 !(closest_exam_no %in% as.character(1:3))] <-
      'gen3omni2exam3'
    
    #Get Omni 1 Exams
    return_vec[omni1_sel_vec &
                 closest_exam_no == '3'] <- 'omni1exam3'
    return_vec[omni1_sel_vec &
                 closest_exam_no == '4'] <-
      'offspringexam9omni1exam4'
    #few patients will require exam 1, or 2
    #see describe_required_datasheets() function
    #will default them to exam 3 here:
    return_vec[omni1_sel_vec &
                 !(closest_exam_no %in% as.character(3:4))] <-
      'omni1exam3'
    
    
    #Get Omni 2 exams
    return_vec[omni2_sel_vec &
                 closest_exam_no == '2'] <- 'gen3omni2exam2'
    return_vec[omni2_sel_vec &
                 closest_exam_no == '3'] <- 'gen3omni2exam3'
    #few patients will require exam 1. W
    #see describe_required_datasheets() function
    #will default them to exam 2 here:
    return_vec[omni2_sel_vec &
                 !(closest_exam_no %in% as.character(2:3))] <-
      'gen3omni2exam2'
    
    
    return(return_vec)
  }


generate_data_cols_from_exam <-
  function(df,
           colnames = c(),
           closest = 'absolute',
           fill_nas_from_other_col = TRUE,
           is_binary = T) {
    #'Draw columns from correct exam.
    #'
    #'@description Draw columns from correct exam, based on which should be closest. Can also replace exams manually, if automatic selection is not right for some parameters.
    #'
    #'@param df data.frame The dataframe
    #'@param colnames The names of the columns to be drawn from exams.
    #'@param closest 'absolute', 'previous', or 'after', to decide which exam should be used to draw data from.
    #'@param fill_nas_from_other_col logical Whether to search for a value in columns from other exams, if the desired one held only NA.
    #'@param is_binary logical True, if a data point is binary (e.g. can either have Diabetes, or not. Blood Pressure would not be binary.)
    #'
    #'@return The dataframe with the new columns.
    
    #Identify for each row the closest exam time point.
    closest_exams <- get_datasheet_closest_exam(df, closest)
    
    
    #Load column data dictionary
    path_to_column_dict = 'read_data/rename_sheets_basecamb/ExamsDataDictionary.csv'
    
    col_dict <- readr::read_csv(path_to_column_dict)
    
    #Get data from correct exam column for each of the desired data columns
    for (colname in colnames) {
      print(paste('Gathering data for', colname))
      df[[colname]] <- closest_exams
      df[[colname]] <-
        pull_data_from_correct_exam_col(
          df,
          colname,
          col_dict = col_dict,
          fill_nas_from_other_col,
          is_binary = is_binary
        )
      label(df[[colname]]) <- 'Calculated from Exam Sheet'
    }
    
    
    return(df)
  }


pull_data_from_correct_exam_col <-
  function(df,
           colname,
           col_dict,
           fill_nas_from_other_col = FALSE,
           
           is_binary = T) {
    #'Get data from correct exam column into each row.
    #'
    #'@param df data.frame The dataframe.
    #'@param colname The column for which exam data should be gathered
    #'@param fill_nas_from_other_col Whether to take value from another than preferred exam column, if preferred one held NA.
    #'@param col_dict data.frame The dictionary which will connect the desired title of column of interest to the correct column(s) within the preferred exam.
    #'@param is_binary logical Whether the data is binary. In this case, assuming that '1' is the factor corresponding to TRUE
    
    
    #Pull the column that should be getting data from several exams. This has been previously set to hold the name of the corresponding exam.
    column <- dplyr::pull(df, colname)
    
    return_vec <- rep(FALSE, length(column))
    
    #Match each column to be filled with the corresponding name of the exam sheet
    for (exam in unique(column)) {
      matched_colnames <-
        identify_correct_column(
          data_of_interest = colname,
          preferred_exam = exam,
          col_dict = col_dict
        )
      
      if (any(is.na(matched_colnames))) {
        print(
          paste(
            "No columns were specified to hold data for",
            colname,
            'and',
            exam,
            '. Filling with NA.'
          )
        )
        retrieved_column <- rep(NA, length(column))
      }
      else{
        if (is_binary) {
          #For binary data, return T if any of the specified columns hold a '1'
          retrieved_column <-
            rowwise_any_meeting_condition(df, matched_colnames, '1') #Assuming that being equal to 1 in the factor means TRUE.
        } else{
          #For continuous data, only allow one column to draw data from.
          if (length(matched_colnames) > 1) {
            print(
              paste(
                'Error gathering data for',
                colname,
                '. Assumed the data is not binary, but trying to combine results from several columns:',
                matched_colnames
              )
            )
          } else{
            if (!(matched_colnames[1] %in% colnames(df))) {
              print(
                paste(
                  'Error gathering data for ',
                  matched_colnames[1],
                  '. It does not exist in dataframe, but was believed to be there according to ExamsDataDictionary.csv'
                )
              )
            }
            #Extract data from the specified column
            retrieved_column <- df[[matched_colnames[1]]]
          }
          
        }
      }
      return_vec[column == exam] <- retrieved_column[column == exam]
    }
    
    return(return_vec)
    
    
    
  }

merge_df_with_exam_sheets <- function(df, FHS_exams) {
  #' Merge the dataframe with all exam sheets, giving each column name except for ID and IDTYPE in each exam the corresponding suffix.
  #'
  #'
  #' @param df data.frame The Dataframe to merge with by IDTYPE and ID
  #' @param FHS_exams list A list of all FHS_exams, in the shape exam_name:exam_dataframe
  #'
  #' @return The merged, wide dataframe
  
  for (i in 1:length(names(FHS_exams))) {
    name <- names(FHS_exams)[i]
    print(paste('Merging data with exam', name))
    exam <- FHS_exams[[i]]
    
    #Add suffix to first datasheet
    
    colnames(exam) <- paste(colnames(exam), name, sep = '_')
    exam$ID <- exam[[paste('ID', name, sep = '_')]]
    exam$IDTYPE <- exam[[paste('IDTYPE', name, sep = '_')]]
    #Join
    df <-
      df %>% left_join(exam,
                       by = c('ID', 'IDTYPE'),
                       suffix = c('', paste('_', name, sep = '')))
  }
  
  
  return(df)
}

create_datasheet_merged_exam_data <- function(df) {
  #'Create Dataframe holding data from all exam sheets.
  #'
  #'@description The full dataframe is merged from various exam sheets, sometimes holding overlapping information.
  #'
  #'@param df data.frame The dataframe to be restructured.
  #'@param FHS_exams list A list of all FHS exams.
  #'
  #'@return The restructured dataframe.
  
  
  #Get exams in a labelled list (dict)
  FHS_exams <- read_exam_data()
  
  #merge dataframe with exams, giving each exam data the correct suffix
  df_w_exams <- merge_df_with_exam_sheets(df, FHS_exams)
  
  return(df_w_exams)
}

add_continuous_exam_data <- function(df) {
  #'Add continuous data (weight, height, BP, physical activity,...) from correct exam sheet.(closest absolutely to CT scan)
  #'
  #'@description Add continuous data. data.
  #'
  #'@param df data.frame The dataframe merged with all exam data.
  #'
  #'@return The dataframe including columns for anthropometric data.
  
  #Must correspond to names in ExamsDataDictionary.csv
  colnames <-
    c(
      'Height_in',
      'Weight_lb',
      'NeckCircumferenceIn',
      'WaistGirthIn',
      'HipGirthIn',
      'ThighGirthIn',
      'SagittalAbdominalDiameterCm',
      'BPsystol',
      'BPdiastol',
      'HrsBasalActivity',
      'HrsSedentaryActivity',
      'HrsSlightActivity',
      'HrsModerateActivity',
      'HrsHeavyActivity',
      'HandGripStrength1Right',
      'HandGripStrength1Left',
      'QuickWalkS',
      'ChairStands',
      'InterimQuitSmokingTime',
      'AgeStoppedSmoking',
      'AgeStartedSmoking',
      'SmokedPerDayAverage'
    )
  #Want to look at absolute closest exam
  closest <- 'absolute'
  
  
  df <-
    generate_data_cols_from_exam(
      df,
      colnames,
      closest = closest,
      fill_nas_from_other_col = TRUE,
      is_binary = F
    )
  
  df <-
    df %>% mutate(
      Height_m = as.numeric(Height_in) * 0.0245,
      Weight_kg = as.numeric(Weight_lb) * 0.453592
    ) %>%
    mutate(BMI = Weight_kg / Height_m ^ 2)
  return(df)
}

add_binary_exam_data <- function(df) {
  #'Add continuous data (weight, height, BP, physical activity,...) from correct exam sheet.(closest after CT scan.)
  #'
  #'@description Add binary data.
  #'
  #'@param df data.frame The dataframe merged with all exam data.
  #'
  #'@return The dataframe including columns for binary data.
  
  #Handle alcohol special cases:
  #Set NA to FALSE/0
  df$alcohol_wine_r_once_a_month_gen3exam1[is.na(df$alcohol_wine_r_once_a_month_gen3exam1)] <-
    0
  df$alcohol_wine_w_once_a_month_gen3exam1[is.na(df$alcohol_wine_w_once_a_month_gen3exam1)] <-
    0
  df$alcohol_wine_r_drinks_per_week_offspringexam7[is.na(df$alcohol_wine_r_drinks_per_week_offspringexam7)] <-
    0
  df$alcohol_wine_w_drinks_per_week_offspringexam7[is.na(df$alcohol_wine_w_drinks_per_week_offspringexam7)] <-
    0
  
  #Handle white and red wine together:
  df$alcohol_wine_r_once_a_month_gen3exam1 <-
    (df$alcohol_wine_r_once_a_month_gen3exam1 == 1) |
    (df$alcohol_wine_w_once_a_month_gen3exam1 == 1)
  df$alcohol_wine_r_drinks_per_week_offspringexam7 <-
    df$alcohol_wine_r_drinks_per_week_offspringexam7 + df$alcohol_wine_w_drinks_per_week_offspringexam7
  
  #In offspring exam 7, alcohol is given as drinks per week on average, not whether a drink would be had once per week. Change this to a binary drinks in a week or no.
  df$alcohol_beer_drinks_per_week_offspringexam7 <-
    df$alcohol_beer_drinks_per_week_offspringexam7 > 0
  df$alcohol_wine_r_drinks_per_week_offspringexam7 <-
    df$alcohol_wine_r_drinks_per_week_offspringexam7 > 0
  df$alcohol_liquor_drinks_per_week_offspringexam7 <-
    df$alcohol_liquor_drinks_per_week_offspringexam7 > 0
  
  
  #Handle menopause data: get TRUE/FALSE column for every exam
  df$menopause_gen3exam1 <-
    (df$menopause_gen3exam1 != 0) &
    (df$menopause_gen3exam1 != 8) #0 means period not stopped, 8 is code for male
  df$menopause_status_gen3omni2exam2 <-
    df$menopause_status_gen3omni2exam2 == 3 |
    df$menopause_status_gen3omni2exam2 == 4 |
    df$menopause_status_gen3omni2exam2 == 5 #3 is perimenopausal (periods stopped for <= 1 year, 4 is periods stopped for >= 1 year, 5 is period stopped but reinduced by hormone therapy)
  df$menopause_status_gen3omni2exam3 <-
    df$menopause_status_gen3omni2exam3 == 4 |
    df$menopause_status_gen3omni2exam3 == 5 |
    df$menopause_status_gen3omni2exam3 == 6 #4: premenopausal, periods stopped for less than 1 year, 5: over a year, 6: stopped and reinduced by hormonal therapy
  df$menopause_status_offspringexam7 <-
    df$menopause_status_offspringexam7 == 3 |
    df$menopause_status_offspringexam7 == 2 #2: no periods, 3: periods reinduced by hormone therapy
  df$menopause_status_offspringexam8 <-
    df$menopause_status_offspringexam8 == 1 |
    df$menopause_status_offspringexam8 == 2 |
    df$menopause_status_offspringexam8 == 3 #1: periods stopped but reinduced by hormone therapy, 2: periods stopped for over a year, 3: periods stopped for less than a year
  df$menopause_offspringexam9omni1exam4 <-
    df$menopause_offspringexam9omni1exam4 == 1
  
  
  #Must correspond to names in ExamsDataDictionary.csv
  colnames <-
    c(
      "receivesBPMeds",
      "Diabetes",
      "ThyroidDisease",
      "GallbladderDisease",
      "HeartValveSurgery",
      "CarotidArterySurgery",
      "ThoracicAortaSurgery",
      "AbdominalAortaSurgery",
      "hasCoronaryStent",
      "HxOfHospitalizationForHeartFailure",
      "HistoryOfPE",
      "AtrialFibrillationHx",
      "SyncopeHx",
      "FemoralLowerExtremitySurgery",
      "Dementia",
      "Parkinson",
      "SeizureHx",
      "LungFibrosis",
      "Asthma",
      "COPD",
      "Emphysema",
      "HxGallbladderSurgery",
      "HxOtherCVProcedure",
      "HxHysterectomy",
      "HxOvarectomy",
      "HxProstateDisease",
      "IntermittentClaudication",
      "HxDVT",
      "HxSpinalStenosis",
      "HxSleepApnea",
      "HxDepression",
      "HxPsychosis",
      "HxAnxiety"
    )
  #Want to look at closest exam post CT for these.
  closest <- 'after'
  
  
  df <-
    generate_data_cols_from_exam(
      df,
      colnames,
      closest = closest,
      fill_nas_from_other_col = TRUE,
      is_binary = T
    )
  
  #And at closest absolute exam for these:
  colnames <-
    c(
      "CurrentSmoker",
      "AlcoholDrinksSpirits",
      "AlcoholDrinksWine",
      "AlcoholDrinksBeer",
      "Menopause"
    )
  closest <- 'absolute'
  df <-
    generate_data_cols_from_exam(
      df,
      colnames,
      closest = closest,
      fill_nas_from_other_col = TRUE,
      is_binary = T
    )
  
  #Handle alcohol:
  #Sum up if any alcohol is drunk:
  df$Alcohol <-
    df$AlcoholDrinksBeer |
    df$AlcoholDrinksWine | df$AlcoholDrinksSpirits
  
  
  return(df)
}

rowwise_any_meeting_condition <- function(df, colnames, condition) {
  #'Check if any of a series of columns in a dataframe meets a condition.
  #'
  #'@param df data.frame The dataframe
  #'@param colnames character A vector of names of columns to check
  #'@param condition Which condition the vectors should be equal to. (i.e., df[[colname]] == condition)
  #'
  #'@return A selection vector with TRUE, where any column met the condition.
  return_vec <- rep(FALSE, nrow(df))
  for (colname in colnames) {
    column <- df[[colname]]
    
    #If column is not a logical vector already, turn it into one by using column == condition
    if (!is.logical(column))
    {
      meeting_condition <- column == condition
      meeting_condition <- replace_na(meeting_condition, FALSE)
    }
    else{
      meeting_condition <- column
    }
    
    return_vec <- meeting_condition | return_vec
  }
  
  return(return_vec)
}


identify_correct_column <- function(data_of_interest,
                                    preferred_exam,
                                    col_dict,
                                    append_exam_name_to_colname = T) {
  #' Get name of column in datasheet that will hold the desired data.
  #'
  #' @param data_of_interest character The title of the column of interest. e.g. 'Diabetes'
  #' @param preferred_exam character The exam of choice. e.g. 'gen3exam1'
  #' @param col_dict data.frame The column dict. See explanation below.
  #' The sheet has one column named 'Exam' holding the name of the exam which the row corresponds to. All other columns will hold the names of variables of interest, e.g. 'Diabetes'
  #' Each cell will then hold for the exam corresponding to the row one or more column names corresponding to the variable given as column name.
  #' E.g. 'gen3exam1':'Diabetes' will hold 'diabetes_meds_ever', which is the name of the column in the gen3exam1 sheet, which will be used as source of truth for whether a participant has diabetes.
  #' If several entries are given, split by '|', having any of them TRUE will lead to an overall true value for e.g. 'Diabetes'
  #' If no column in the exam of interest holds valuable information for the data_of_interest, the name of another exam will be given, the row of which will be searched.
  #' @param append_exam_name_to_colname logical Whether the name of the exam from which data should be drawn should be appended, e.g. 'Diabetes_gen3exam1'
  #'
  #' @return The names of the columns of interest as a vector.
  
  
  #Extract names of columns where the data of interest is stored.
  exam_names <- col_dict$Exam
  
  exam_row <- col_dict[exam_names == preferred_exam,]
  
  colnames <- exam_row[[data_of_interest]][1]
  
  colnames <- stringr::str_split(colnames, coll('|'))[[1]]
  
  #Check, if a cell points to another exam row and move there, if so.
  while (colnames[1] %in% exam_names) {
    preferred_exam <- colnames[1]
    exam_row <- col_dict[col_dict$Exam == preferred_exam,]
    
    colnames <- exam_row[1, data_of_interest]
    colnames <- stringr::str_split(colnames, coll('|'))[[1]]
  }
  
  
  
  if (append_exam_name_to_colname & !any(is.na(colnames))) {
    #Append name of exam to each column name
    colnames <- paste(colnames, preferred_exam, sep = '_')
  }
  
  return(colnames)
}
