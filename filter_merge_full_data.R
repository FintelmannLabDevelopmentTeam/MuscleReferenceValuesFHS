source('read_data/Get_FHS_data.R')
source('read_data/read_filter_assessed_df.R')
source('read_data/read_FHS_exams.R')
source('util/util.R')
source('util/exclude_medical_reasons.R')
source('read_data/add_nearest_labs.R')
source('read_data/Add_Stroke_TIA_data.R')
source('read_data/read_cvd_soe_data.R')
source('util/Physical_Activity_Index.R')
source('util/FHS_Risk_Score.R')
source('util/summarize_conditions.R')
require(tidyverse)
library(reshape)

keep_only_relevant_columns <- function(df){
  #' Filter dataframe to hold only columns relevant for analysis.
  #' 
  #' @param df data.frame The dataframe to be filtered
  
  #Remove columns that came from merging exam sheets, holding various age and date variables.
  df_filtered <- df %>% dplyr::select(
    -contains('exam'),
    -vertebral_level_selection,
    -contains('Date_e'),
    -contains('Age_e')
  )
  
  return(df_filtered)
}


filter_merge_full_data <- function(paths_path = 'read_data/rename_sheets_basecamb/paths.csv', special_path=NULL, drop_cols = T){
  #'Read assessed_df, add CT date and demographics, as well as the corresponding, relevant exam data.
  #'
  #'@description Read assessed_df, calculate other metrics, add exam data, demographics, ...
  #'In summary: Get a datasheet that is ready for final analysis.
  #'
  #'@param paths_path character The path to the paths.csv, holding the paths to all files, including assessed_df.csv, which holds the results of algorithm and QAs
  #'@param special_path character If specified, another path from outside of paths.csv will be used, which is stored in this variable.
  #'@param drop_cols bool The drop_cols argument of read_filter_assessed_df()
  #'
  #'@return The filtered dataframe
  #'
  
  paths <- readr::read_csv(paths_path)
  if(is.null(special_path)){
    
    assessed_df_path <- paths[paths$key == 'assessed_df', 'path']$path[1]
  }else{
    assessed_df_path <- special_path
  }
  print(paste("Reading", assessed_df_path))
  #Read results from QA with measurements and exclusion/inclusion.
  df <- read_filter_assessed_df(assessed_df_path, factorize_ID = FALSE, drop_cols=drop_cols)
  
  #Gather exam dates, CT dates, age at CT1 and CT2, Ethnicities
  FHS_data_no_exams <- Get_FHS_data(factorize_ID = FALSE)
  df_w_data <- df %>% left_join(FHS_data_no_exams, by=c('ID', 'IDTYPE'))  
  
  #Add to each row of data, whether the measurement stems from CT1 or CT2. 
  #Also determine the correct participant age for each row and adds age intervals
  df_w_data<-add_is_CT_1_or_2(df_w_data)
  
  #Find for any CT scan the closest exam, in absolute terms, pre CT, and post CT.
  df_w_data <- identify_closest_exam_to_CT(df_w_data)
  #no exam dates after exam 9. Will drop them here, therefore.
  drop_cols <- sapply(10:32, function(x){paste('Date_e', x, sep='')})
  df_w_data <- df_w_data %>% dplyr::select(-all_of(drop_cols))
  
  #Add data from exam sheets.
  df_w_data <- factorize_IDTYPE(df_w_data)
  df_w_data_w_merged_exams <- create_datasheet_merged_exam_data(df_w_data)
  
  #Add cancer data
  cancer <- read_fhs_cancer()
  df_w_data_w_merged_exams <- df_w_data_w_merged_exams %>% left_join(cancer, by=c('ID', 'IDTYPE'))
  
  #Add Labs data
  labs_path <- paths[paths$key == 'labs1', 'path']$path[1]
  labs2_path <- paths[paths$key == 'labs2', 'path']$path[1]
  df_w_data_w_merged_exams <- add_nearest_labs(df_w_data_w_merged_exams, labs_path, labs2_path)
  #Add data for cardiovascular disease series of events
  cvd_data_path <- paths[paths$key == 'cvd_data', 'path']$path[1]
  df_w_data_w_merged_exams <- add_cvd_soe_data(df_w_data_w_merged_exams, cvd_data_path) 
  #Add stroke and TIA history data
  stroke_data_path <- paths[paths$key == 'stroke_data', 'path']$path[1]
  df_w_data_w_merged_exams <- add_stroke_tia_data(df_w_data_w_merged_exams, stroke_data_path)
  
  #Draw data from merged exam sheets:
  #Add continuous data from exam sheets.
  df_w_data_w_merged_exams <- add_continuous_exam_data(df_w_data_w_merged_exams)
  #Add binary data from exam sheets. This will include mainly medical history data.
  df_w_data_w_merged_exams <- add_binary_exam_data(df_w_data_w_merged_exams)

  

  #Calculate SMI, SMG, ..
  df_w_data_w_merged_exams<-add_muscle_metrics(df_w_data_w_merged_exams)
  
  #Calculate Framingham Risk Score
  df_w_data_w_merged_exams<-add_FHS_risk_score(df_w_data_w_merged_exams, assume_binary_NA_means_FALSE = T)
  
  #Calculate Physical Activity Index
  df_w_data_w_merged_exams<-add_physical_activity_index(df_w_data_w_merged_exams, adjust_basal_act_hrs_to_fit_24h = T, tolerance_threshold = 4, replace_na_w_0 = T)
  
  #Calculate Hypertension
  df_w_data_w_merged_exams <- add_hypertension(df_w_data_w_merged_exams)
  
  #Calculate metabolic syndrome
  df_w_data_w_merged_exams <- add_metabolic_syndrome(df_w_data_w_merged_exams)
  
  #Add Body surface area
  df_w_data_w_merged_exams <- add_body_surface_area(df_w_data_w_merged_exams)
  
  #Calculate for any patient in a row, whether they have CT1, CT2, both, and whether they have data available for T5, 8, 10, and L3
  df_w_data_w_merged_exams <- add_patient_level_data(df_w_data_w_merged_exams)
  
  
  #Filter columns of the dataframe to hold only relevant ones for analysis.
  df_merged <- keep_only_relevant_columns(df_w_data_w_merged_exams)
  
  
  
  return(df_merged)
}



