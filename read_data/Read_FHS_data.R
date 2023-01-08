#' 
#' 
#' FHS Body Composition 
#' Read in all data in this script, source this page when loading in all data to merge. 
#'
#'  
require(readr)
require(dplyr)
source('read_data/read_sas_and_apply_data_dict.R')

Read_FHS_data <-  function(include_survival_data = F, include_labs = F, include_pft = F){
  #'Read and merge data.
  #'
  #'@description Read and merge Framingham datasheets regarding CT date, age, sex, cardiac survival,
  #'stroke survival, all cause mortality, race, pulmonary function tests (of offspring exam 7 and
  #'generation 3, as well as generation 3 omni), albumin labs. Make sure to correctly specify
  #'paths to datasheets in paths.csv.
  #'
  #'@param include_survival_data logical Whether to include data from survival sheets.
  #'@param include_labs logical Whether to include data from lab sheets.
  #'@param include_pft logical Whether to include data from PFT sheets.
  #'
  #'@return merged datasheet with all patients that received at least one CT scan.
  
  
  paths <- readr::read_csv('read_data/rename_sheets_basecamb/paths.csv') %>%
    dplyr::pull(path, key)
  
  # CT Dates -- > Use this to select all the patients who got CT scans
  path <- paths[['CTdate']]
  CTdate <- read_sas_and_apply_data_dict(path, 'dd_CTdate.csv')
  
  # Age/Sex, and exam timepoints
  agesex <- read_sas_and_apply_data_dict(paths[['agesex']], 'dd_agesex.csv')
  
  # Left join datasheets to keep data only for participants with CT scans.
  join_v <- c('ID', 'IDTYPE')
  FHS_data <- CTdate %>%
    left_join(agesex, by=join_v, suffix=c('_CT_date', '_age_sex'))
  
  # Race/Ethnicity
  race <- read_sas_and_apply_data_dict(paths[['race']], 'dd_race.csv')
  
  #Rename and refactor race
  race <- decode_race_ethnicity(race)
  
  FHS_data <- FHS_data %>%
    left_join(race, by=join_v, suffix=c('_CT_date_age_sex', '_race'))
  
  
  
  #Read in and add survival data
  if(include_survival_data){
    
    #'Survival/follow-up for CHF, CVD, CHD
    surv_cardiac <- read_sas_and_apply_data_dict(paths[['surv_cardiac']],'dd_surv_cardiac.csv')
  
    #'Survival Follow up Stroke
    surv_stroke <- read_sas_and_apply_data_dict(paths[['surv_stroke']],'dd_surv_stroke.csv')
  
    #' survival all cause mortality
    surv_allcause <- read_sas_and_apply_data_dict(paths[['surv_allcause']],'dd_surv_allcause.csv')
    
    #Join the three survival sheets into one
    surv <- surv_allcause %>%
      full_join(surv_cardiac, by=c('ID', "IDTYPE"), suffix=c('_surv_allcause', "_surv_cardiac")) %>%
      full_join(surv_stroke, by=c('ID', "IDTYPE"), suffix=c('_surv_allcause_cardiac', "_surv_stroke"))
  
    FHS_data <- FHS_data %>% 
      left_join(surv, by=join_v, suffix=c('_CT_date_age_sex_race', '_surv'))
  }
  
  
  if(include_pft){
    #' Pulmonary Function Test
    pftoffspring <- read_sas_and_apply_data_dict(paths[['pftoffspring']], 'dd_pftoffspring.csv')
  
    pftgen3omni <- read_sas_and_apply_data_dict(paths[['pftgen3omni']],'dd_pftgen3omni.csv')
  
    pft <- rbind(pftoffspring, pftgen3omni)
    
    FHS_data <- FHS_data %>%
      left_join(pft, by=join_v, suffix=c('_CT_date_age_sex_race', '_pft'))
  }
  
  if(include_labs)
    {#' Blood Albumin Levels 
    laboffspring <- read_sas_and_apply_data_dict(paths[['laboffspring']], 'dd_laboffspring.csv')
  
    labgen3omni <- read_sas_and_apply_data_dict(paths[['labgen3omni']], 'dd_labgen3omni.csv')
  
    labs <- rbind(laboffspring, labgen3omni)
    FHS_data <- FHS_data %>%
      left_join(labs, by=join_v, suffix=c('_CT_date_age_sex_race', '_labs'))
    }
  
  
  
  return(FHS_data)
}

get_race_ethnicity_dict <- function(){
  #Datadict following vr_raceall_2008_a_0712 data guide provided by FHS
  #and following NIH guidelines for reporting race (https://grants.nih.gov/grants/guide/notice-files/not-od-15-089.html#:~:text=The%20revised%20standards%20contain%20five,%22Not%20Hispanic%20or%20Latino.%22)
  #
  race_ethnicity_dict <- list(
    W = 'White',
    B = 'Black or African American',
    A = 'Asian',
    P = 'Native Hawaiian or Other Pacific Islander',
    N = 'American Indian or Alaskan Native',
    X = 'Asian Indian or Pacific Islander',
    O = 'Other',
    R = 'Race/Ethnicity not reported',#Unknown,
    H = 'Hispanic or Latino'
  )
  
  return(race_ethnicity_dict)
}

decode_race_ethnicity <- function(race_df){
  #' Decode race and ethnicity as described in the Framingham RACE_CODE data dictionary
  #' 
  #' @param race_df data.frame The dataframe with the race data
  #' 
  #' @return the decoded race data.
  
  race_ethnicity_dict <- get_race_ethnicity_dict()
  #Create new dataframe for race with RACE_CODE column initially
  race_ethnicity_df <- data.frame(RACE_CODE=race_df$RACE_CODE, IDTYPE = race_df$IDTYPE, ID = race_df$ID)
  #Add binary column with names from race_ethnicity_dict which will hold FALSE by default,
  #and TRUE if the respective letter is in RACE_CODE
  print('Collecting columns for Race and Ethnicity:')
  for(race_letter in names(race_ethnicity_dict)){
    
    race <- race_ethnicity_dict[[race_letter]]
    
    race_ethnicity_df[[race]] <- FALSE
    
    print(race)
    race_ethnicity_df <- race_ethnicity_df %>% rowwise() %>%
      mutate(temp = grepl(race_letter, RACE_CODE))
    
    race_ethnicity_df[[race]] <- race_ethnicity_df$temp
  }
  race_ethnicity_df <- race_ethnicity_df %>% dplyr::select(-temp)
  
  #Report empty race codes as not reported
  
  race_ethnicity_df[[race_ethnicity_dict[['R']]]] <- race_ethnicity_df$RACE_CODE == '' | race_ethnicity_df[[race_ethnicity_dict[['R']]]]  
  
  
  
  return(race_ethnicity_df)
}