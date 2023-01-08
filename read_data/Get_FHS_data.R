#' Importing Framingham Data
#' Total ~ 20 SAS files
#' Will import a few of the relevant ones to start out in this scrip
#' 
#' Paths are outlined in paths.csv --> Folder with all data is stored in dropbox 'Framingham' subfolder.

require(haven)
require(dplyr)
require(table1)
source('read_data/Read_FHS_data.R')

Get_FHS_data <- function(factorize_ID = TRUE,
                         include_survival_data = F, include_labs = F, include_pft = F) {
  #'Read FHS data.
  #'
  #'@description Read FHS data defined in Read_FHS_data.R and exam data defined in Read_FHS_exams.R.
  #'Calculate time difference from CT1 and 2, as well as age during those procedures. Calculate time from
  #'pulmonary function test to CT1 and 2. Factorize cohort, as well as sex, cardiovaskular outcomes, stroke.
  #'Rename columns meaningfully. Factorize mortality. Calculate time from CT2 to death.
  #'
  #'@param factorize_ID bool whether to factorize the cohort IDTYPE column.
  #'@param include_survival_data logical Whether to include data from survival sheets.
  #'@param include_labs logical Whether to include data from lab sheets.
  #'@param include_pft logical Whether to include data from PFT sheets.
  #'
  #'@return The gathered datasheet.
  
  # Read in FHS data
  
  print('Reading FHS data')
  FHS_data <- Read_FHS_data(include_survival_data, include_labs, include_pft)
  print('Finished reading FHS data.')
  
  # Data Tidying ####
  FHS_data$Sex <- factor(FHS_data$Sex, levels = c(1:2), labels = c("Male", "Female"))
  
  # Define Age at CT
  FHS_data$timediffct1 <- lubridate::time_length(difftime(FHS_data$SCAN_DATE1, FHS_data$Date_e1), "years")
  FHS_data$timediffct2 <- lubridate::time_length(difftime(FHS_data$SCAN_DATE2, FHS_data$Date_e1), "years")
  
  label(FHS_data$timediffct1) <- 'Time difference from first exam to CT1'
  label(FHS_data$timediffct2) <- 'Time difference from first exam to CT2'
  
  FHS_data$ageCT1 <- FHS_data$Age_e1+FHS_data$timediffct1
  FHS_data$ageCT2 <- FHS_data$Age_e1+FHS_data$timediffct2
  
  #Relabel age, race, ...
  label(FHS_data$ageCT1) <- "Age at MDCT 1"
  label(FHS_data$ageCT2) <- "Age at MDCT 2"
  label(FHS_data$RACE_CODE) <- "Race"
  
  # Time from PFT to CT 2 
  if(include_pft){
    FHS_data$timepftCT2 <- lubridate::time_length(difftime(FHS_data$SCAN_DATE2, FHS_data$PFT_date), "years")
    
    #Relabel PFT columns
    label(FHS_data$timepftCT2) <- "Time Between PFT and CT 2 (Years)"
    label(FHS_data$FEV) <- "Forced Expiratory Volume at 1 Second (FEV1)"
    label(FHS_data$FVC) <- "Forced Vital Capacity (FVC)"
    label(FHS_data$PF) <- "Peak Expiratory Flow (liters/s)"
    label(FHS_data$FEV_FVC) <- "FEV1/FVC Ratio"
  }
  
  
  #Factorize IDTYPE to cohorts
  if(factorize_ID){
    FHS_data$IDTYPE <- factor(FHS_data$IDTYPE, levels = c(1, 3, 7, 72), 
                            labels = c('Offspring (Gen 2)', 'Gen 3', "Omni 1", "Omni 2"))
  }
  
  
  #Factorize and relabel Survival and CVD SOE data
  if(include_survival_data){
    FHS_data$chd <- factor(FHS_data$chd, levels=c(0:1), labels = c("No", "Yes"))
    FHS_data$chf <- factor(FHS_data$chf, levels=c(0:1), labels = c("No", "Yes"))
    FHS_data$cvd <- factor(FHS_data$cvd, levels=c(0:1), labels = c("No", "Yes"))
    FHS_data$stroke <- factor(FHS_data$stroke, levels=c(0:1), labels = c("No", "Yes"))
    FHS_data$CHDDEATH <- factor(FHS_data$CHDDEATH, levels=c(0:1), labels = c("No", "Yes"))
    FHS_data$CVDDEATH <- factor(FHS_data$CVDDEATH, levels=c(0:1), labels = c("No", "Yes"))
    
    label(FHS_data$chd) <- "Coronary Heart Disease"
    label(FHS_data$chf) <- "Congestive Heart Failure"
    label(FHS_data$cvd) <- "Cardiovascular Disease"
    label(FHS_data$stroke) <- "Stroke"
    label(FHS_data$CHDDEATH) <- "Death from Coronary Heart Disease"
    label(FHS_data$CVDDEATH) <- "Death from Cardiovascular Disease"
    
    
    #Factorize Mortality with Label
    FHS_data$Mortality <- factor(ifelse(is.na(FHS_data$DATEDTH), 'Alive', 'Dead'))
    #Define Time from CT2 to Death and Label 
    FHS_data$timect2death <- ifelse(!is.na(FHS_data$DATEDTH), 
                                    lubridate::time_length(difftime(FHS_data$DATEDTH, FHS_data$SCAN_DATE2), "years"), 
                                    NA)
    label(FHS_data$timect2death) <- "Time from CT 2 to Death (If Applicable)"
    label(FHS_data$Mortality) <- "Mortality (All-Cause)"
    }

  
  return(FHS_data)
  
}




















