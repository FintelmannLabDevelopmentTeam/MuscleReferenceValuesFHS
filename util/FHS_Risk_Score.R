library(tidyverse)

FHS_Risk_Score <- function(sex = c('Male', 'Female'),
                           age, 
                           total_cholesterol, 
                           hdl_cholesterol, 
                           systolic_bp, 
                           systolic_bp_treated, 
                           smoking, 
                           diabetes){
  #'Calculate the FHS risk score.
  #'
  #'@description 10-year CVD event risk score, as published by D'agostino in 2008.
  #'See General Cardiovascular Risk Profile for Use in Primary Care The Framingham Heart Study, DOI: 10.1161/CIRCULATIONAHA.107.699579
  #'
  #'@param sex The patient sex.
  #'@param age numeric The patient age.
  #'@param total_cholesterol numeric The patient total cholesterol
  #'@param hdl_cholesterol numeric The patient HDL cholesterol
  #'@param systolic_bp numeric The systolic blood pressure of the patient.
  #'@param systolic_bp_treated logical Whether the patient is being treated for high blood pressure.
  #'@param smoking logical Whether the patient smokes.
  #'@param diabetes logical Whether the patient has diabetes.
  #'
  #'@return The patient's 10-year CVD FHS risk score.
  
  #There are two different models, one for male, one for female.
  #t = 10 years.
  risk <- 0
  
  if(any(is.na(c(sex, age, total_cholesterol, hdl_cholesterol, systolic_bp, systolic_bp_treated, smoking, diabetes)))){
    print("Cannot calculate FHS risk score if any of the predictors are NA. Returning NA.")
    return(NA)
  }
  
  if(sex == 'Male'){
    #Adjust blood pressure for treatment
    beta_bp <- 1.93303
    if(systolic_bp_treated) {
      beta_bp <- 1.99881
    }
    
    h_0 <- .88936
    
    risk <- 1- h_0 **
      exp(
        log(age) * 3.06117 +
          log(total_cholesterol) * 1.1237 +
          log(hdl_cholesterol) * -0.93263 +
          log(systolic_bp) * beta_bp +
          as.numeric(smoking) * 0.65451 + 
          as.numeric(diabetes) * 0.57367 -
          23.9802
      )
  }
  
  if(sex == 'Female'){
    beta_bp <- 2.76157
    if(systolic_bp_treated){
      beta_bp <- 2.82263
    }
    
    h_0 <- 0.95012
    
    risk <- 1- h_0 **
      exp(
        log(age) * 2.32888 +
          log(total_cholesterol) * 1.20904 +
          log(hdl_cholesterol) * -0.70833 +
          log(systolic_bp) * beta_bp +
          as.numeric(smoking) * 0.52873 +
          as.numeric(diabetes) * 0.69154 -
          26.1931
      )
  }
  
  return(risk)
  
}

test_fhs_calculator <- function(){
  #Compare inputs to known outputs from online calculator:
  #https://www.framinghamheartstudy.org/fhs-risk-functions/cardiovascular-disease-10-year-risk/
  
  risk_1 = FHS_Risk_Score('Male', 45, 160, 40, 120, F, T, F)
  
  
  print(paste(risk_1, 'was estimated. According to online-tool, should be: 9.5%'))
  
  risk_2 = FHS_Risk_Score('Female', 70, 180, 30, 160, T, T, T)
  print(paste(risk_2, 'was estimated. According to online-tool, should be: 68.5%'))
  
}


add_FHS_risk_score <- function(df, assume_binary_NA_means_FALSE = T){
  #' Add the Framingham Risk score to a dataframe. See 10-year CVD event risk score, as published by D'agostino in 2008.
  #' 
  #' @param df data.frame The dataframe to which to add the FHS risk score
  #' @param assume_binary_NA_means_FALSE logical Whether to assume that if a value is NA for the binary options, that this will be FALSE.
  #' 
  #' @return The dataframe with the added FHS_Risk_Score_10yr_CVD column
  
  get_row_fhs_score <- function(row){
    sex <- row[['Sex']]
    age <- row[['age']]
    total_cholesterol <- row[['CHOL']]
    hdl_cholesterol <- row[['HDL']]
    systolic_bp <- row[['BPsystol']]
    bp_treated <- row[['receivesBPMeds']]
    smoking <- row[['CurrentSmoker']]
    diabetes <- row[['Diabetes']]
    
    if(assume_binary_NA_means_FALSE){
      if(is.na(bp_treated)){bp_treated <- F}
      if(is.na(smoking)){smoking <- F}
      if(is.na(diabetes)){diabetes <- F}
    }
    
    if(any(is.na(c(sex, age, total_cholesterol, hdl_cholesterol, systolic_bp, bp_treated, smoking, diabetes)))){
      return(NA) #Cannot calculate risk score if any variable is NA.
    }
    
    fhs_score_row <- FHS_Risk_Score(sex, age, total_cholesterol, hdl_cholesterol, systolic_bp, bp_treated, smoking, diabetes)
    
    return(fhs_score_row)
  }
  
  fhs_risk_column <- apply(df, 1, get_row_fhs_score, simplify=T)
  
  df$FHS_Risk_Score_10yr_CVD <- fhs_risk_column
  
  #See how it compares with Hoffmann cohort: They had only Cohort 1 participants, with mean FHS Risk score of .09 and SD 0.1
  hoffmann_cohort <- df  %>% filter(CT_number =='1') %>% distinct(ID, IDTYPE, .keep_all = T)
  print(paste('Cohort corresponding to Hoffmann`s would have FHS Risk Score Mean(SD): ', round(mean(hoffmann_cohort$FHS_Risk_Score_10yr_CVD, na.rm=T), 3), '(', round(sd(hoffmann_cohort$FHS_Risk_Score_10yr_CVD, na.rm=T),3), ')', sep=''))
  print(paste('Where Hoffmann`s had 0.09 (0.1).'))
  
  return(df)
}
