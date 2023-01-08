require(tidyverse)

physical_activity_index <- function(
    basal_activity_hrs,
    sedentary_activity_hrs,
    slight_activity_hrs,
    moderate_activity_hrs,
    heavy_activity_hrs
){
  #' Calculate the Physical Activity Index
  #' 
  #' @description Kannel W. et al., 1979, Some Health Benefits of Physical Activity: The Framingham Heart Study
  #' 
  #' @param basal_activity_hrs numeric The number of hours spent with basal activities: sleep, rest.
  #' @param sedentary_activity_hrs numeric The number of hours spent with slight activities: walking
  #' @param moderate_activity_hrs numeric The number of hours spent with moderate activities
  #' @param heavy_activity_hrs numeric The number of hours spent with heavy activities.
  #' 
  #' @return The Physical activity index
  
  if(!(basal_activity_hrs +
       sedentary_activity_hrs +
       slight_activity_hrs +
       moderate_activity_hrs +
       heavy_activity_hrs ==
       24)){
    print('Sum of activity hours should be 24. Aborting.')
    return(NA)
  }
  
  pai <-basal_activity_hrs + 
    1.1 * sedentary_activity_hrs +
    1.5 * slight_activity_hrs +
    2.4 * moderate_activity_hrs +
    5 * heavy_activity_hrs
    
  return(pai)
  
}
#Give function a shortcut
pai <- physical_activity_index


add_physical_activity_index <- function(
    df, adjust_basal_act_hrs_to_fit_24h = T, tolerance_threshold = 4, replace_na_w_0 = T
  ){
  #' Add the Physical Activity Index as a column to the dataframe.
  #' 
  #' @param df data.frame The dataframe to which to add the PAI
  #' @param adjust_basal_act_hrs_to_fit_24h logical Whether to adjust the hours reported to spend with basal activity, such that the sum of all activity hours equals 24.
  #' @param tolerance_threshold numeric By how many hours the sum of activity hours may differ from 24 to still adjust the number of hours spent with basal activity.
  #' @param replace_na_w_0 numeric Whether to replace NA values with 0.
  #' 
  #' @return The dataframe with the Physical_Activity_Index column.
  
  get_row_pai <- function(row){
    #'Calculate the physical activity index for each row.
    h_act_basal <- row[['HrsBasalActivity']]
    h_act_sedentary <- row[['HrsSedentaryActivity']]
    h_act_slight <- row[['HrsSlightActivity']]
    h_act_moderate <- row[['HrsModerateActivity']]
    h_act_heavy <- row[['HrsHeavyActivity']]
    
    if(replace_na_w_0){
      h_act_basal <- ifelse(is.na(h_act_basal), 0, h_act_basal)
      h_act_sedentary <- ifelse(is.na(h_act_sedentary), 0, h_act_sedentary)
      h_act_slight <- ifelse(is.na(h_act_slight), 0, h_act_slight)
      h_act_moderate <- ifelse(is.na(h_act_moderate), 0, h_act_moderate)
      h_act_heavy <- ifelse(is.na(h_act_heavy), 0, h_act_heavy)
    }
    
    
    
    if(adjust_basal_act_hrs_to_fit_24h){
      difference <- 24-(h_act_basal + h_act_sedentary + h_act_slight + h_act_moderate + h_act_heavy)
      h_act_basal <- h_act_basal + difference
      #If any of the values is NA, return NA for for the activity index.
      if(is.na(difference)) {return(NA)}
      if(abs(difference) > tolerance_threshold){
        
        return(NA)
      }
    }
    
    physical_act_index <- physical_activity_index(h_act_basal, h_act_sedentary, h_act_slight, h_act_moderate, h_act_heavy)
    return(physical_act_index)
  }
  
  pai <- apply(df, 1, get_row_pai, simplify = T)
  
  print(paste('In', sum(is.na(pai)), 'cases, it was not possible to calculate the physical activity index.'))
  print(paste('Possible Reason includes if Sum of hours is not within 24 +-', tolerance_threshold, ', will return NA for physical activity index.'))
  
  df$Physical_Activity_Index <- pai
  
  return(df)
}
