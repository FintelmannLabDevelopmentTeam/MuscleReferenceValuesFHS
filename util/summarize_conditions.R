#Create arterial hypertension, CVD presence, .. categories
require(tidyverse)
source('read_data/Read_FHS_data.R')




has_hypertension <-
  function(systolic_bp,
           diastolic_bp,
           bp_lowering_treatment) {
    #' Calculate if a participant has high blood pressure.
    #'
    #' @description High blood pressure is defined as systolic Blood pressure >= 140mmHg, diastolic blood pressure >= 90mmHg, or if an individual is receiving medication against high blood pressure.
    #'
    #' @param df data.frame The dataframe.
    #' @param systolic_bp numeric The systolic blood pressure.
    #' @param diastolic_bp numeric The diastolic blood pressure.
    #' @param bp_lowering_treatment logical Whether the individual takes medication to lower their blood pressure.
    #'
    #' @return The calculated hypertension yes/no data.
    
    has_ht <-
      systolic_bp >= 140 | diastolic_bp >= 90 | bp_lowering_treatment
    
    return(has_ht)
  }

add_hypertension <- function(df) {
  #' Add Hypertension column to a dataframe.
  #'
  #' @param df data.frame The dataframe.
  #'
  #' @return The dataframe, with Hypertension column attached.
  
  bp_systol <- df$BPsystol
  bp_diastol <- df$BPdiastol
  bp_lowering_meds <- df$receivesBPMeds
  
  df$Hypertension <-
    has_hypertension(bp_systol, bp_diastol, bp_lowering_meds)
  
  return(df)
}


has_metabolic_syndrome <-
  function(triglycerides,
           systolic_bp,
           diastolic_bp,
           bp_lowering_treatment,
           fasting_glucose_mgdl,
           HDL,
           waist_circumference_in,
           Sex) {
    #' For all individuals in a vector, calculate if they have metabolic syndrome.
    #'
    #' @description Metabolic syndrome is defined following
    #' Modified National Cholesterol Education ProgramAdult Treatment Panel III guidelines (Expert Panel on Detection, 2001, Table 8)
    #'
    #' @param triglycerides numeric The triglycerides in mg/dl.
    #' @param systolic_bp numeric The systolic blood pressure.
    #' @param fasting_glucose_mgdl numeric The fasting glucose level in mg/dl.
    #' @param diastolic_bp numeric The diastolic blood pressure.
    #' @param bp_lowering_treatment logical Whether the individual takes medication to lower their blood pressure.
    #' @param HDL numeric HDL in mg/dl
    #' @param waist_circumference_in The waist circumference in inches.
    #' @param Sex character The individual's sex.
    #'
    #' @return The MetabolicSyndrome column.
    
    men_sel_vector <- Sex == 'Male'
    female_sel_vector <- Sex == 'Female'
    #If 3 of the following are present, metabolic syndrome will be assumed.
    c1 <- rep(F, length(Sex))
    c1[men_sel_vector] <- waist_circumference_in[men_sel_vector] > 40
    c1[female_sel_vector] <-
      waist_circumference_in[female_sel_vector] > 35
    
    c2 <- triglycerides >= 150
    
    c3 <-
      systolic_bp >= 130 | diastolic_bp >= 85 | bp_lowering_treatment
    
    c4 <- fasting_glucose_mgdl >= 110
    
    c5 <- rep(F, length(Sex))
    c5[men_sel_vector] <- HDL[men_sel_vector] < 40
    c5[female_sel_vector] <- HDL[female_sel_vector] < 50
    
    #Assume that missing values are false.
    c1[is.na(c1)] <- F
    c2[is.na(c2)] <- F
    c3[is.na(c3)] <- F
    c4[is.na(c4)] <- F
    c5[is.na(c5)] <- F
    
    n_true <-
      as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5)
    
    metabolic_syndrome <- n_true >= 3
    
    return(metabolic_syndrome)
  }


add_metabolic_syndrome <- function(df) {
  #' Add MetabolicSyndrome column to the dataframe.
  #'
  #' @param df data.frame The dataframe to which to add the column.
  #'
  #'@return The dataframe with the MetabolicSyndrome column.
  
  ms <-
    has_metabolic_syndrome(
      df$TRIG,
      df$BPsystol,
      df$BPdiastol,
      df$receivesBPMeds,
      df$GLUC,
      df$HDL,
      df$WaistGirthIn,
      df$Sex
    )
  
  df$MetabolicSyndrome <- ms
  
  return(df)
}

add_body_surface_area <- function(df) {
  #' Add BSA column, calculated based on Du Bois et al., 1916.
  #'
  #' @param df data.frame The dataframe to which to add the column.
  #'
  #'@return The dataframe with new BSA column.
  height_cm <- df$Height_m * 100
  weight_kg <- df$Weight_kg
  
  bsa <- 0.007184 * weight_kg ** 0.425 * height_cm ** 0.725
  
  df$BSA <- bsa
  return(df)
}


handle_multi_race_categories <-
  function(df, summarize_as = 'Other / Not reported') {
    #'Some participants fall into multiple categories of self-reported race/ethnicity.
    #'Here, these are re-grouped into one single categorial variable "Other/Multiple".
    #'
    #'@param df data.frame The dataframe with the racial columns.
    #'@param summarize_as character Under which name to summarize conditions not falling into one unique category.
    #'
    #'@return The updated data.frame with a Race column.
    
    #Collect categories
    race_ethnicity_dict <- get_race_ethnicity_dict()
    race_ethnicity_categories <-
      all_of(unlist(race_ethnicity_dict, use.names = F))
    
    
    
    df$Race <- ''
    
    #Loop over all categories. Can only clearly set it if only one box is ticked.
    # (Could be rewritten to avoid double comparisons)
    for (re in race_ethnicity_categories) {
      #Initialize selection vector as all participants self-reporting to be part of a category
      re_sel_vec <- df[[re]]
      #Compare if any other racial category has been ticked.
      for (comparison_re in race_ethnicity_categories) {
        #Avoid self-comparison
        #Also avoid comparison between Asians and Asian Indian or Pacific Islander. This combination is still Asian.
        if (!(re == comparison_re) &
            !(re == 'Asian' & #Note that this condition will leave boxes ticked for Asian, if participants self-report as Asian Indian or Pacific Islander, but will still set them to False in the Asian Indian or Pacific Islander column.
              comparison_re == 'Asian Indian or Pacific Islander')) {
          #Set it to false wherever other categories have been selected
          re_sel_vec[df[[comparison_re]]] <- F
          
        }
      }
      #Store name of inspected race/ethnicity wherever it was uniquely selected in the race
      df$Race[re_sel_vec] <- re
    }
    
    #Summarize certain conditions:
    for (cat in c('Other', 'Race/Ethnicity not reported', '')) {
      df$Race[df$Race == cat] <- summarize_as
    }
    
    #Turn into factor
    df$Race <- factor(df$Race, levels = unique(df$Race))
    
    return(df)
  }

#'Choose the most recent data for a given participant.
#'
#'@description In some cases, participants will have measurements generated from both CT-1 and CT-2 in their datasheet.
#'To ensure that the most recent data of a participant is portrayed in tables not stratifying by vertebral level,
#'this function helps select the most recent data of given participants.
#'
#'@param df data.frame The dataframe.
#'
#'@return The dataframe with only one (the most recent) row of data per participant.
choose_most_recent_participant_data <- function(df){
  
  df <- df %>%
    group_by(ID,IDTYPE) %>%
    arrange(study_date) %>% #Arranging by study date, since this is the factor deciding from where data would be drawn, too.
    mutate(latest_study_date = max(study_date)) %>%
    filter(study_date == latest_study_date) %>%
    distinct(ID, IDTYPE, .keep_all = T) %>% 
    ungroup()
  
  return(df)
  
}
