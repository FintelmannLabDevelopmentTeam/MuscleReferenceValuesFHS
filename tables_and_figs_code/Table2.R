require(gtsummary)
source('filter_merge_full_data.R')
library('tidyverse')
require('gt')

#Helper functions
mean_minus_2_sd <- function(x){return(mean(x)-2*sd(x))}

get_n <- function(x){return(sum(!is.na(x)))}

reference_value_tables <- function(df = NULL,
                                   root_path = 'figures/',
                                   name = '')
{
  #' Create and save reference tables for a given cohort.
  #' 
  #' @param df data.frame The cohort dataframe.
  #' @param root_path character The root path to where the figures should be saved.
  #' @param name character Which name the saved files should have.
  
  #Set layout for gtsummary
  gtsummary::theme_gtsummary_compact()
  gtsummary::theme_gtsummary_journal('jama')
  
  #Load data, if not given:
  if(is.null(df)){df <- get_final_datasheet()}
  
  #Tidy data
  table2_df <- df %>% dplyr::select(ID, IDTYPE, vertebral_level, decade, Sex, vertebral_level, muscle_area_cm2, muscle_mean_hu, skeletal_muscle_index, skeletal_muscle_gauge, patient_id) %>% dplyr::rename(CSMA = muscle_area_cm2, SMI = skeletal_muscle_index, SMG = skeletal_muscle_gauge, SMRA = muscle_mean_hu)
  
  #Necessary for when called from function. table2_df_wide has to be in global namespace, apparently.
  table2_df_wide <<- table2_df %>% tidyr::pivot_wider(id_cols = c('ID', 'IDTYPE', 'decade', 'Sex', 'patient_id'), names_from='vertebral_level', values_from=c('CSMA', 'SMRA', 'SMI', 'SMG'))
  
  
  
  
  #Adjust header based on name:
  if(name!=''){
    title_name <- paste(' in', name)
    saving_name <- paste('_', name, sep='')
  }else{
    title_name <- name
    saving_name <- name
  }
  
  
  #Option 1: all metrics: -> Preferred 
  table_2_long <- table2_df_wide %>% dplyr::select(-patient_id, -ID, -IDTYPE) %>%
    tbl_strata(strata=Sex,
               .tbl_fun = 
                 ~ .x %>%
                 tbl_summary(by=decade,
                             
                             label=list(CSMA_T5 ~ 'T5 CSMA',
                                        CSMA_T8 ~ 'T8 CSMA',
                                        CSMA_T10 ~ 'T10 CSMA',
                                        CSMA_L3 ~ 'L3 CSMA',
                                        
                                        SMRA_T5 ~ 'T5 SMRA',
                                        SMRA_T8 ~ 'T8 SMRA',
                                        SMRA_T10 ~ 'T10 SMRA',
                                        SMRA_L3 ~ 'L3 SMRA',
                                        
                                        SMI_T5 ~ 'T5 SMI',
                                        SMI_T8 ~ 'T8 SMI',
                                        SMI_T10 ~ 'T10 SMI',
                                        SMI_L3 ~ 'L3 SMI',
                                        
                                        SMG_T5 ~ 'T5 SMG',
                                        SMG_T8 ~ 'T8 SMG',
                                        SMG_T10 ~ 'T10 SMG',
                                        SMG_L3 ~ 'L3 SMG'
                             ),
                             
                             type=list(
                               c(CSMA_T5,   CSMA_T8,   CSMA_T10,  CSMA_L3,   SMRA_T5,  SMRA_T8,  SMRA_T10, SMRA_L3 ,
                                 SMI_T5,   SMI_T8,   SMI_T10,  SMI_L3,   SMG_T5,   SMG_T8,   SMG_T10,  SMG_L3) ~ 'continuous2'),
                             
                             digits = list(all_continuous2() ~ 1),
                             
                             statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({IQR})"),#, "{mean_minus_2_sd}"),
                             
                             missing='no',#Can do this, because missing values are only introduced by widening
                             missing_text = "(Missing)"
                             
                 ) %>%
                 
                 modify_header(all_stat_cols() ~ "**{level}**, N={n}")%>% 
                 #modify_footnote(all_stat_cols() ~ "Mean (SD), Median (IQR) or Frequency (%)") %>%
                 modify_caption(paste("**Table 2. Reference Values", title_name, "**",sep='')) %>%
                 
                 add_stat_label() %>%
                 bold_labels() %>%
                 italicize_levels() ,
               .header =  "**{strata}**, N = {c(length(unique((table2_df_wide[table2_df_wide$Sex == as.character(strata[1]), ])$patient_id)), length(unique((df[df$Sex == as.character(strata[2]), ])$patient_id))) }"
    )
  #Rename mean minus 2 sd:
  table_2_long[["table_body"]][['label']] <- gsub('mean_minus_2_sd', 'Mean - 2 SD', table_2_long[["table_body"]][['label']])
  
  
  #Could split this table into two parts, one CSMA and SMI, the other SMRA and SMG for the appendix.
  table_2_long
  
  #Long table with only CSMA and SMI: Table 2:
  table_2_long_CSMA_SMI <- table2_df_wide %>% dplyr::select(-patient_id, -ID, -IDTYPE, -contains('SMRA'), -contains('SMG')) %>%
    tbl_strata(strata=Sex,
               .tbl_fun = 
                 ~ .x %>%
                 tbl_summary(by=decade,
                             
                             label=list(CSMA_T5 ~ 'T5 CSMA',
                                        CSMA_T8 ~ 'T8 CSMA',
                                        CSMA_T10 ~ 'T10 CSMA',
                                        CSMA_L3 ~ 'L3 CSMA',
                                        
                                  
                                        
                                        SMI_T5 ~ 'T5 SMI',
                                        SMI_T8 ~ 'T8 SMI',
                                        SMI_T10 ~ 'T10 SMI',
                                        SMI_L3 ~ 'L3 SMI'
                                        
                                        
                             ),
                             
                             type=list(
                               c(CSMA_T5,   CSMA_T8,   CSMA_T10,  CSMA_L3,  
                                 SMI_T5,   SMI_T8,   SMI_T10,  SMI_L3) ~ 'continuous2'),
                             
                             digits = list(all_continuous2() ~ 1),
                             
                             statistic = all_continuous() ~ c("{get_n}","{mean} ({sd})", "{median} ({IQR})"),#, "{mean_minus_2_sd}"),
                             
                             missing='no',#Can do this, because missing values are only introduced by widening
                             missing_text = "(Missing)"
                             
                 ) %>%
                 
                 modify_header(all_stat_cols() ~ "**{level}**, N={n}")%>% 
                 #modify_footnote(all_stat_cols() ~ "Mean (SD), Median (IQR) or Frequency (%)") %>%
                 modify_caption(paste("**Table 2. Age-specific reference values for cross-sectional muscle area (CSMA) and skeletal muscle index (SMI)", title_name, "**", sep='')) %>%
                 
                 add_stat_label() %>%
                 bold_labels() %>%
                 italicize_levels() ,
               .header =  "**{strata}**, N = {c(length(unique((table2_df_wide[table2_df_wide$Sex == as.character(strata[1]), ])$patient_id)), length(unique((df[df$Sex == as.character(strata[2]), ])$patient_id))) }"
    )
  #Rename mean minus 2 sd:
  table_2_long_CSMA_SMI[["table_body"]][['label']] <- gsub('mean_minus_2_sd', 'Mean - 2 SD', table_2_long_CSMA_SMI[["table_body"]][['label']])
  #Rename get_n
  table_2_long_CSMA_SMI[["table_body"]][['label']] <- gsub('get_n', 'N', table_2_long_CSMA_SMI[["table_body"]][['label']])
  #Remove .00 values
  for(i in 1:5){
    for(j in 1:2){
      key <- paste('stat', i, j, sep='_')
      table_2_long_CSMA_SMI[["table_body"]][[key]]<- gsub('.00', '', table_2_long_CSMA_SMI[["table_body"]][[key]])
    }
  }
  
  table_2_long_CSMA_SMI
  
  #And the same for Appendix 4 with SMRA and SMG
  table_2_long_SMRA_SMG <- table2_df_wide %>% dplyr::select(-patient_id, -ID, -IDTYPE, -contains('SMI'), -contains('CSMA')) %>%
    tbl_strata(strata=Sex,
               .tbl_fun = 
                 ~ .x %>%
                 tbl_summary(by=decade,
                             
                             label=list(SMRA_T5 ~ 'T5 SMRA',
                                        SMRA_T8 ~ 'T8 SMRA',
                                        SMRA_T10 ~ 'T10 SMRA',
                                        SMRA_L3 ~ 'L3 SMRA',
                                        
                                        
                                        
                                        SMG_T5 ~ 'T5 SMG',
                                        SMG_T8 ~ 'T8 SMG',
                                        SMG_T10 ~ 'T10 SMG',
                                        SMG_L3 ~ 'L3 SMG'
                                        
                                        
                             ),
                             
                             type=list(
                               c(SMRA_T5,   SMRA_T8,   SMRA_T10,  SMRA_L3,  
                                 SMG_T5,   SMG_T8,   SMG_T10,  SMG_L3) ~ 'continuous2'),
                             
                             digits = list(all_continuous2() ~ 1),
                             
                             statistic = all_continuous() ~ c("{get_n}","{mean} ({sd})", "{median} ({IQR})"),#, "{mean_minus_2_sd}"),
                             
                             missing='no',#Can do this, because missing values are only introduced by widening
                             missing_text = "(Missing)"
                             
                 ) %>%
                 
                 modify_header(all_stat_cols() ~ "**{level}**, N={n}")%>% 
                 #modify_footnote(all_stat_cols() ~ "Mean (SD), Median (IQR) or Frequency (%)") %>%
                 modify_caption(paste("**Table 2. Age-specific reference values for mean skeletal muscle radio-attenuation (SMRA) and skeletal muscle gauge (SMG)", title_name, "**", sep="")) %>%
                 
                 add_stat_label() %>%
                 bold_labels() %>%
                 italicize_levels() ,
               .header =  "**{strata}**, N = {c(length(unique((table2_df_wide[table2_df_wide$Sex == as.character(strata[1]), ])$patient_id)), length(unique((df[df$Sex == as.character(strata[2]), ])$patient_id))) }"
    )
  #Rename mean minus 2 sd:
  table_2_long_SMRA_SMG[["table_body"]][['label']] <- gsub('mean_minus_2_sd', 'Mean - 2 SD', table_2_long_SMRA_SMG[["table_body"]][['label']])
  #Rename get_n
  table_2_long_SMRA_SMG[["table_body"]][['label']] <- gsub('get_n', 'N', table_2_long_SMRA_SMG[["table_body"]][['label']])
  #Remove .00 values
  for(i in 1:5){
    for(j in 1:2){
      key <- paste('stat', i, j, sep='_')
      table_2_long_SMRA_SMG[["table_body"]][[key]]<- gsub('.00', '', table_2_long_SMRA_SMG[["table_body"]][[key]])
    }
  }
  
  table_2_long_SMRA_SMG
  
  #Option 2 one table for each sex:
  
  partial_stats_table2 <- function(df, sex){
    tbl <- df %>%
      filter(Sex == sex) %>% 
      dplyr::select(-patient_id, -ID, -IDTYPE, -Sex) %>%
      tbl_strata(strata=vertebral_level,
                 .tbl_fun = 
                   ~ .x %>%
                   tbl_summary(by=decade,
                               
                               label = list(
                                 
                               ),
                               
                               type = list(
                                 c(CSMA, SMRA, SMI, SMG) ~ 'continuous2'
                               ),
                               
                               digits = list(all_continuous2() ~ 1),
                               
                               statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({IQR})"),#, "{mean_minus_2_sd}"),
                               
                               missing_text = "(Missing)",
                               missing = 'no'
                               
                   )
                 %>%
                   bold_labels()%>%
                   italicize_levels() %>%  
                   add_stat_label(label=all_continuous() ~ c('Mean (SD)', 'Median (IQR)' , 'Mean - 2*SD'))
                 ,
                 .header =  "**{strata}**, N = {n}") %>% modify_caption(paste(sep = '', '**Table 2', ifelse(sex=='Male', 'B', 'A'), '. Age-specific reference values for skeletal muscle in ', sex, title_name, '**'))
    
    #Rename mean minus 2 sd:
    tbl[["table_body"]][['label']] <- gsub('mean_minus_2_sd', 'Mean - 2 SD', tbl[["table_body"]][['label']])
    
    
    return(tbl)
  }
  
  male_tbl <- table2_df %>% partial_stats_table2('Male') 
  
  female_tbl <- table2_df %>% partial_stats_table2('Female')
  
  
  male_tbl
  female_tbl
  
  ###########
  # Save various options
  
  save_table <- function(table, table_name){
    saving_path <- paste(root_path, table_name, saving_name, '.html', sep='')
    gtsave(as_gt(table), saving_path)
  }
  
  save_table(male_tbl, 'ref_values_male')
  save_table(female_tbl, 'ref_values_female')
  
  save_table(table_2_long, 'ref_values_all')
  save_table(table_2_long_CSMA_SMI, 'ref_values_CSMA_SMI')
  save_table(table_2_long_SMRA_SMG, 'ref_values_SMRA_SMG')
  
}


