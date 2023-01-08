require(tidyverse)
source('filter_merge_full_data.R')
source('util/summarize_conditions.R')
source('util/util.R')
source('read_data/read_sas_and_apply_data_dict.R')
source('read_data/Read_FHS_data.R')


#df_main is created using FHS_Final_Analysis.R. It has  been filtered for exclusion during QA and cancer history, and reduced to 1 measurement per participant.
#merged_datasheet holds measurements and clinical data from all analyzed scans, regardless of exclusion. All these were examined during QA.
#merged_datasheet_no_cancer holds measurements and clinical data from all analyzed scans, where participants had no hx of malignancy.
#df_excluded holds measurements not excluded during QA from participants without history of malignancy.
#Other questions were already answered in CoauthorsAnalysis.R

merged_datasheet <- filter_merge_full_data()

#Exclude cancer
merged_datasheet_no_cancer <- exclude_medical_reasons(merged_datasheet, c('exclude_cancer'))

#Exclude based on QA
questions <- get_questions()
df_excluded <- exclude_rows_based_on_questions(merged_datasheet_no_cancer, questions[['questions']], questions[['exclusion_questions']]
                                               , questions[['muscle_exclusion_questions']], questions[['sat_exclusion_questions']])

#Update has_T5, has_T8, has_CT1, ..
df_excluded <- add_patient_level_data(df_excluded)

#Reduce to one series per participant, preferring the more recent CT2.
df_main <- reduce_to_one_study_per_participant(df_excluded, prefer_CT = 2)



#Read in official documentation of which scans were taken during the FHS.
#################
paths <- readr::read_csv('read_data/rename_sheets_basecamb/paths.csv')%>%
  dplyr::pull(path, key)
# CT Dates -- > Use this to select all the patients who got CT scans
path <- paths[['CTdate']]
CTdate <- read_sas_and_apply_data_dict(path, 'dd_CTdate.csv')

#Append race and ethnicity data to this
race_df <- read_sas_and_apply_data_dict(paths[['race']], 'dd_race.csv')
race_df <- handle_multi_race_categories(decode_race_ethnicity(race_df))
#... and sex and age data:
agesex <- read_sas_and_apply_data_dict(paths[['agesex']], 'dd_agesex.csv')
agesex$Sex <- factor(agesex$Sex, levels = c(1:2), labels = c("Male", "Female"))

CTdate <- CTdate %>%
  left_join(race_df, by = c("ID", "IDTYPE"))  %>% 
  left_join(agesex, by = c("ID", "IDTYPE")) %>%
  factorize_IDTYPE()

#End of Readin of official scan timepoint documentation
########################


#Start of Analysis
###################

#Q: What is the discrepancy between what should be there and what is there?
############

#Participants:


CTdate %>% distinct(ID, IDTYPE) %>% nrow() #Participants originally imaged
#Difference:
CTdate %>% distinct(ID, IDTYPE) %>% nrow() - merged_datasheet %>% distinct(ID, IDTYPE) %>% nrow()

#For scan numbers, assume 2 series per time point (5mm abdo and 2.5mm thoracic)
#How many timepoints were there?
CTdate$n_timepoints <- as.numeric(!is.na(CTdate$SCAN_DATE1)) + as.numeric(!is.na(CTdate$SCAN_DATE2))
sum(CTdate$n_timepoints)#On how many timepoints images were taken

#And to how many timepoint/images we had access:
merged_datasheet %>% distinct(ID, IDTYPE, study_date) %>% nrow()
#Multiply by 2 - we used only one 5mm abdo and 2.5mm thoracic scan per time point

#Q: Exclusion for cancer: How many timepoints do we have afterwards?
merged_datasheet_no_cancer %>% group_by(ID, IDTYPE) %>% 
  mutate(n_timepoints = length(unique(study_date))) %>%
  distinct(ID,IDTYPE, .keep_all=T) %>% 
  dplyr::pull(n_timepoints) %>%
  sum()

#Q: Of how many participants?
n_participants_post_cancer <- length(unique(merged_datasheet_no_cancer$patient_id))
n_participants_post_cancer

#Q: How many time points were dropped?
#dropped <- setdiff(merged_datasheet$patient_id, merged_datasheet_no_cancer$patient_id)
#dropped_df <- merged_datasheet %>% filter(patient_id %in% dropped) 
#dropped_df %>%
#  distinct(ID, IDTYPE, study_date, .keep_all=T) %>%
#  nrow()
#This above calculation is false, but it gives a valuable learning point.
#Cancer exclusion does not only exclude participants entirely, but also single timepoints (CT-2)
#of participants that developed cancer between CT-1 and CT-2.
merged_datasheet %>% distinct(ID, IDTYPE, study_date) %>% nrow() * 2 - 
  (merged_datasheet_no_cancer %>% group_by(ID, IDTYPE) %>% 
    mutate(n_timepoints = length(unique(study_date))) %>%
    distinct(ID,IDTYPE, .keep_all=T) %>% 
    dplyr::pull(n_timepoints) %>%
    sum()) * 2


#Q: How many scan timepoints had to be dropped at each level? 
##
lvls <- unique(merged_datasheet$vertebral_level)

for(lvl in lvls){
  lvl_df <- merged_datasheet_no_cancer %>% filter(vertebral_level == lvl)
  n_timepoints_post_cancer <- lvl_df %>% nrow()
  
  lvl_df_main <- df_main %>% filter(vertebral_level == lvl)
  n_measurements <- lvl_df_main %>% nrow()
  
  
  cat("\n",
      lvl,":\n",
      "N Timepoints: ", n_timepoints_post_cancer,
      "\nN scans dropped by choice of one scan: ", (n_timepoints_post_cancer - n_participants_post_cancer)*2,
      "\nN participants dropped by QA Exclusion: ", (n_participants_post_cancer - n_measurements), " (", round((1- n_measurements / n_participants_post_cancer) * 100, digits=1),  "%)",
      "\nN measurements available: ", n_measurements, " (", round((n_measurements / n_participants_post_cancer) * 100, digits=1),  "% of participants)",
      "\n", sep=""
      )
}

#Q:
##How many participants had at least one validated muscle measurement?
df_main %>% distinct(ID, IDTYPE) %>% nrow()
#And how many scans?
df_main %>% filter(vertebral_level == "L3") %>% nrow() +
df_main %>% filter(vertebral_level != "L3") %>% distinct(ID, IDTYPE) %>% nrow()


