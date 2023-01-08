require(tidyverse)
require(gtsummary)
source('filter_merge_full_data.R')

#Get datasheet with exam data, assessed_df data, and some calculated patient level data.
merged_datasheet <- filter_merge_full_data()
merged_datasheet_no_cancer <- exclude_medical_reasons(merged_datasheet, c('exclude_cancer'))

#Exclude QA only
questions <- get_questions()
df_post_exclusion <- exclude_rows_based_on_questions(merged_datasheet_no_cancer, questions[['questions']], questions[['exclusion_questions']]
                                       , questions[['muscle_exclusion_questions']], questions[['sat_exclusion_questions']])

#QA exclusion description after cancer was excluded
summary <- summarize_qa(merged_datasheet_no_cancer, questions[['questions']], questions[['exclusion_questions']]
             , questions[['muscle_exclusion_questions']], questions[['sat_exclusion_questions']])

gtsummary::theme_gtsummary_compact()
gtsummary::theme_gtsummary_journal('jama')
summary %>% select(contains('_yes'), lvl) %>% 
  mutate(grainy_image_yes = lowdose_yes + reconstruction_filter_yes) %>% 
  select(-lowdose_yes, -reconstruction_filter_yes, -incomplete_sat_fov_yes) %>%
  select(#Reorder        
         level_unavailable_yes,
         segmentation_impossible_yes, 
         incomplete_muscle_fov_yes,
         touching_gantry_yes,    
         grainy_image_yes,              
         artifacts_yes,           
         edema_yes,                
         arms_down_unremovable_yes,  
           
         
         phantom_unremovable_yes,  
         segmentation_edited_yes, 
         level_selection_edited_yes, 
         
         iv_contrast_yes, 
         oral_contrast_yes, lvl) %>% 
  tbl_summary(
  by=lvl,
  type = c(iv_contrast_yes, 
           oral_contrast_yes,        
           level_unavailable_yes,    
           segmentation_impossible_yes, 
           grainy_image_yes,              
           artifacts_yes,           
           edema_yes,                
           arms_down_unremovable_yes,  
           touching_gantry_yes,      
             
           phantom_unremovable_yes,  
           segmentation_edited_yes, 
           level_selection_edited_yes, 
           incomplete_muscle_fov_yes 
           ) ~ 'continuous',
  label = list(iv_contrast_yes ~ 'I.V. Contrast', 
              oral_contrast_yes ~ 'Oral Contrast',        
              level_unavailable_yes ~ 'Vertebral Level Unavailable',    
              segmentation_impossible_yes ~ 'Segmentation Impossible Overall', 
              grainy_image_yes ~ 'Reason: Grainy Image Quality',              
              artifacts_yes ~ 'Reason: CT Artifacts',           
              edema_yes ~ 'Reason: Edema',                
              arms_down_unremovable_yes ~ 'Reason: Arms down and unremovable',  
              touching_gantry_yes ~ 'Reason: Touching the Gantry',      
              phantom_unremovable_yes ~ 'Reason: Phantom could not be removed',  
              segmentation_edited_yes ~ 'Segmentation Edited', 
              level_selection_edited_yes ~ 'Level Selection Edited', 
              incomplete_muscle_fov_yes ~ 'Reason: Incomplete Field of View'),
  statistic = list(all_continuous() ~"{mean}")
)


#How many participants were there initially?
merged_datasheet %>% distinct(ID, IDTYPE) %>% count()
#How many studies were there initially?
merged_datasheet %>% distinct(patient_id, study_date) %>% count()
#How many participants had two CT scans/timepoints initially?
merged_datasheet %>% distinct(ID, IDTYPE, .keep_all = T) %>%
  filter(two_CTs_available) %>% count()

# How many patients were dropped due to cancer?
merged_datasheet_no_cancer %>% distinct(patient_id) %>% count()
4421-3836
#How many studies were dropped due to cancer?
merged_datasheet_no_cancer %>% distinct(patient_id, study_date) %>% count()
6506 - 5532

#After cancer exclusion:
#How many participants?
merged_datasheet_no_cancer %>% distinct(patient_id) %>% count()
#How many scans/timepoints?
merged_datasheet_no_cancer %>% distinct(patient_id, study_date) %>% count()
#How many participants had two CT scans/timepoints
merged_datasheet_no_cancer %>% distinct(ID, IDTYPE, .keep_all = T) %>%
  filter(two_CTs_available) %>% count()

#QA exclusion on each vertebral level:
merged_datasheet_no_cancer %>% distinct(patient_id, study_date, vertebral_level, .keep_all=T) %>% group_by(vertebral_level) %>% count() %>%
  left_join(df_post_exclusion %>% group_by(vertebral_level) %>% count(), by=c('vertebral_level'), suffix=c('_pre_exclusion', '_post_exclusion')) %>%
  mutate(dropped_during_qa = n_pre_exclusion - n_post_exclusion)


#After QA and cancer exclusion:
#How many participants?
df_post_exclusion %>% distinct(patient_id) %>% count()
#How many scans/timepoints?
df_post_exclusion %>% distinct(patient_id, study_date) %>% count()
#How many participants had two CT scans/timepoints
df_post_exclusion %>% distinct(ID, IDTYPE, .keep_all = T) %>%
  filter(two_CTs_available) %>% count()


#Reduce to one metric, preferring CT2:
rcdf <- reduce_to_one_study_per_participant(df_post_exclusion, prefer_CT = 2)

#Investigate amount of scans dropped per level:
df_post_exclusion %>% group_by(vertebral_level) %>% count() %>% inner_join(
  rcdf %>% group_by(vertebral_level) %>% count(), by=c('vertebral_level'), suffix=c('_pre_reduction', '_post_reduction')) %>%
  mutate(n_dropped = n_pre_reduction - n_post_reduction)

#Investigate how many measurements we have per level
n_participants <- length(unique(rcdf$patient_id))
rcdf %>% group_by(vertebral_level) %>% count() %>%
  mutate(percentage_of_participants = round((n/n_participants)*100, digits=1))
