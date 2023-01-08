##################################
#Dependencies
##################################
require(tidyverse)
require(gtsummary)
source('tables_and_figs_code/Create_All_Figures.R')
source('filter_merge_full_data.R')


##################################
#Load data (dataframes and LMS models)
##################################

##################################
#Create subgroups of the data
##################################

#Cohort for the Main Analysis:
#Exclusion of History of cancer, except non-malignant skin cancer
#Exclusion based on QA
#Reduction to one measurement per participant and vertebral level, preferring the more recent CT2
###################
#Gather data.
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


#Racial/Ethnic Subgroups of the main analysis dataframe:
race_ethnicity_subgroup_categories <- unlist(get_race_ethnicity_dict())
race_ethnicity_subgroups <- list()
sum_nrows <- 0
for(sg in race_ethnicity_subgroup_categories){
  race_ethnicity_subgroups[[sg]] <- df_main[df_main[[sg]],]
  sum_nrows <- sum_nrows + sum(df_main[[sg]])
}
print('Like this, some participants will appear in multiple subgroups. The amount of multiple reports is:')
print(sum_nrows - nrow(df_main))

#FHS cohort subgroups of the main analysis dataframe:
cohorts <- levels((df_main$IDTYPE))
cohort_subgroups <- list()

for(cohort in cohorts){
  cohort_df <- df_main %>% filter(IDTYPE == cohort)
  if(nrow(cohort_df)>0){
    cohort_subgroups[[cohort]] <- cohort_df
  }
}



#Load LMS models for main cohort:
load('centile_curves/main_cohort_reference_curve_models_2022-12-10.RData')
#Will load main_cohort_models, which were fitted as described in the appendix.



##################################
#Run main analysis
##################################
df <- NULL #necessary for enumerating male and female in cohort summary tables.
#Unelegant fix, but works.
table2_df_wide <- NULL #Same thing for table 2

#Main cohort:
create_and_save_all_figures(df_main, 'the main cohort2', LMS_models=main_cohort_models)

#Racial subgroups:
for(subgroup in names(race_ethnicity_subgroups)){
  print(paste('Generating figures for racial/ethnic subgroup:', subgroup))
  if(!(subgroup == 'Other' | subgroup == 'Race/Ethnicity not reported')){
    create_and_save_all_figures(race_ethnicity_subgroups[[subgroup]], paste(subgroup, 'participants'), 'figures',
                                LMS_models = NULL, cohort_summary = T, reference_values = F, fig3=F)
  }
  }

#FHS Cohort subgroups:
for(cohort in names(cohort_subgroups)){
  cohort_df <- cohort_subgroups[[cohort]]
  create_and_save_all_figures(cohort_df, cohort, 'figures',
                              LMS_models = NULL, cohort_summary = T, reference_values = F, fig3=F)
}