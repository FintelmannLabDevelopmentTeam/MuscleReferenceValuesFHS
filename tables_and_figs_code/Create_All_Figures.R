require(tidyverse)
source('tables_and_figs_code/Fig_3_Violin_plots.R')
source('tables_and_figs_code/Fig_4_LMS_Curves.R')
source('tables_and_figs_code/Table1_App1_Cohort_Characteristics_Medical_Conditions.R')
source('tables_and_figs_code/Table2.R')



create_and_save_all_figures <- function(df, name, root_path = 'figures', LMS_models = NULL,
                                        fig3=T, cohort_summary=T, reference_values=T){
  #' Create and save all tables and figures for the analysis based on a dataframe and LMS models.
  #' 
  #' @description This function facilitates subgroup analyses by running everything based on a given dataframe and name
  #' 
  #' @param df data.frame The dataframe for the analysis.
  #' @param name character The name of the cohort described in df.
  #' @param root_path character The root path to the folder where the figures and tables will be saved.
  #' @param LMS_models list A list of LMS models named in the format "metric_sex_vertebral-level" (e.g., "CSA_Female_L3")
  #' @param fig3 logical Whether to create violin plots.
  #' @param cohort_summary logical Whether to create cohort summary tables.
  #' @param reference_values logical Whether to create reference_values tables.
  
  print(paste("Creating and saving figures for:", name))
  
  #Create saving path:
  saving_path <- paste(root_path, name, '', sep='/')
  #Create the directories
  dir.create(file.path(root_path, name))
  dir.create(file.path(root_path, name, 'violin_plots'))
  dir.create(file.path(root_path, name, 'LMS_curves'))
  dir.create(file.path(root_path, name, 'cohort_summaries'))
  dir.create(file.path(root_path, name, 'reference_values'))
  
  
  #################
  #Figures
  #################
  
  #Figure 1 is not plotted in R.
  #Figure 2 is not plotted in R.
  
  #Figure 3: Violin plots
  if(fig3){
    print('Generating Fig. 3')
    plot_fig_3(df, paste(saving_path, 'violin_plots/', sep='/'), name)  
  }
  
  #Figure 4: LMS reference curves
  
  if(!is.null(LMS_models)){
    print('Generating LMS figures')
    plot_LMS_models(LMS_models,
                    root_path = paste(saving_path, 'LMS_curves/', sep='/'),
                    name = name,
                    
                    centiles = rev(c(3, 15, 50, 85, 97)),
                    lwidth = .8,
                    point_size = .3
                    )
  }
  
  #################
  #Tables
  #################
  
  #Table 1, Cohort Characteristics:
  #&
  #Appendix 1, Medical Conditions and more detailed description of the cohort
  df <<- NULL #necessary for enumerating male and female in cohort summary tables.
  #Unelegant fix, but works.
  table2_df_wide <<- NULL #Same thing for table 2
  
  
  
  if(cohort_summary){
    print('Generating cohort summary tables')
    cohort_summary_tables(df, 
                          paste(saving_path, 'cohort_summaries/', sep='/'), 
                          name)
  }
  
  #Table 2: Reference values
  if(reference_values){
    print("Generating reference value tables")
    reference_value_tables(df, 
                           paste(saving_path, 'reference_values/', sep='/'), 
                           name)
  }
}
