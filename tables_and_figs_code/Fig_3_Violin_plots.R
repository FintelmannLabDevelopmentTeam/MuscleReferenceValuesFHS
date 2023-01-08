require(tidyverse)
source('filter_merge_full_data.R')
require("ggpubr")



plot_fig_3 <- function(df = NULL, root_path='figures/', name='', height=297/2, width=210, dpi = 600, units="mm",
                       flip_axis = F,
                       mark_minus_2_SD_or_3rd_percentile = 'mean_minus_2_sd',
                       legend.position = 'none',
                       scales='free_y',
                       save_as_individual_plots_only = F,
                       split_in_pairs_of_two = T){
  #' Create and save Figure 3: Violin plots
  #' 
  #' @param df data.frame The dataframe with cohort data. Will be calculated, if NULL.
  #' @param root_path character The root path to where the figures should be saved.
  #' @param name character Which name the saved files should have.
  #' @param height numeric The height of the saved figure. (Din A4 is 297 x 210 mm)
  #' @param width numeric The width of the saved figure.
  #' @param dpi numeric The dots per inch of the saved figure. 
  #' @param units character The metric of height and width. May be "in", "cm", "mm", "px"
  #' @param flip_axis logical Whether to flip the axis.
  #' @param mark_minus_2_SD_or_3rd_percentile character Marks mean minus 2 SD with vertical red bar, if equal to 'mean_minus_2_sd'. Marks 3rd percentile, otherwise.
  #' @param legend.position character The legend.position attribute. none for no legend. bottom for bottom legend, ...
  #' @param scales character The scales parameter of facet_grid. Indicates whether scaling can be different between subplots.
  #' @param save_as_individual_plots_only logical Whether plots should be saved only individually for each metric, or also combined in one larger plot.
  #' @param split_in_pairs_of_two logical Whether to save plots in pairs of two (CSMA on top of SMI, and SMRA on top of SMG)
  
  #Load data, if none is provided:
  
  if(is.null(df)){
    #Gather data.
    merged_datasheet <- filter_merge_full_data()
    
    #Exclude cancer
    merged_datasheet_no_cancer <- exclude_medical_reasons(merged_datasheet, c('exclude_cancer'))
    
    #Exclude based on QA
    questions <- get_questions()
    df_excluded <- exclude_rows_based_on_questions(merged_datasheet_no_cancer, questions[['questions']], questions[['exclusion_questions']]
                                                         , questions[['muscle_exclusion_questions']], questions[['sat_exclusion_questions']])
    
    #Reduce to one series per participant, preferring the more recent CT2.
    df <- reduce_to_one_study_per_participant(df, prefer_CT = 2)
  }
  
  
  #Some data tidying: renaming and selecting only necessary columns.
  fig_3_data <- df %>% 
    dplyr::select(ID, IDTYPE, vertebral_level, decade, Sex, vertebral_level, muscle_area_cm2, muscle_mean_hu, skeletal_muscle_index, skeletal_muscle_gauge, CT_number, patient_id) %>% 
    dplyr::rename(CSMA = muscle_area_cm2, SMI = skeletal_muscle_index, SMG = skeletal_muscle_gauge, SMRA = muscle_mean_hu)
 
   
  #Make sure that female is on top:
  fig_3_data$Sex <- factor(fig_3_data$Sex, levels = c("Female", "Male"))
  
  #Helper functions for creating the violin plots:
  
  #Red horizontal bar
  mean_minus_2_sd <- function(x, na.rm=FALSE){return(mean(x, na.rm=na.rm)-2*sd(x, na.rm=na.rm))}
  third_percentile <- function(x, na.rm=F){
    return(as.numeric(quantile(x, probs=c(.03)), na.rm=na.rm)[1])
  }
  
  red_vertical_bar <- ifelse(mark_minus_2_SD_or_3rd_percentile == 'mean_minus_2_sd', mean_minus_2_sd, third_percentile)
  
  add_separators <- function(plot, n = 4, color = 'black', linewidth = .05, linetype = 'dashed'){
    for(i in 1:n){
      xintercept = .5 + i
      plot <- plot + geom_vline(xintercept=xintercept, color=color, linetype=linetype, width = linewidth) 
    }
    return(plot)
  }
  
  violin_plot_fig3 <- function(fig_3_data, measure, title, ylab){
    #' Plot a measure as violin plot, stratified by decade, sex, and vertebral level.
    #' 
    #' @param fig_3_data data.frame The data to be plotted
    #' @param measure character The name of the column of the measure to be plotted
    #' @param title character The name of the plot
    #' @param ylab character The y label of the plot.
    #' 
    #' @return The plot
    
    #The metric to plot
    measure <- rlang::sym(measure)
    
    
    df_plot <- fig_3_data %>% dplyr::select(!!measure, vertebral_level, decade, Sex, CT_number)
    
    
    plot_violin <- df_plot %>% ggplot(mapping = aes(x=decade,  y = !!measure, fill = decade))  + 
      geom_violin(draw_quantiles=c(.03, .25, .50, .75), width=1, trim=F) + #3rd percentile is mean - 1.96 SD in symmetric distribution
      
      labs(x = 'Age (Years)', 
           y = ylab,
           #title = title,
           fill = 'Age (Years)') +
      theme_gray() + 
      #coord_fixed(ratio=.2) +
      
      scale_fill_manual(values=rev(c('#eff3ff',
        '#c6dbef',
        '#9ecae1',
        '#6baed6',
        '#3182bd'))) + #Values taken from brewer for six values, except the darkest
      theme(legend.position = legend.position) +
      #Mark mean with dot
      stat_summary(fun = mean, geom = "point", size=1, color='black',#aes(shape = decade, color = decade), 
                   position = position_dodge(width = 1),
                   show.legend = FALSE, inherit.aes = TRUE)+
      #Mark red vertical bar
      stat_summary(fun = red_vertical_bar, 
                   geom = "crossbar", 
                   width = 0.5,
                   size = .3,
                   #linetype = 'dashed',
                   colour = "red",#aes(shape = decade, color = decade), 
                   position = position_dodge(width = 1),
                   show.legend = FALSE) #+ geom_point(mapping=aes(x=decade, y=!!measure)) #To see how data truly falls
    
    if(flip_axis){
      plot_violin <- plot_violin + coord_flip() + 
      facet_grid(cols=vars(Sex), rows=vars(vertebral_level), scales = scales)}else{
        plot_violin <- plot_violin + 
          facet_grid(cols=vars(vertebral_level), rows=vars(Sex), scales=scales)
      }
    
    return(plot_violin)
  }
  
  
  plot_and_save <- function(metric = c('CSMA, SMI, SMRA, SMG'), title='', ylab=''){
    #' Plot and save the violin plot for a given metric.
    #' 
    #' @param metric character The metric.
    #' @param title character The title of the plot. The name of the cohort will be added to it, if given.
    #' @param ylab character The label of the y axis of the plot.
    
    if(name != ''){
      title <- paste(title, 'in', name)
    }
    
    #Plot
    plot_violin <- violin_plot_fig3(fig_3_data, metric, title, ylab)
    plot_violin
    
    #Save:
    saving_path <- paste(root_path, 'violin_', metric, '_', name, '.png', sep='')
    ggsave(saving_path, plot=plot_violin, height=height, width=width, units=units, dpi = dpi, limitsize=F)
    
    if(!save_as_individual_plots_only){
      return(plot_violin)
    }
  }
  
  #CSMA:
  plot_csma <- plot_and_save('CSMA', 'Cross-Sectional Muscle Area', expression('Cross-Sectional Muscle Area (cm'^2 * ')'))
  
  #SMI:
  plot_smi <- plot_and_save('SMI', 'Skeletal Muscle Index', expression('Skeletal Muscle Index (cm'^2 * '/m'^2*')'))
  
  #SMRA:
  plot_smra <- plot_and_save('SMRA', 'Skeletal Muscle Radio-Attenuation', 'Skeletal Muscle Radio-Attenuation (HU)')
 
  #SMG:
  plot_smg <- plot_and_save('SMG', 'Skeletal Muscle Gauge', expression('Skeletal Muscle Gauge (HU * cm'^2 * '/m'^2*')'))
  
  if(!save_as_individual_plots_only){
    
    if(!split_in_pairs_of_two){
      #Combine plots into one larger figure using ggarrange
      combined_fig <- ggarrange(
        plot_csma,
        plot_smi,
        plot_smra,
        plot_smg,
        labels = c("A", "B", "C", "D"),
        ncol=2,
        nrow=2,
        common.legend=T,
        legend=legend.position
      )
      saving_path <- paste(root_path, 'violin_all_metrics_', name, '.png', sep='')
      ggsave(saving_path, plot=combined_fig, height=height*2, width = width*2, units=units, dpi=dpi)
    }else{
        
      combined_fig_csma_smi <- ggarrange(
        plot_csma,
        plot_smi,
        labels=c("A", "B"),
        ncol=1, nrow=2,
        common.legend=T, legend=legend.position
      )
      saving_path <- paste(root_path, 'violin_csma_smi_', name, '.png', sep='')
      ggsave(saving_path, plot=combined_fig_csma_smi, height=height*2, width = width, units=units, dpi=dpi)
      
      combined_fig_smra_smg <- ggarrange(
        plot_smra,
        plot_smg,
        labels=c("A", "B"),
        ncol=1, nrow=2,
        common.legend=T, legend=legend.position
      )
      saving_path <- paste(root_path, 'violin_smra_smg_', name, '.png', sep='')
      ggsave(saving_path, plot=combined_fig_smra_smg, height=height*2, width = width, units=units, dpi=dpi)
      
      }
  }
}
