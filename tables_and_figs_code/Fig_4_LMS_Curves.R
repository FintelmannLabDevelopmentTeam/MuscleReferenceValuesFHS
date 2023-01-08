require(tidyverse)
require(gamlss)
require("ggpubr")


get_plotting_dataframe <-
  function(model, metric,
           centiles = c(2.275, 10, 25, 50, 75, 90, 97.725)) {
    #' A helper function which will extract from a given model the dataframe needed to plot it in ggplot2.
    #' The functionality was taken out of the source code of the original centiles function.
    #'
    #'@param model The LMS model for which to plot the centiles.
    #'@param metric character The metric for which to gather the data.
    #'@param centiles numeric Which centiles should be plotted.
    #'
    #'@return The dataframe for plotting the model in ggplot2.
    
    xvar <- model$xvar
    #order xvar and yvar
    oxvar <- xvar[order(xvar)] #sort in ascending order
    oyvar <- model$y[order(xvar)] #model$y probably observations
    
    #Get name of quantile function
    fname <- model$family[1] 
    qfun <- paste("q", fname, sep = "") 
    
    #Create dataframe with 3 columns: two linking age to predictions and the third to the corresponding percentile,
    #such that ggplot2 can later group by the percentile column
    centile_data <- data.frame()
    
    #Get function call for fitting centiles for a model based on amount of parameters
    lpar <- length(model$parameters)
    for (cen in centiles) {
      cent <- cen / 100
      #Get fitted data points for centile
      if (lpar == 1) {
        newcall <- call(qfun, cent, mu = fitted(model,
                                                "mu")[order(xvar)])
      }
      else if (lpar == 2) {
        newcall <- call(qfun,
                        cent,
                        mu = fitted(model,
                                    "mu")[order(xvar)],
                        sigma = fitted(model, "sigma")[order(xvar)])
      }
      else if (lpar == 3) {
        newcall <- call(
          qfun,
          cent,
          mu = fitted(model,
                      "mu")[order(xvar)],
          sigma = fitted(model, "sigma")[order(xvar)],
          nu = fitted(model, "nu")[order(xvar)]
        )
      }
      else {
        newcall <- call(
          qfun,
          cent,
          mu = fitted(model,
                      "mu")[order(xvar)],
          sigma = fitted(model, "sigma")[order(xvar)],
          nu = fitted(model, "nu")[order(xvar)],
          tau = fitted(model,
                       "tau")[order(xvar)]
        )
      }
      centile_fits <- eval(newcall)
      
      #effectively qBCPEo(cent, mu=fitted, ...)
      
      #Create new dataframe with Age and Percentile data (x and y) for all centiles in question
      centile_df <-
        data.frame(Age = oxvar, Percentile = as.character(cen))
      centile_df[[metric]] <- centile_fits
      centile_data <- rbind(centile_data, centile_df)
    }
    #Turn centile names into factor
    centile_data$Percentile <-
      factor(centile_data$Percentile, levels = centiles)
    return(centile_data)
  }


plot_LMS_models <-
  function(models,
           metrics = c('CSMA', 'SMI', 'SMRA', 'SMG'),
           metrics_ylabs = list(
             CSMA=expression('Cross-Sectional Muscle Area (cm'^2 * ')'),
             SMI = expression('Skeletal Muscle Index (cm'^2 * '/m'^2*')'),
             SMRA = 'Skeletal Muscle Radio-Attenuation (HU)',
             SMG = expression('Skeletal Muscle Gauge (HU * cm'^2 * '/m'^2*')')
               ),
           root_path = 'figures/',
           name = '',
           height=297/2,
           width=210, dpi = 600, units="mm",
           centiles = c(3, 15, 50, 85, 97),
           points = TRUE,
           lwidth = 1,
           point_size = .5,
           legend.position = 'right',
           scales = 'free_y',
           save_as_individual_plots_only = F,
           split_in_pairs_of_two = T) {
    #'Plot all centiles for a metric in ggplot, stratified by Sex and vertebral level.
    #'
    #' @param models list A list of LMS models named as "metric_sex_vertebral-level" (e.g., "SMI_Female_L3")
    #' @param metrics character Which metrics to be plotted, e.g., CSMA.
    #' @param metric_ylabs list ylabs corresponding to the metrics, stored in a list(metric=ylab)
    #' @param root_path character The root path to where the figures should be saved.
    #' @param name character Which name the saved files should have.
    #' @param height numeric The height of the saved figure. (Din A4 is 297 x 210 mm)
    #' @param width numeric The width of the saved figure.
    #' @param dpi numeric The dots per inch of the saved figure. 
    #' @param units character The metric of height and width. May be "in", "cm", "mm", "px"
    #' @param centiles numeric Which centiles should be plotted.
    #' @param points logical Whether the datapoints should also be plotted.
    #' @param lwidth numeric the width of the lines.
    #' @param point_size numeric The size of the points, if plotted.
    #' @param legend.position character Where to display the legend.
    #' @param scales character The scales parameter of facet_grid. Indicates whether scaling can be different between subplots.
    #' @param save_as_individual_plots_only logical Whether plots should be saved only individually for each metric, or also combined in one larger plot.
    #' @param split_in_pairs_of_two logical Whether to save plots in pairs of two (CSMA on top of SMI, and SMRA on top of SMG)
    
    
    
    #Create plot for all metrics:
    
    
    plot_and_save <- function(metric){
      Sex <- c('Male', 'Female')
      VertebralLevels <- c('L3', 'T10', 'T8', 'T5')
      
      #Create dataframe as a combination of dataframes used for plotting of all models (Male, Female, vertebral levels)
      curve_frame <- data.frame()
      for (sex in Sex) {
        for (lvl in VertebralLevels) {
          #print(paste('Gathering data for', metric, sex, lvl))
          #Get dataframe usable for plotting based on the correct model
          model_name <- paste(metric, sex, lvl, sep = '_')
          model <- models[[model_name]]
          df_strata <- get_plotting_dataframe(model, metric, centiles)
          
          #Add sex and vertebral level for later stratification in the plot.
          df_strata[['Sex']] <- sex
          df_strata[['VertebralLevel']] <- lvl
          #Combine plotting dataframes into bigger one.
          curve_frame <- rbind(curve_frame, df_strata)
        }
      }
      
      #make sure the order of levels is correct
      curve_frame$VertebralLevel <-
        factor(curve_frame$VertebralLevel, levels = c('T5', 'T8', 'T10', 'L3'))
      
      plot <- ggplot()
      
      #Add points
      if (points) {
        points_df <- data.frame()
        for (sex in Sex) {
          for (lvl in VertebralLevels) {
            
            model <- models[[paste(metric, sex, lvl, sep='_')]]
            xvar <- model$xvar
            
            
            oxvar <- xvar[order(xvar)] #sort in ascending order
            oyvar <- model$y[order(xvar)] #model$y probably observations
            
            data <- data.frame(Age = oxvar,
                               Sex = sex,
                               VertebralLevel = lvl)
            data[[metric]] <- oyvar
            points_df <- rbind(points_df, data)
            
          }
        }
        #Reorder vertebral levels
        points_df$VertebralLevel <-
          factor(points_df$VertebralLevel, levels = c('T5', 'T8', 'T10', 'L3'))
        
        
        plot <-
          plot + geom_point(
            data = points_df,
            aes_string(x = 'Age', y = metric),
            size = point_size,
            colour = 'gray'
          )
        
      }
      
      
      #Color Options. May decide on one.
      # col.centiles = c('#bdd7e7',
      #                  '#6cafd8',
      #                  '#3182bd',
      #                  '#08519c',
      #                  '#3182bd',
      #                  '#6cafd8',
      #                  '#bdd7e7')#see colors(). Otherwise use hexcodes as strings, or number codes: #=c(1,2,3,4,5,6,7)+14, #want 2 in the middle(red, 4 is dark blue, 5 is turkis, then repeats itself)
      # col.centiles = c('red',
      #                  '#6cafd8',
      #                  '#3182bd',
      #                  '#08519c',
      #                  '#3182bd',
      #                  '#6cafd8',
      #                  '#bdd7e7')#see colors(). Otherwise use hexcodes as strings, or number codes: #=c(1,2,3,4,5,6,7)+14, #want 2 in the middle(red, 4 is dark blue, 5 is turkis, then repeats itself)
      col.centiles <-
        c('#bdd7e7',
          '#6cafd8',
          '#3182bd',
          'orange',
          '#3182bd',
          '#6cafd8',
          '#bdd7e7')#see colors(). Otherwise use hexcodes as strings, or number codes: #=c(1,2,3,4,5,6,7)+14, #want 2 in the middle(red, 4 is dark blue, 5 is turkis, then repeats itself)
      
      #Adjust for amount of centiles
      col.centiles <- col.centiles[1+floor((length(col.centiles)-length(centiles))/2):length(col.centiles)]
      
      
      #Plot with lines
      plot <- plot +
        geom_line(
          data = curve_frame,
          aes_string(x = 'Age',
                     y = metric,
                     colour = 'Percentile'),
          size = lwidth
        )
      
      #Adjust title based on name:
      t <- paste('Age-Dependent', metric, 'Reference Curves in 38-80 year olds')
      if(name != ''){
        t <- paste(t, 'in', name)
      }
      
      #Add labels, grid, theme, color scale:
      plot <- plot +
        labs(
          x = 'Age (Years)',
          y = metrics_ylabs[[metric]],
          #title = t, #Was commented out for the final version. If a title is desired again, remove the #
          colour = 'Percentile'
        ) +
        coord_cartesian(clip = 'off') +
        theme_gray() +
        theme(legend.position = legend.position) +
        scale_colour_manual(values = col.centiles) +
        facet_grid(cols = vars(VertebralLevel), rows = vars(Sex), scales = scales)
      
      plot
      
      #Save it
      filepath = paste(root_path, 'LMS_curves_', metric, '_', name, '.png', sep='')
      ggsave(filename=filepath, plot=plot, height=height, width=width, units=units, dpi=dpi)
      
      #return the plot.
      return(plot)
    }
    
    #Plot and save all metrics
    plots <- list()
    for(metric in metrics){
      plots[[metric]] <- plot_and_save(metric)
    }
    
    if(!save_as_individual_plots_only){
      if(!split_in_pairs_of_two){
        all_plots <- ggarrange(
          plots[["CSMA"]],
          plots[["SMI"]],
          plots[["SMRA"]],
          plots[["SMG"]],
          labels = c("A", "B", "C", "D"),
          ncol=2,
          nrow=2,
          common.legend=T,
          legend=legend.position
        )
        saving_path <- paste(root_path, 'lms_all_metrics_', name, '.png', sep='')
        ggsave(saving_path, plot=combined_fig, height=height*2, width = width*2, units=units, dpi=dpi)
        
      }else{
        split_plots_csma_smi <- ggarrange(
          plots[["CSMA"]],
          plots[["SMI"]],
          labels = c("A", "B"),
          ncol=1,
          nrow=2,
          common.legend=T,
          legend=legend.position
        )
        saving_path <- paste(root_path, 'lms_csma_smi_', name, '.png', sep='')
        ggsave(saving_path, plot=split_plots_csma_smi, height=height*2, width = width, units=units, dpi=dpi)
        
        split_plots_smra_smg <- ggarrange(
          plots[["SMRA"]],
          plots[["SMG"]],
          labels = c("A", "B"),
          ncol=1,
          nrow=2,
          common.legend=T,
          legend=legend.position
        )
        saving_path <- paste(root_path, 'lms_smra_smg_', name, '.png', sep='')
        ggsave(saving_path, plot=split_plots_smra_smg, height=height*2, width = width, units=units, dpi=dpi)
      }
      }
    
    
  }
