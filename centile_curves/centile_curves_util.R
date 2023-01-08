library('gamlss')
library('tidyverse')
source('tables_and_figs_code/Fig_4_LMS_Curves.R')

#' This file holds utility functions around the gamlss package.
#' Used for e.g.,
#' visualization of centile plots in ggplot2
#' extraction of data from diagnostic plots



get_model <- function(metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), sex = c('Male', 'Female'), lvl =c('T5', 'T8', 'T10', 'T12')){
  #' Get from the models list the model for a given metric, sex, and vertebral level.
  #' 
  #'@param metric character Which metric to be used, e.g., CSMA.
  #'@param sex character Patient sex. e.g., Male
  #'@param lvl character The vertebral level, e.g., T5
  #'@param centiles numeric Which centiles should be plotted.
  #'
  #'@return The model.
  
  name <- paste(metric, sex, lvl, sep='_')
  return(models[[name]])
}
get_model_sheet <- function(metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), sex = c('Male', 'Female'), lvl =c('T5', 'T8', 'T10', 'T12')){
  #' Get a stratified datasheet on which a model should be trained for a given metric, sex, and vertebral level.
  #' 
  #'@param metric character Which metric to be used, e.g., CSMA.
  #'@param sex character Patient sex. e.g., Male
  #'@param lvl character The vertebral level, e.g., T5
  #'
  #'@return The stratified datasheet.
  
  sheet <- df_slim %>% filter(Sex == sex, vertebral_level == lvl)
  return(sheet)
}

get_centiles <- function(model, metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), sex = c('Male', 'Female'), lvl =c('T5', 'T8', 'T10', 'T12')){
  #' Show the centiles in their original implementation from gamlss.
  #' 
  #' @param model The model.
  #'@param metric character Which metric to be used, e.g., CSMA.
  #'@param sex character Patient sex. e.g., Male
  #'@param lvl character The vertebral level, e.g., T5
  #'@param centiles numeric Which centiles should be plotted.
  
  
  
  centiles(model, xvar=model$xvar, 
           cent=c(2.275, 10, 25, 50, 75, 90, 97.725),
           legend=TRUE,
           xlab='Age',
           ylab=metric,
           main=paste(metric, 'at', lvl, 'in', sex),
           plot=TRUE,
           points = TRUE,
           cex=.2,
           pch=20,#shape of points, see ?points
           col.centiles=c('#bdd7e7', '#6cafd8', '#3182bd', '#08519c', '#3182bd', '#6cafd8', '#bdd7e7'),#see colors(). Otherwise use hexcodes as strings, or number codes: #=c(1,2,3,4,5,6,7)+14, #want 2 in the middle(red, 4 is dark blue, 5 is turkis, then repeats itself)
           lwd.centiles=1,
           lty.centiles=1)
  
}




predict_patient <- function(metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), sex = c('Male', 'Female'), lvl =c('T5', 'T8', 'T10', 'T12'), age, measurement){
  #' Predict Z score and percentile of a given individual, given their age, sex, and measured metric.
  #'
  #'@param metric character Which metric to be used, e.g., CSMA.
  #'@param sex character Patient sex. e.g., Male
  #'@param lvl character The vertebral level, e.g., T5
  #'@param age numeric The patient age.
  #'@param measurement numeric What was measured during segmentation.
  
  
  model <- get_model(metric, sex, lvl)
  z <- gamlss::z.scores(model, y = measurement, x = age) 
  percentile <- pnorm(z)
  print(paste('Patient has z score', z, 'and is on percentile', percentile*100))
}

plot_centiles <- function(model, metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), sex = c('Male', 'Female'), lvl =c('T5', 'T8', 'T10', 'L3'), centiles=c(2.275, 10, 25, 50, 75, 90, 97.725), points=TRUE, legend.position='bottom'){
  #'Plot the centiles in ggplot
  #'
  #'@param model The model
  #'@param metric character Which metric to be used, e.g., CSMA.
  #'@param sex character Patient sex. e.g., Male
  #'@param lvl character The vertebral level, e.g., T5
  #'@param centiles numeric Which centiles should be plotted.
  #'@param points logical Whether the datapoints should also be plotted.
  #'@param legend.position character Where to display the legend.
  
  centile_data <- get_plotting_dataframe(model, metric, centiles)
  
  #Color
  col.centiles=c('#bdd7e7', '#6cafd8', '#3182bd', 'orange', '#3182bd', '#6cafd8', '#bdd7e7')#see colors(). Otherwise use hexcodes as strings, or number codes: #=c(1,2,3,4,5,6,7)+14, #want 2 in the middle(red, 4 is dark blue, 5 is turkis, then repeats itself)
  
  
  #Plot with lines
  plot <- ggplot() + 
    geom_line(data=centile_data, aes_string(x='Age',
                                            y=metric,
                                            colour='Percentile'),
              size=1.5)+
    labs(x = 'Age', 
         y = metric,
         title = paste(metric, 'at', lvl, 'in', sex),
         colour = 'Percentile')+
    coord_cartesian(clip='off') +
    theme_gray() + 
    scale_colour_brewer(palette = 'Blues', type='seq') +
    theme(legend.position = legend.position) +
    scale_colour_manual(values=col.centiles)
  
  
  #Add points
  if(points){
    xvar <- model$xvar
    oxvar <- xvar[order(xvar)] #sort in ascending order
    oyvar <- model$y[order(xvar)] #model$y probably observations
    plot <- plot + geom_point(data=data.frame(Age=oxvar, metric=oyvar), aes(x=Age, y=metric),
                              size=.5,
                              colour='gray')
  }
  
  
  plot
}


compare_centiles <- function(models, model_names = NULL, metric = c('CSMA', 'SMI', 'SMRA', 'SMG'), centiles = c(3, 15, 50, 85, 97) ){
  #' Plot centiles of different models next to each other
  #' 
  #' @param models list A list of models
  #' @param model_names character A vector with names of the models
  #' @param centiles numeric Which centile curves should be compared.
  
  #Create datasheet with points for various centiles and models
  if(is.null(model_names)){
    model_names <- 1:length(models)
  }
  
  centile_data <- get_plotting_dataframe(models[[1]], metric, centiles)
  centile_data$model <- model_names[1]
  
  for(i in 2:length(models)){
    model <- models[[i]]
    model_name <- model_names[i]
    model_centile_data <- get_plotting_dataframe(model, metric, centiles)
    model_centile_data$model <- model_name
    
    centile_data <- rbind(centile_data, model_centile_data)
  }
  
  metric <- rlang::sym(metric)
  
  centile_data$model <- factor(centile_data$model)
  
  plot <- ggplot(centile_data, 
         aes(x=Age, y=!!metric, color = model)
         ) +
    geom_line() +
    facet_grid(rows = 'Percentile')
  
  return(plot)
}



##############################################
#Objective comparison of models for ideal model selection



###############Extracting information from the wormplot
#################
wormplot_info <- function(model, 
                          n.inter = 16,
                          xlim.all = 4,#how wide the plot will be
                          ylim.all = 12 * sqrt(1/length(resid)), #how high the plot will be
                          level = .95, #Confidence Interval
                          cex = 1 ,
                          cex.lab = 1,
                          pch = 21, #Shape of the points
                          bg = "wheat", #Background color of the plotted points
                          overlap = 0,
                          show.given = T,
                          xlim.worm = 3.5
){
  #The following code is based on the gamlss::wp function. It is useful to extract numeric ratings based on the plots.
  
  #Return values:
  total_outside_CI <- 0 #How many points fall out of 95% Confidence Interval
  sum_absolute_distance_from_0 <- 0 #What is the average distance to the central line.
  
  wp_info_results <- list()
  
  resid <- resid(model)
  
  #if only one interval:
  if(n.inter == 1){
    #get values of qq plot
    qq <- as.data.frame(qqnorm(resid, plot = FALSE))
    #detrend them
    qq$y <- qq$y - qq$x # should be 0, detrended(?)
    
    #Build 95% CI ines
    lz <- -xlim.all
    hz <- xlim.all
    dz <- 0.25
    z <- seq(lz, hz, dz) #vector from lowest point on x axis to highest in steps of dz (.25)
    p <- pnorm(z) #normal distribution function. dnorm is normal density function
    se <- (1/dnorm(z)) * (sqrt(p * (1 - p)/length(qq$y)))
    low <- qnorm((1 - level)/2) * se
    high <- -low
    if (any(abs(qq$y) > ylim.all)) {
      warning("Some points are missed out ", "\n", "increase the y limits using ylim.all")
    }
    if (any(abs(qq$x) > xlim.all)) {
      warning("Some points are missed out ", "\n", "increase the x limits using xlim.all")
    }
    
    #plot the points
    plot(qq$x, qq$y, ylab = "Deviation", xlab = "Unit normal quantile", 
         xlim = c(-xlim.all, xlim.all), ylim = c(-ylim.all, 
                                                 ylim.all), cex = cex, pch = pch, bg = bg, cex.lab = cex.lab)
    #plot cosmetics
    grid(lty = "solid")
    abline(0, 0, lty = 2, col = col)
    abline(0, 1e+05, lty = 2, col = col)
    lines(z, low, lty = 2)
    lines(z, high, lty = 2)
    
    
    #Get how many points fall out of 95% CI
    
    #Calculate 95% CI values for any point of the worm plot and check whether they fall below or within
    sorted_qq <- qq %>% arrange(x)
    p <- pnorm(sorted_qq$x) #normal distribution function. dnorm is normal density function
    se <- (1/dnorm(sorted_qq$x)) * (sqrt(p * (1 - p)/length(sorted_qq$y)))
    
    lower_curve <- qnorm((1 - level)/2) * se
    
    lines(sorted_qq$x, lower_curve, lty=2)
    higher_curve <- -lower_curve
    
    points_outside_CI <- sorted_qq$y < lower_curve | sorted_qq$y > higher_curve
    print(paste(sum(points_outside_CI), '/', length(points_outside_CI), 'were outside of the Confidence Interval. That is', round(sum(points_outside_CI)/length(points_outside_CI), 4)*100, '% of all points.'))
    
    #color those points in red
    points_outside <- (qq %>% dplyr::arrange(x))[points_outside_CI, ] 
    points(points_outside, col='red', bg='red', lw=5)
    
    total_outside_CI <- sum(points_outside_CI)
    sum_absolute_distance_from_0 <- sum(abs(sorted_qq$y))
    
    
  }else{ #Now with multiple intervals
    xvar <- model[['xvar']]
    
    w <- model$weights
    if (all(trunc(w) == w)) 
      xvar <- rep(xvar, w)
    
    #Get model intervals as in wp function
    xcut.points <- NULL
    given.in <- co.intervals(xvar, number = n.inter, 
                             overlap = overlap)
    
    
    if (overlap == 0) {
      check.overlap <- function(interval) {
        if (!is.matrix(interval)) {
          stop(paste("The interval specified is not a matrix."))
        }
        if (dim(interval)[2] != 2) {
          stop(paste("The interval specified is not a valid matrix.\nThe number of columns should be equal to 2."))
        }
        crows = dim(interval)[1]
        for (i in 1:(crows - 1)) {
          if (!(abs(interval[i, 2] - interval[i + 1, 1]) < 
                1e-04)) {
            interval[i + 1, 1] = interval[i, 2]
          }
        }
        return(interval)
      }
      
      given.in <- check.overlap(given.in)
    }
    
    
    
    total.points <- 0
    coef <- coef1 <- NULL
    y.y <- resid
    x.x <- resid
    
    
    ylim.worm = 12 * sqrt(n.inter/length(resid))
    
    qqs <- list()
    
    fct_call_n <- 1 # Iteration of the panel.fun function from the coplot function
    par(mar = c(1, 1, 1, 1))
    
    
    #Function for each individual plot, documentation see above in similar function part.
    panel.fun <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
                          cex = par("cex"), col.smooth = "red", span = 2/3, iter = 3, line = T, show_plot = T,
                          ...) {
      qq <- as.data.frame(qqnorm(y, plot = FALSE))
      qqs[[length(qqs)+1]] <- qq
      
      
      if (any(is.infinite(qq$y))) 
        line <- FALSE
      qq$y <- qq$y - qq$x
      if(show_plot)
      {grid(nx = NA, ny = NA, lwd = 2)
      points(qq$x, qq$y, pch = pch, col = col, bg = bg, cex = cex)
      abline(0, 0, lty = 2, col = col)
      abline(0, 1e+05, lty = 2, col = col)}
      yuplim <- 10 * sqrt(1/length(qq$y))
      level <- 0.95
      lz <- -xlim.worm
      hz <- xlim.worm
      dz <- 0.25
      z <- seq(lz, hz, dz)
      p <- pnorm(z)
      se <- (1/dnorm(z)) * (sqrt(p * (1 - p)/length(qq$y)))
      low <- qnorm((1 - level)/2) * se
      high <- -low
      {
        no.points <- length(qq$y)
        total.points <<- total.points + no.points
        no.mis <- sum(abs(qq$y) > ylim.worm)
        cat("number of missing points from plot=", no.mis, 
            " out of ", no.points, "\n")
        if (any(abs(qq$y) > ylim.worm)) 
          warning("Some points are missed out ", "\n", 
                  "increase the y limits using ylim.worm")
      }
      if (any(abs(qq$x) > xlim.worm)) {
        warning("Some points are missed out ", "\n", "increase the x limits using xlim.worm")
      }
      if(show_plot){
      lines(z, low, lty = 2, lwd = 0.5)
      lines(z, high, lty = 2, lwd = 0.5)
      }
      #Calculate how many points fall out of CI, same as above.
      sorted_qq <- qq %>% arrange(x)
      p <- pnorm(sorted_qq$x) #normal distribution function. dnorm is normal density function
      se <- (1/dnorm(sorted_qq$x)) * (sqrt(p * (1 - p)/length(sorted_qq$y)))
      
      lower_curve <- qnorm((1 - level)/2) * se
      
      if(show_plot){
      lines(sorted_qq$x, lower_curve, lty=2)}
      higher_curve <- -lower_curve
      
      points_outside_CI <- sorted_qq$y < lower_curve | sorted_qq$y > higher_curve
      
      wp_info_results[[paste('wp_outside_CI_age_', round(given.in[fct_call_n, 1],1), '-', round(given.in[fct_call_n, 2],1), sep='')]] <- sum(points_outside_CI)
      
      wp_info_results <<- wp_info_results
      
      fct_call_n <<- fct_call_n + 1
        
      points_outside <- (qq %>% dplyr::arrange(x))[points_outside_CI, ] 
      if(show_plot){
      points(points_outside, col='red', bg='red', lw=5)}
      
      total_outside_CI <<- total_outside_CI + sum(points_outside_CI)
      sum_absolute_distance_from_0 <<- sum_absolute_distance_from_0 + sum(abs(sorted_qq$y))
      
      if (line == TRUE & show_plot) {
        fit <- lm(y ~ x + I(x^2) + I(x^3), data = qq)
        s <- spline(qq$x, fitted(fit))
        flags <- s$x > -2.5 & s$x < 2.5
        
        lines(list(x = s$x[flags], y = s$y[flags]), col = col, 
              lwd = 0.5)
        assign("coef1", coef(fit), envir = parent.frame(n = 3))
        assign("coef", c(coef, coef1), envir = parent.frame(n = 3))
      }
    }
    
    bar.bg <- c(num = "light blue")
    
    
    coplot(y.y ~ x.x | xvar, given.values = given.in, panel = panel.fun, 
           ylim = c(-ylim.worm, ylim.worm), xlim = c(-xlim.worm, 
                                                     xlim.worm), ylab = "Deviation", xlab = "Unit normal quantile", 
           show.given = show.given, bg = bg, pch = pch, cex = cex, 
           bar.bg = bar.bg, line=F)
    
  }
  
  print(paste('A total of', total_outside_CI, 'points fell out of the 95% CI'))
  wp_info_results$wp_total_outside_CI <- total_outside_CI
  wp_info_results$wp_mean_residual_distance_to_0 <- sum_absolute_distance_from_0/length(resid)
  
  return(wp_info_results)
}



######################
#See if distribution of z scores based on model is normal.
get_z_scores <- function(model, age = NULL, measures = NULL){
  #' Calculate z scores for measures based on a given model.
  #' 
  #' @param model The LMS model.
  #' @param age numeric The age values
  #' @param measures numeric The measured metrics corresponding to the age.
  #' 
  #' @return The z scores of the given pairs of age and measures.
  
  #Extract age and measured values from model, if no others are given.
  if(is.null(age)){
    age <- model$xvar
  }
  if(is.null(measures)){
    measures <- model$y
  }
  z <- gamlss::z.scores(model, y = measures, x = age)
  
  return(z)
}

check_normality <- function(values){
  #' Assess normality of a vector of values
  #' 
  #' @param values numeric The values to be checked.
  
  #Visualize distribution:
  histogram <- ggplot(data.frame(x=values), aes(x=x)) + geom_histogram()
  
  SD <- sd(values)
  Mean <- mean(values)
  
  #Test if mean is significantly different from 0
  #Since we want the mean to be 0, high p is good here.
  t <- t.test(x=values, alternative='two.sided', mu=0, conf.level=.95)
  print(t)
  p_value_t_test_mean_not_0 <- t$p.value
  
  #Kolmogorov-Smirnov Test for standard normality (N(0, 1)):
  #Null: values are drawn from N(0,1)
  #-> High p-values means that the Null is very probable
  ks <- ks.test(x=values, y = pnorm, alternative = "two.sided")
  print(ks)
  p_value_ks_test_distribution_not_normal <- ks$p.value
  
  #Shapiro Wilk Test for standard normality:
  #Null: values are drawn from normal distribution
  #-> High p-values mean that the null is very probable, i.e. that the data is normally distributed
  shapiro <- shapiro.test(values)
  print(shapiro)
  p_value_shapiro_test_distribution_not_normal <- shapiro$p.value
  
  #Return the results in a list:
  normality_check_results <- list(
    histogram = histogram,
    SD = SD,
    Mean = Mean,
    p_value_t_test_mean_not_0 = p_value_t_test_mean_not_0,
    p_value_ks_test_distribution_not_normal = p_value_ks_test_distribution_not_normal,
    p_value_shapiro_test_distribution_not_normal = p_value_shapiro_test_distribution_not_normal
  )
  
  return(normality_check_results)
}

check_model_z_for_normality <- function(model){
  #' Extract z scores of all data a model was trained on and check whether they are normally distributed.
  #' 
  #' @param model The model.
  #'
  #' @return The normality statistics
  
  zs <- get_z_scores(model)
  return(check_normality(zs))
}

compare_models <- function(models, gaic_penalties, wormplot_stats=T, round_digits = 2){
  #' Compile dataframe for comparison from list of models
  #' 
  #' @param models list A list of models
  #' @param gaic_penalties numeric A vector of gaic penalties of the same length as models, corresponding in order to the gaic penalties used for fitting the model.
  #' @param wormplot_stats logical Whether to add wormplot stats.
  #' @param round_digits numeric To how many digits to round values
  #' @return A dataframe providing metrics for each of the models for comparison, as well as the models.
  
  

  #Add AIC and BIC:
  AICs <- round(purrr::map_dbl(models, AIC), digits = round_digits)
  BICs <- round(purrr::map_dbl(models, BIC), digits = round_digits)
  
  #Create dataframe for comparison:
  comparison <- data.frame(gaic_penalty = gaic_penalties, AIC = AICs, BIC = BICs)
  
  
  #Extract degrees of freedom:
  #Models might have different parameters. Loop over all of them, and create
  #columns in the datasheet, once new parameters are encountered.
  dfs <- list()
  for(i in 1:length(models)){
    model <- models[[i]]
    params <- model$parameters
    params.df <- paste(params, '.df', sep='')
    for(param.df in params.df){
      if(!(param.df %in% colnames(comparison))){
        comparison[[param.df]]<-NA
      }
      comparison[i, param.df] <- round(model[[param.df]], digits = round_digits)
    }
  }
  
  #Extract worm plot stats
  if(wormplot_stats){
    wp_stats <- purrr::map(models, wormplot_info, n.inter=16)
    
    for(wp_stat_name in names(wp_stats[[1]])){
      comparison[[wp_stat_name]] <- purrr::map_dbl(wp_stats, function(wp_stat){
        return(round(wp_stat[[wp_stat_name]], round_digits))
      })
    }
    }
  #Extract Q statistics
  get_q_stats <- function(model){
    return(Q.stats(model, xvar=model[['xvar']], n.inter=10))
  }
  Q_stats <- purrr::map(models, get_q_stats)
  
 
  comparison$Q_stats <- Q_stats
  
  get_from_Q_stats <- function(Q_stat, row, col){
    return(round(Q_stat[row, col], digits = round_digits))
  }
  for(name in colnames(Q_stats[[1]])){
    if((!name == 'N')){
      comparison[[paste('Q_stats_TOTAL_', name, sep='')]] <- purrr::map_dbl(Q_stats, get_from_Q_stats,'TOTAL Q stats', name) 
      comparison[[paste('Q_stats_p_', name, sep='')]] <- purrr::map_dbl(Q_stats, get_from_Q_stats,'p-val for Q stats', name)
    }
  }
  
  #Test for normality of z scores calculated by the model:
  normality_check <- purrr::map(models, check_model_z_for_normality)
  #Add results to dataframe:
  for(normality_statistic in names(normality_check[[1]])){
    comparison[[paste('normality_check_', normality_statistic, sep='')]] <- 
      purrr::map(normality_check, function(nc){
        if(is.numeric(nc[[normality_statistic]])){
          return(round(nc[[normality_statistic]], digits = round_digits))
        }else{
            return(nc[[normality_statistic]])
          }
        
        })
    
  }
  
  #Add models to dataframe.
  comparison$model <- models
  
  return(comparison)
}
