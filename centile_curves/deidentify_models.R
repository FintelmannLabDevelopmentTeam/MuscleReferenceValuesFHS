require(gamlss)
require(tidyverse)


deidentify_model <- function(model){
  #' Deidentify a LMS model.
  #' 
  #' @param model The LMS model
  #' 
  #' @return The deidentified model
  
  #Need to delete y (the measurements), and x (age) 
  #Residuals will hold residuals and need to be eliminated, too.
  #Rosenthal also deleted y, residuals, and xvar from their models.
  
  model$y <- NULL
  model$residuals <- NULL
  
  #replace x with a dummy:
  min_x <- min(model$xvar)
  max_x <- max(model$xvar)
  n_x <- length(model$xvar)
  model$xvar <- seq(min_x, max_x, length.out = n_x)
  
  #Need to replace fitted values as well to avoid poor curve fit:
  pred <- predict(model, newdata = model$xvar)
  
  qfun <- qBCPE
  model$mu.fv <- pred$mu
  model$sigma.fv <- pred$sigma
  model$nu.fv <- pred$nu
  model$tau.fv <- pred$tau
  
  return(model)
  
}


models_anon <- purrr::map(main_cohort_models, deidentify_model)

#Test that these give the same results:
z.scores(models_anon[[1]], 150, 50)
z.scores(main_cohort_models[[1]], 150, 50)

#Save models_anon. 
save(models_anon, file='models_anonymized_2022-12-10.RData')

