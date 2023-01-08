require(png)
require(dplyr)

show_preview_of_row <- function(row, WD='path/to/FraminghamWD'){
  #' Show segmentation overlaid on dicom for a given datapoint.
  #' 
  #' @param row A row of a dataframe.
  #' @param WD character The path to the working directory of the quality assurance program.
  
  #Extract path of image and display it.
  rel_path <- row['split_img_path_in_working_dir']
  img <- readPNG(paste(WD, rel_path, sep=''))
  grid::grid.raster(img)
}

open_dicom_series <- function(row, WD='path/to/FraminghamWD'){
  #' Open DICOM series of a given row in RadiAnt dicom viewer.
  #' 
  #' @param row A row of a dataframe.
  #' @param WD character The path to the working directory of the quality assurance program.
                              
  #Extract from row the path to the series data, create command to run RadiAnt in that directory, and run it.
  rel_path <- row['SeriesDataLocation']
  abs_path <- paste(WD, rel_path, sep='\\')
  cmd <- paste('"C:/Program Files/RadiAntViewer64bit/RadiAntViewer.exe"', abs_path)
  print(paste('Running cmd:', cmd))
  system(cmd)
}


add_is_CT_1_or_2 <- function(df){
  #'Add column CT_number holding 1 or 2 based on with which date study date aligns.
  #'Also adds age of patient based on which was selected.
  #'
  #'@param df data.frame The dataframe to which the row should be added.
  

  #Match Study date of CT scan with records of CT1 or CT2 date
  df_is1 <- !(is.na(df$SCAN_DATE1)) & df$study_date == df$SCAN_DATE1
  df_is2 <- !(is.na(df$SCAN_DATE2)) &  df$study_date == df$SCAN_DATE2
  df[df_is1, 'CT_number'] <- 1
  df[df_is2, 'CT_number'] <- 2
  df$CT_number <- as.factor(df$CT_number)
  
  #Calculate patient Age by also choosing from CT1 or CT2 age
  df <- df %>% 
    mutate(age = ifelse(as.integer(CT_number) == 1, ageCT1, ageCT2))
  
  #Plot Age distribution: 
  ggplot(df, aes(x=age)) + geom_histogram()
  
  #Factorize age
  intervals <- c(30, 45, 55, 65, 75, 95)
  labels <- c('<45', '45-54', '55-64', '65-74', ">=75")
  print('Choosing the following intervals, following Hoffmann et al.')
  print(labels)
  decade <- cut(df$age, breaks = intervals, include.lowest = TRUE, right=FALSE, labels = labels, ordered_results = TRUE)
  if(sum(is.na(decade)) != 0){
    print('Error in putting ages into intervals. Intervals were chosen in a way that did not accomodate all values.')
  }
  
  decade <- factor(decade, levels=labels)
  df$decade <- decade
  df %>% group_by(decade) %>% count()
  return(df)
}

add_patient_level_data <- function(df){
  #'Add patient level data. Like has_T5, has_CT1, ..
  #'
  #'@param df data.frame The dataframe
  
  df <- df %>%
    group_by(patient_id) %>% distinct(patient_id, vertebral_level, study_date, .keep_all=T) %>%
    mutate(has_T5 = any(vertebral_level=="T5"),
           has_T8 = any(vertebral_level == "T8"),
           has_T10 = any(vertebral_level == "T10"),
           has_L3 = any(vertebral_level == "L3")) %>%
    mutate(has_CT1 = any(as.integer(CT_number) == 1),
           has_CT2 = any(as.integer(CT_number) == 2)) %>%
    mutate(two_CTs_available = has_CT1 & has_CT2)%>%
    ungroup()
  return(df)
}

add_muscle_metrics <- function(df){
  #'Add SMI and SMG
  #'
  #'@param df data.frame The dataframe.
  
  df <- df %>% mutate(skeletal_muscle_index = muscle_area_cm2/(Height_m**2)) %>%
    mutate(skeletal_muscle_gauge = muscle_mean_hu*skeletal_muscle_index)
  return(df)
}

get_numbered_vector <- function(str, n, sep='_'){
  #' Get vector with numbered chars, such as str1, str2, .., strn
  #' 
  #' @param str The prefix
  #' @param n The length of the final vector
  #' @param sep The separator
  #' 
  #' @return The numbered vector.
  
  vec <- c()
  for (i in 1:n){
    vec <- append(vec, paste(str, i, sep=sep))
  }
  return(vec)
}

factorize_IDTYPE <- function(df){
  #'factorize the IDTYPE (cohort) column of a dataframe and return it.
  #'
  #'@param df data.frame the Dataframe with IDTYPE column
  #'
  #'@return the factorized dataframe
  df$IDTYPE <- factor(as.numeric(df$IDTYPE), levels = c(1, 2, 3, 7, 72), 
                               labels = c('Offspring (Gen 2)', 'New Offspring Spouse', 'Gen 3', "Omni 1", "Omni 2"))
  return(df)
  }


