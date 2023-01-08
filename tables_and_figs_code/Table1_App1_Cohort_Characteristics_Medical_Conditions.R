require(gtsummary)
require(gt)
source('filter_merge_full_data.R')
library('tidyverse')
source('read_data/Read_FHS_data.R')
source('util/summarize_conditions.R')

cohort_summary_tables <- function (df = NULL,
                                   root_path = 'figures/',
                                   name = '')
{
  #' Create and save summary tables for a given cohort
  #' 
  #' @param df data.frame The cohort dataframe.
  #' @param root_path character The root path to where the figures should be saved.
  #' @param name character Which name the saved files should have.
  
  #Set layout for gtsummary
  gtsummary::theme_gtsummary_compact()
  gtsummary::theme_gtsummary_journal('jama')
  
  #Load and tidy data, if none is given:
  if(is.null(df)){
    df <- get_final_datasheet()
  }
  df$CT_number <- factor(df$CT_number, levels = c('1', '2'))
  
  #Relabel Gen 2/3:
  df$IDTYPE <- factor(df$IDTYPE, levels = c('Offspring (Gen 2)', 'Gen 3', "Omni 1", "Omni 2"), 
                      labels = c('Offspring (Generation 2)', 'Generation 3', "Omni 1", "Omni 2"))
  
  #Handle multi race categories:
  df <- handle_multi_race_categories(df, summarize_as = "Other / Not reported")
  
  remove_stat_labels_from_rows <- function(table){
    #'Remove the stat label from each row's caption
    #'e.g., do not show BMI, Mean(SD), but just BMI
    #'
    #'@param table list A gtsummary table.
    #'
    #'@return The table without the statistical labels.
    
    for(n in names(table$table_body)){
      
      if(grepl('stat_label', n)){
        table$table_body[[n]] <- rep(NA, length(table$table_body[[n]]))
      }
    }
    return(table)
  }
  
  
  
  #Select variables
  table_df2 <- df %>% choose_most_recent_participant_data() %>%
    dplyr::select(
      age,
      Sex,
      Height_m,
      Weight_kg,
      BMI,
      WaistGirthIn,
     # HandGripStrength1Right, 
    #  HandGripStrength1Left,
    #  QuickWalkS, 
    #  ChairStands,
      CurrentSmoker,
      Alcohol,
      Hypertension,
      Menopause,
      Diabetes,
      MetabolicSyndrome,
      FHS_Risk_Score_10yr_CVD,
      Physical_Activity_Index,
      Race,
      IDTYPE,
      -ID
          ) %>%
    mutate(
      Height_m = as.numeric(Height_m),
      Weight_kg = as.numeric(Weight_kg)
    )
  
  
  ########
  #Compact summary table, stratified by sex.
  
  table_1_sex <- table_df2 %>%
    
    tbl_summary(
      by = Sex,
      
      label = list(
        age ~ "Age",
        BMI ~ 'BMI',
        Height_m ~ 'Height (m)',
        Weight_kg ~ 'Weight (kg)',
        CurrentSmoker ~ 'Current Smoker',
        Alcohol ~ 'Drinking Alcohol',
        WaistGirthIn ~ 'Waist circumference (in)',
        #HandGripStrength1Right ~ 'Right Hand Grip Strength (nearest kg)', 
        ##HandGripStrength1Left ~ 'Left Hand Grip Strength (nearest kg)',
        #QuickWalkS ~ 'Quick Walk Time (s)', 
        #ChairStands ~ 'Time For 10 Chair Stands (s)',
        Menopause ~ 'Menopause',
        Diabetes ~ 'Diabetes',
        MetabolicSyndrome ~ 'Metabolic syndrome',
        FHS_Risk_Score_10yr_CVD ~ 'Framingham Risk Score',
        Physical_Activity_Index ~ 'Physical Activity Index',
        IDTYPE ~ "FHS Cohort",
        Race ~ 'Race and Ethnicity'
      ),
      
      type = list(
        c(Race, 
          IDTYPE) ~ "categorical",
        c(
          CurrentSmoker,
          Diabetes,
          MetabolicSyndrome,
          Alcohol,
          Menopause
        ) ~ "dichotomous",
        c(
          age,
          BMI,
          Height_m,
          Weight_kg,
          WaistGirthIn,
          FHS_Risk_Score_10yr_CVD,
          Physical_Activity_Index
        ) ~ "continuous"
      ),
      
      digits = list(contains("age") ~ 1),
      
      statistic = all_continuous() ~ c("{mean} ({sd})"),
      #, "{median} ({IQR})"),
      
      
      sort = all_categorical() ~ 'frequency',
      missing_text = "(Missing)",
      missing='no' #For compact table
      
    ) %>%
    
    modify_header(all_stat_cols() ~ "**{level}**, N={n}") %>%
    modify_footnote(
      all_stat_cols() ~ "Mean (Standard Deviation) for continuous and N (%) for categorical variables"
    ) %>%
   # modify_caption("**Table 1. Cohort Characteristics**") %>%
    
    #add_stat_label() %>%
    bold_labels() %>%
    italicize_levels() %>%
    remove_stat_labels_from_rows()
  
  ##################
  #Detailed table holding everything stratified by sex and vertebral level
  
  full_table_df <- df %>% dplyr::select(
    
    #Demographics and anthropometrics
    age,
    Sex,
    Height_m,
    Weight_kg,
    BMI,
    BSA,
    contains('Girth'),
    SagittalAbdominalDiameterCm,
    NeckCircumferenceIn,
    
    #Risk factors
    BPsystol,
    BPdiastol,
    receivesBPMeds,
    Hypertension,
    CurrentSmoker,
    Alcohol,
    Menopause,
    
    #Labs
    HDL,
    CHOL,
    GLUC,
    TRIG,
    
    #Important Medical conditions:
    Diabetes,
    
    #Indizes and Summaries:
    MetabolicSyndrome,
    FHS_Risk_Score_10yr_CVD,
    Physical_Activity_Index,
    #Physical Performance
    HandGripStrength1Right, HandGripStrength1Left,
    QuickWalkS, ChairStands,
    
    
    #Subgroups
    #all_of(unlist(get_race_ethnicity_dict(), use.names=F)), #Racial Groups
    Race,
    IDTYPE,
    CT_number,
    
    #CT
    slice_thickness_mm,
    
    
    
    #Medical history
    
    #CV
    Myocardial_Infarction_Hx,
    Angina_Pectoris_Hx,
    Coronary_Insufficiency,
    STROKE_TIA_Hx,
    IC_Intermittent_Claudication_Hx,
    HxDVT,
    CHF_Congestive_Heart_Failure_Hx,
    HeartValveSurgery,
    ThoracicAortaSurgery,
    AbdominalAortaSurgery,
    HxOtherCVProcedure,
    HxOfHospitalizationForHeartFailure,
    HistoryOfPE,
    AtrialFibrillationHx,
    SyncopeHx,
    
    #Metabolic and Endocrine
    ThyroidDisease,
    GallbladderDisease,
    HxGallbladderSurgery,
    
    #Other procedures
    FemoralLowerExtremitySurgery,
    
    #Neuro and Psych
    Dementia,
    Parkinson,
    SeizureHx,
    HxDepression,
    HxPsychosis,
    HxAnxiety,
    HxSpinalStenosis,
    
    #Pulmo
    LungFibrosis,
    Asthma,
    COPD,
    Emphysema,
    HxSleepApnea,
    
    #Reproductive
    HxHysterectomy,
    HxOvarectomy,
    HxProstateDisease,
    
    #Strata
    Sex, vertebral_level
    
    
  )
  
  
  #df needs to be placed in the global context.
  df <<- df
  
  table_1_detailed <- full_table_df %>%
    tbl_strata(
      strata = Sex,
      .tbl_fun =
        ~ .x %>%
        tbl_summary(
          by = vertebral_level,
          
          label = list(
            
            age ~ "Age",
            BMI ~ 'BMI',
            BSA ~ 'Body Surface area (cm2)',
            Height_m ~ 'Height (m)',
            Weight_kg ~ 'Weight (kg)',
            BPsystol ~ 'Systolic blood pressure (mmHg)',
            BPdiastol ~ 'Diastolic blood pressure (mmHg)',
            CurrentSmoker ~ 'Current Smoker',
            Alcohol ~ "Drinking Alcohol",
            Race ~ 'Race and Ethnicity',
            
            SagittalAbdominalDiameterCm ~ 'Sagittal abdominal diameter (cm)',
            NeckCircumferenceIn ~ 'Neck circumference (in)',
            WaistGirthIn ~ 'Waist circumference (in)',
            HipGirthIn ~ 'Hip circumference (in)',
            ThighGirthIn ~ 'Thigh circumference (in)',
            
            receivesBPMeds ~ 'Treated for high blood pressure',
            HDL ~ 'Plasma HDL Cholesterol (mg/dl)',
            CHOL ~ 'Serum cholesterol (mg/dl)',
            GLUC ~ 'Fasting plasma glucose (mg/dl)',
            TRIG ~ 'Plasma triglycerides (mg/dl)',
            
            Diabetes ~ 'Diabetes',
            MetabolicSyndrome ~ 'Metabolic syndrome',
            Menopause ~ "Menopause",
            
            FHS_Risk_Score_10yr_CVD ~ 'Framingham Risk Score',
            Physical_Activity_Index ~ 'Physical Activity Index',
            
            HandGripStrength1Right ~ 'Right Hand Grip Strength (nearest kg)', 
            HandGripStrength1Left ~ 'Left Hand Grip Strength (nearest kg)',
            QuickWalkS ~ 'Quick Walk Time (s)', 
            ChairStands ~ 'Time For 10 Chair Stands (s)',
            
            
            IDTYPE ~ "FHS Cohort",
            CT_number ~ 'FHS CT Timepoint',
            
            slice_thickness_mm ~ "Slice Thickness (mm)",
            
            #CV
            Myocardial_Infarction_Hx ~ 'Myocardial Infarction',
            Angina_Pectoris_Hx ~ 'Angina Pectoris',
            Coronary_Insufficiency ~ 'Coronary Insufficiency',
            STROKE_TIA_Hx ~ 'Stroke or TIA',
            IC_Intermittent_Claudication_Hx ~ 'Intermittent Claudication',
            HxDVT ~ 'Deep Vein Thrombosis',
            CHF_Congestive_Heart_Failure_Hx ~ 'Congestive Heart Failure',
            HeartValveSurgery ~ 'Heart Valve Surgery',
            ThoracicAortaSurgery ~ 'Surgery of the Thoracic Aorta',
            AbdominalAortaSurgery ~ 'Surgery of the Abdominal Aorta',
            HxOtherCVProcedure ~ 'Other Cardiovascular Procedure',
            HxOfHospitalizationForHeartFailure ~ 'Hospitalization for Heart Failure',
            HistoryOfPE ~ 'Pulmonary Embolism',
            AtrialFibrillationHx ~ 'Atrial fibrillation',
            SyncopeHx ~ 'Syncope',
            
            #Metabolic and Endocrine
            ThyroidDisease ~ 'Thyroid Disease',
            GallbladderDisease ~ 'Gallbladder Disease',
            HxGallbladderSurgery ~ 'Gallbladder Surgery',
            
            #Other procedures
            FemoralLowerExtremitySurgery ~ 'Lower Extremity Surgery',
            
            #Neuro and Psych
            Dementia ~ 'Dementia',
            Parkinson ~ 'Parkinson',
            SeizureHx ~ 'Seizure',
            HxDepression ~ 'Depression',
            HxPsychosis ~ 'Psychosis',
            HxAnxiety ~ 'Anxiety',
            HxSpinalStenosis ~ 'Spinal stenosis',
            
            #Pulmo
            LungFibrosis ~ 'Lung Fibrosis',
            Asthma ~ 'Asthma',
            COPD ~ 'COPD',
            Emphysema ~ 'Emphysema',
            HxSleepApnea ~ 'Sleep Apnea',
            
            #Reproductive
            HxHysterectomy ~ 'Hysterectomy',
            HxOvarectomy ~ 'Ovarectomy',
            HxProstateDisease ~ 'Prostate Disease'
            
          ),
          
          type = list(
            c(Race,
              IDTYPE, slice_thickness_mm, CT_number) ~ "categorical",
            c(CurrentSmoker, receivesBPMeds, Diabetes, MetabolicSyndrome, Myocardial_Infarction_Hx,
              Angina_Pectoris_Hx,
              Coronary_Insufficiency,
              STROKE_TIA_Hx,
              IC_Intermittent_Claudication_Hx,
              HxDVT,
              CHF_Congestive_Heart_Failure_Hx,
              HeartValveSurgery,
              ThoracicAortaSurgery,
              AbdominalAortaSurgery,
              
              #hasCoronaryStent,
              HxOtherCVProcedure,
              HxOfHospitalizationForHeartFailure,
              HistoryOfPE,
              AtrialFibrillationHx,
              SyncopeHx,
              #Metabolic and Endocrine
              ThyroidDisease,
              GallbladderDisease,
              HxGallbladderSurgery,
              
              #Other procedures
              FemoralLowerExtremitySurgery,
              
              #Neuro and Psych
              Dementia,
              Parkinson,
              SeizureHx,
              HxDepression,
              HxPsychosis,
              HxAnxiety,
              HxSpinalStenosis,
              
              #Pulmo
              LungFibrosis,
              Asthma,
              COPD,
              Emphysema,
              HxSleepApnea,
              
              #Reproductive
              HxHysterectomy,
              HxOvarectomy,
              HxProstateDisease,
              Alcohol,
              Menopause) ~ "dichotomous",
            c(
              age,
              BPsystol,
              BMI,
              Height_m,
              Weight_kg,
              BSA,
              BPdiastol,
              SagittalAbdominalDiameterCm,
              NeckCircumferenceIn,
              WaistGirthIn,
              HipGirthIn,
              ThighGirthIn,
              HDL,
              CHOL,
              GLUC,
              TRIG,
              FHS_Risk_Score_10yr_CVD,
              Physical_Activity_Index
            ) ~ "continuous"
          ),
          
          digits = list(contains("age") ~ 1),
          
          statistic = all_continuous() ~ c("{mean} ({sd})"),
          #, "{median} ({IQR})"),
          
          
          sort = all_categorical() ~ 'frequency',
          missing_text = "(Missing)"
          
        ) %>%
        
        modify_header(all_stat_cols() ~ "**{level}**, N={n}") %>%
        #modify_footnote(
        #  all_stat_cols() ~ "Mean (Standard Deviation) for continuous and N (%) for categorical variables"
        #) %>%
       # modify_caption("**Table 1. Cohort Characteristics**") %>%
        
        add_stat_label() %>%
        bold_labels() %>%
        italicize_levels() ,
      .header =  "**{strata}**, N = {c(length(unique((df[df$Sex == as.character(strata[1]), ])$patient_id)), length(unique((df[df$Sex == as.character(strata[2]), ])$patient_id))) }"    ) %>%
    remove_stat_labels_from_rows()

  
  
  
  
  
  ########
  #Saving the tables
  #########
  if(name != ''){
    name <- paste('_', name, sep='')
  }
  
  path <- paste(root_path, 'tbl1_sex_compact', name, '.html', sep='')
  gtsave(as_gt(table_1_sex), path)
  
  path <- paste(root_path, 'tbl1_everything', name, '.html', sep='')
  gtsave(as_gt(table_1_detailed), path)
  
}



