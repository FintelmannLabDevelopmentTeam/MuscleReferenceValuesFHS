# Framingham Heart Study: Reference values for skeletal muscle.

These scripts will read in curated measurements for skeletal muscle, based on a quality assurance after automated segmentation. They will merge them with clinical datasheets provided by the Framingham Heart Study, generating a final tidied and clean datasheet.

The purpose of these scripts is described in the associated publication: "Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment: The Framingham Heart Study"

## Setup
To use these files, data sheets need to be linked. Edit the corresponding fields in `read_data/rename_sheets_basecamb/paths.csv` to match the paths of corresponding data files.

## Table of Contents

-   `FHS_Final_Analysis.R`
    -   The starting point of data loading, merging, and tidying.
    -   Initializes the generation of all tables and figures, for main and subgroup analyses.
-   `inter_intrarater_assessment.R`
    -   Holds the code responsible for calculating the Intraclass correlation coefficients and Cohen's kappa for inter- and intrareader agreement
-   `centile_curves/Model_Centiles.Rmd`
    -   The code used to model and evaluate the different LMSP models used for centile curve and z score generation.
-   `Shiny/`
    -   The code for the Shiny app.

All other files hold various utility codes and will be called from the three files above, which serve as starting points for the different analyses.

`read_data/FHS Data Merging.png` provides a graphical overview of how different data sources are connected to the final datasheet. This has to be used with caution, however, since the code has been updated after the creation of this overview.

## Adding other data columns from the exam sheets
Code is in place to easily select additional data of interest from the closest exam sheet. It is a good idea to visit `add_binary_exam_data()` and `add_continuous_exam_data()` in `Read_FHS_exams.R` for more information.

## Contact
These scripts were generated by P. Erik Tonnesen ([ptonnesen\@mgh.harvard.edu](mailto:ptonnesen@mgh.harvard.edu)). Please refer to them or the corresponding author of the associated publication.

# Associated publication:
"Muscle Reference Values from Thoracic and Abdominal CT for Sarcopenia Assessment: The Framingham Heart Study"
