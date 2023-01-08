import pyperclip

#A simple script to create R Markdown code that will show tables 
#for all models and their corresponding centile and worm plots, as well as Q stats

metrics = ('CSMA', 'SMI', 'SMRA', 'SMG')

sexes = ('Female', 'Male')
vertebral_levels = ('T5', 'T8', 'T10', 'L3')

n_gaic_penalties = 10

RMD_code = '\n' \
           '# Models {.tabset}\n' \
           '\n' \
           '\n'

for metric in metrics:
    RMD_code += f'\n' \
              f'## {metric} ' + '{.tabset}\n' \
                                '\n'

    for lvl in vertebral_levels:
        RMD_code += f'\n' \
              f'### {lvl} ' + '{.tabset}\n' \
                                '\n'

        for sex in sexes:
            RMD_code += f'\n' \
                        f'#### {sex} ' + '{.tabset}\n' \
                                        '\n'

            #Print comparison dataframe:
            RMD_code += f'\n' \
                        f'##### Comparison\n' \
                        f'\n' \
                        f'```{"{r comparison_table_" + metric + "_" + sex + "_" + lvl  + "}"}\n' \
                        f'model_name <- paste("{metric}", "{sex}", "{lvl}", sep="_")\n' \
                        f'model_table <- models[[model_name]]\n' \
                        f'DT::datatable(model_table %>% dplyr::select(-model, -Q_stats, -normality_check_histogram))\n' \
                        f'```\n' \
                        f'\n'

            #And for different centiles the overlap of models
            RMD_code += '###### Centile curves: {.tabset} \n\n'

            for centile in (3, 15, 50, 85, 97):
                centile = str(centile)
                RMD_code += f'####### Centile {centile} \n' \
                            f'\n' \
                            f'```{"{r comparison_centiles_" + centile + "_" + metric + "_" + sex + "_" + lvl  + "}"}\n' \
                            f'compare_centiles(model_table$model, model_table$gaic_penalty, "{metric}", c({centile})) \n' \
                            f'```\n' \
                            f'\n'

            #Now create figures for each model
            for i in range(n_gaic_penalties):
                i+=1
                RMD_code+= f'\n' \
                           f'##### Model {i} {"{.tabset}"}\n' \
                           f'\n' \
                           f'```{"{r comparison_table_" + metric + "_" + sex + "_" + lvl  + "_Model" + str(i) +  "}"}\n' \
                           f'DT::datatable(model_table[{i}, ] %>% select(-model, -Q_stats, -normality_check_histogram))\n' \
                           f'model <- model_table$model[{i}][[1]]\n' \
                           f'```\n' \
                           f'\n' \
                           f'###### Centiles\n' \
                           f'\n' \
                           f'```{"{r centiles_" + metric + "_" + sex + "_" + lvl  + "_Model" + str(i) +  "}"}\n' \
                           f'plot_centiles(model, "{metric}", "{sex}", "{lvl}")\n' \
                           f'```\n' \
                           f'\n' \
                           f'###### WormPlot:\n' \
                           f'\n' \
                           f'```{"{r wormplot_" + metric + "_" + sex + "_" + lvl  + "_Model" + str(i) +  "}"}\n' \
                           f'wormplot_info(model)\n' \
                           f'```\n' \
                           f'\n' \
                           f'###### Q-Stats:\n' \
                           f'\n' \
                           f'```{"{r Q_stats_" + metric + "_" + sex + "_" + lvl  + "_Model" + str(i) +  "}"}\n' \
                           f'Q.stats(model, xvar=model$xvar)\n' \
                           f'```\n' \
                           f'\n' \
                           f'\n' \
                           f'###### Residuals Histogram:\n' \
                           f'\n' \
                           f'Can check for normality here: \n' \
                           f'```{"{r z_scores_hist_" + metric + "_" + sex + "_" + lvl  + "_Model" + str(i) +  "}"}\n' \
                           f'model_table$normality_check_histogram[{i}]\n' \
                           f'```\n' \
                           f'\n'

print(RMD_code)

#Copying to clipboard:
pyperclip.copy(RMD_code)