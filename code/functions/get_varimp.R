get_df_varimp <- function(df_varimp, SoilType) {
  
  # modeling & varimp
  model_varimp <- train(as.formula(paste(SoilType, "~ .")), 
                        data = df_varimp,
                        method = 'ranger',
                        num.trees = 200,
                        importance = 'impurity',
                        verbose = FALSE)
  
  # prep df
  df_varimp <- varImp(model_varimp)$importance %>%
    as.data.frame() %>%
    rename(Varimp = 'Overall') %>%
    rownames_to_column(var = "Features")
  
  # grouping
  df_varimp <- df_varimp %>%
    mutate(
      Groups = case_when(
        grepl("Cutter", Features, ignore.case = TRUE) ~ "Advancing",
        grepl("Thrust|Advance", Features, ignore.case = TRUE) ~ "Advancing",
        grepl("EP|Density|Chamber", Features, ignore.case = TRUE) ~ "ChamberPressure",
        grepl("Screw|Gate", Features, ignore.case = TRUE) ~ "ChamberPressure",
        grepl("Foam|Polymer|Air|Additive|Slurry", Features, ignore.case = TRUE) ~ "GroundConditioning",
        grepl("Belt", Features, ignore.case = TRUE) ~ "MuckDischarge",
        grepl("Dev|Pitch|Roll|Yaw", Features, ignore.case = TRUE) ~ "Steering",
        grepl("Grout", Features, ignore.case = TRUE) ~ "TailGrout",
        TRUE ~ "Other"
      )
    )
  return(df_varimp)
}
