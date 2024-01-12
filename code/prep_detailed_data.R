
load_data <- function(files) {
  df <- NULL
  for (i in (1:length(files))) {
    
    data_orig <- read_csv(files[i])
    
    # translate colnames and units
    colnames(data_orig) <- colnames(colnames)
    data_orig <- rbind(colnames[1, ], data_orig[2:nrow(data_orig), ])
    
    # prep df
    data <- data_orig %>% prepmerge_df #%>% drop_na(.)
    df <- rbind(df, data) 
    
    # show progress
    print(paste('Load', i, 'Done'))
  }
  return(df)
}



# load & select
files <- list.files(path = '../data/tbm2_homeward/',
                    pattern = "[.]Csv$",
                    full.names = TRUE, recursive = TRUE)

data_det <- load_data(files)

# save as csv
write_csv(data_det,'../data/tbm2_homeward_det.csv')


