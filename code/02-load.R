# ===============================================================================
# LOAD DATA
# ===============================================================================

# load tunnel 1
data_orig1 <- read_csv('../data/tbm1_outward.csv')

# load tunnel 2
data_orig2 <- read_csv('../data/tbm2_homeward.csv')

# load BH labels
label_orig <- read_csv('../data/geo_osaka.csv') %>% 
  select(!contains('BH')) # remove BH id

colnames <- data_orig2[1,]





