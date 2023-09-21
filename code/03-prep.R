# ===============================================================================
# DATA PREPARATION - TUNNEL 1
# ===============================================================================

data_orig <- data_orig1
colnames(data_orig) <- gsub("\\.", " ", colnames(data_orig))

# list of functions
select <- list.files(path = 'select/', pattern = "[.]R$", 
                     full.names=TRUE, recursive = TRUE)

# load
for (i in (1:length(select))) {
  print(select[i])
  source(select[i])
}


# at the moment, just make it simple by rounding the chainage
chainage_head <- chainage[, 'ChainageHead'] %>%
  set_colnames('Chainage') %>%
  round()

df <- cbind(chainage_head, group_excav, group_advance, group_steer,
            group_gcs, group_epb, group_muck, group_grout) %>% # 
  filter(Chainage > 0) %>% 
  filter(CutterTorque > 0) %>% 
  distinct(., Chainage, .keep_all = TRUE) %>% 
  drop_na()
# unique(chainage_head$Chainage[duplicated(chainage_head$Chainage)])
# df <- df %>% filter(Chainage != c(0, 524, 2054)) %>% filter(Chainage > 0) %>% drop_na()


# labelling
num_segment <- 41 # num ring segment represented by a BH
label <- NULL
for (i in 1:nrow(label_orig)) {
  label_row <- label_orig[i, ]
  label_segment <- NULL
  for (j in 1:num_segment) {
    label_edit <- data.frame(Chainage = label_row$Chainage - ceiling(num_segment/2) + j,
                             DC = label_row$DC,
                             DS = label_row$DS,
                             DG = label_row$DG)
    label_segment <-rbind(label_segment, label_edit)
  }
  label <- rbind(label, label_segment)
}

df_merge1 <- left_join(df, label, by = 'Chainage')
# Remove rows with duplicated chainage
df_merge1 <- df_merge1 %>% .[!duplicated(.$Chainage), ]


# ===============================================================================
# DATA PREPARATION - TUNNEL 2
# ===============================================================================

data_orig <- data_orig2
colnames(data_orig) <- gsub("\\.", " ", colnames(data_orig))


for (i in (1:length(select))) {
  print(select[i])
  source(select[i])
}


# at the moment, just make it simple by rounding the chainage
chainage_head <- chainage[, 'ChainageHead'] %>%
  set_colnames('Chainage') %>%
  round()

df <- cbind(chainage_head, group_excav, group_advance, group_steer,
            group_gcs, group_epb, group_muck, group_grout) %>% # 
  mutate(Chainage = 2020 - Chainage) %>%
  filter(Chainage > 0) %>% 
  filter(CutterTorque > 0) %>% 
  distinct(., Chainage, .keep_all = TRUE) %>% 
  drop_na()


# labelling
num_segment <- 41 # num ring segment represented by a BH
label <- NULL
for (i in 1:nrow(label_orig)) {
  label_row <- label_orig[i, ]
  label_segment <- NULL
  for (j in 1:num_segment) {
    label_edit <- data.frame(Chainage = label_row$Chainage - ceiling(num_segment/2) + j,
                             DC = label_row$DC,
                             DS = label_row$DS,
                             DG = label_row$DG)
    label_segment <-rbind(label_segment, label_edit)
  }
  label <- rbind(label, label_segment)
}

df_merge2 <- left_join(df, label, by = 'Chainage')
# Remove rows with duplicated chainage
df_merge2 <- df_merge2 %>% .[!duplicated(.$Chainage), ]

