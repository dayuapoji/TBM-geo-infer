# ------------------------------------------------------------------------------
# TUNNEL #1
# ------------------------------------------------------------------------------

df_varimp_DC1 <- get_df_varimp(df_merge1 %>% drop_na() %>%
                                select(-Chainage) %>% 
                                select(!contains('air')) %>%
                                select(-c(DG, DS)),
                              'DC')

df_varimp_DS1 <- get_df_varimp(df_merge1 %>% drop_na() %>%
                                select(-Chainage) %>% 
                                select(!contains('air')) %>%
                                select(-c(DG, DC)),
                              'DS')

df_varimp_DG1 <- get_df_varimp(df_merge1 %>% drop_na() %>%
                                select(-Chainage) %>% 
                                select(!contains('air')) %>%
                                select(-c(DC, DS)),
                              'DG')

# ------------------------------------------------------------------------------
# TUNNEL #2
# ------------------------------------------------------------------------------

df_varimp_DC2 <- get_df_varimp(df_merge2 %>% drop_na() %>%
                                 select(-Chainage) %>% 
                                 select(!contains('air')) %>%
                                 select(-c(DG, DS)),
                               'DC')

df_varimp_DS2 <- get_df_varimp(df_merge2 %>% drop_na() %>%
                                 select(-Chainage) %>% 
                                 select(!contains('air')) %>%
                                 select(-c(DG, DC)),
                               'DS')

df_varimp_DG2 <- get_df_varimp(df_merge2 %>% drop_na() %>%
                                 select(-Chainage) %>% 
                                 select(!contains('air')) %>%
                                 select(-c(DC, DS)),
                               'DG')