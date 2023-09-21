# ===============================================================================
# STATIC MODEL - TUNNEL 1
# ===============================================================================

set.seed(123)

# Data splitting
df_train <- df_merge1 %>% drop_na() # drop rows without labels
# df_tunnel1_labeled <- df_train
df_test <- df_merge1[!(df_merge1$Chainage %in% df_train$Chainage), ]

# model training
rf_model_DC <- ranger(DC ~ ., data = df_train %>% 
                        select(-Chainage, -DS, -DG)) 
rf_model_DS <- ranger(DS ~ ., data = df_train %>% 
                        select(-Chainage, -DC, -DG)) 
rf_model_DG <- ranger(DG ~ ., data = df_train %>% 
                        select(-Chainage, -DS, -DC)) 

# prediction
rf_pred_DC <- predict(rf_model_DC, data = df_test %>% 
                        select(-Chainage, -DS, -DG))$predictions
rf_pred_DS <- predict(rf_model_DS, data = df_test %>% 
                        select(-Chainage, -DC, -DG))$predictions
rf_pred_DG <- predict(rf_model_DG, data = df_test %>% 
                        select(-Chainage, -DS, -DC))$predictions

results <- data.frame(#Chainage = df_test$Chainage,
  DC = rf_pred_DC,
  DS = rf_pred_DS,
  DG = rf_pred_DG) 

results$Sum <- rowSums(results)

df_results <- data.frame(Chainage = df_test$Chainage,
                         DC = 100*rf_pred_DC/results$Sum,
                         DS = 100*rf_pred_DS/results$Sum,
                         DG = 100*rf_pred_DG/results$Sum) 


# ===============================================================================
# PLOT
# ===============================================================================

ggplot() +
  geom_col(data = df_results %>% gather(variable, value, DC:DG), 
           aes(Chainage, value, fill = variable),
           position = 'stack', width = 2) +
  geom_col(data = label_orig %>% gather(variable, value, DC:DG),
           aes(Chainage, value, fill = variable),
           position = 'stack', width = 20, color = 'black') +
  scale_fill_manual(values = c("steelblue", "tomato", "yellow")) +
  theme_bw()
