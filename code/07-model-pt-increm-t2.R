# ===============================================================================
# DYNAMIC MODEL - PRETRAINED & INCREMENTAL LEARNING - TUNNEL 2
# ===============================================================================

set.seed(123)

# initialize results df
df_result <- NULL
df_error <- NULL

# loop for dynamic prediction
for (i in  1:(nrow(df_merge2)-1)) {
  
  # Check if there are soil labels
  if (!any(is.na(df_merge2[i, c('DC', 'DS', 'DG')]))) {
    
    # train df
    df_train_i <- rbind(df_merge1, df_merge2[1:i,]) %>% drop_na()
    
    # model training
    rf_model_DC <- ranger(DC ~ ., data = df_train_i %>%
                            select(-Chainage, -DS, -DG),
                          num.trees = 200,
                          # mtry = p,
                          min.node.size = 1,
                          splitrule = 'extratrees') 
    rf_model_DS <- ranger(DS ~ ., data = df_train_i %>% 
                            select(-Chainage, -DC, -DG),
                          num.trees = 200,
                          # mtry = p,
                          min.node.size = 1,
                          splitrule = 'extratrees') 
    rf_model_DG <- ranger(DG ~ ., data = df_train_i %>% 
                            select(-Chainage, -DS, -DC),
                          num.trees = 200,
                          # mtry = p,
                          min.node.size = 1,
                          splitrule = 'extratrees')
  }
  
  # get next ring for to be predicted
  df_pred_i <- df_merge2[i+1,]
  
  # compute predictions
  rf_pred_DC <- predict(rf_model_DC, data = df_pred_i %>% 
                          select(-Chainage, -DS, -DG))$predictions %>% round(3)
  rf_pred_DS <- predict(rf_model_DS, data = df_pred_i %>% 
                          select(-Chainage, -DC, -DG))$predictions %>% round(3)
  rf_pred_DG <- predict(rf_model_DG, data = df_pred_i %>% 
                          select(-Chainage, -DC, -DS))$predictions %>% round(3)
  
  # linearize results to be 100%
  df_result_i <- data.frame(Chainage = df_pred_i$Chainage,
                            
                            DC = 100 * rf_pred_DC/sum(rf_pred_DC, rf_pred_DS, rf_pred_DG),
                            DS = 100 * rf_pred_DS/sum(rf_pred_DC, rf_pred_DS, rf_pred_DG),
                            DG = 100 * rf_pred_DG/sum(rf_pred_DC, rf_pred_DS, rf_pred_DG)) 
  # save in df
  df_result <- rbind(df_result, df_result_i)
  
  # compute and save errors in df
  error_i <- data.frame(Chainage = df_pred_i$Chainage,
                        DC = abs(df_result_i$DC - df_pred_i$DC),
                        DS = abs(df_result_i$DS - df_pred_i$DS),
                        DG = abs(df_result_i$DG - df_pred_i$DG)) %>%
    mutate(MAE = sum(DC, DS, DG)/3) 
  
  df_error <- rbind(df_error, error_i)
  
  # computation control
  print(i)
}

# compute mean absolute error along chainages
for(i in 1:nrow(df_error)) {
  df_error[i, 'LongMAE'] <- df_error[1:i, ] %>% drop_na() %>% select('MAE') %>% colMeans()
}

write_csv(df_result, "../output/df_result_tun2_ptinc.csv")
write_csv(df_error, "../output/df_error_tun2_ptinc.csv")

# ===============================================================================
# PLOT
# ===============================================================================

for (i in 1:nrow(df_result)) {
  
  fig_geo <- ggplot() +
    geom_col(data = df_result[1:i,] %>% gather(variable, value, DC:DG), 
             aes(Chainage, value, fill = variable),
             position = 'stack', width = 2) +
    geom_col(data = label_orig %>% gather(variable, value, DC:DG),
             aes(Chainage, value, fill = variable),
             position = 'stack', width = 10, color = 'black') +
    scale_fill_manual(values = c("steelblue", "tomato", "yellow")) +
    xlim(0, 2050) +
    labs(x = NULL, y = "Soil Type (%)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  fig_data <- ggplot() +
    geom_line(data = df_merge2[1:i,] %>% select(-DC, -DG, -DS) %>% 
                mutate(across(-c(Chainage), scale, center = TRUE, scale = TRUE)) %>%
                melt(id = 'Chainage'),
              aes(x = Chainage, y = value, color = variable),
              alpha = 0.5) +
    geom_point(data = df_merge2[1:i,] %>% select(-DC, -DG, -DS) %>% 
                 mutate(across(-c(Chainage), scale, center = TRUE, scale = TRUE)) %>%
                 tail(1) %>%
                 melt(id = 'Chainage'),
               aes(x = Chainage, y = value, color = variable),
               size = 1, alpha = 0.5) +
    xlim(0, 2050) +
    ylim(-5, 5) +
    labs(x = NULL, y = "Scaled TBM Data") +
    theme_bw() +
    theme(legend.position = 'none',
          axis.text.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  fig_error <- ggplot(df_error[1:i,], aes(x = Chainage, y = MAE)) +
    geom_segment(aes(xend = Chainage, yend = 0), color = "grey") +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = df_error[i,'LongMAE'], 
               linetype = "dashed", linewidth = 1, color = "red") +
    labs(title = paste("Mean Absolute Error Soil Types per Chainage")) +
    xlim(0, 2050) +
    ylim(0, 20) +
    labs(x = NULL, y = "MAE (%)") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  fig_mae <- ggplot(df_error[1:i,], aes(x = Chainage, y = LongMAE)) +
    geom_point(color = 'red', alpha = 0.5) +
    xlim(0, 2050) +
    ylim(0, 20) +
    labs(title = paste("Mean Absolute Error along Chainages")) +
    labs(x = "Chainage (m)", y = "MAE (%)") +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank())
  
  # Combine the ggplots using ggarrange
  combined_plot <- ggarrange(fig_geo, fig_data, fig_error, fig_mae, ncol = 1)
  
  # Save the combined plot as a PNG file
  ggsave(paste0("../output/fig_tun2_ptinc/fig-", i, ".png"), plot = combined_plot, 
         width = 9, height = 9, dpi = 300)
}





