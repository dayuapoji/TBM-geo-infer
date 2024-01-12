set.seed(1)

# ------------------------------------------------------------------------------
# TUNNEL #1
# ------------------------------------------------------------------------------

# initialize
results <- NULL

# compute LOF
for(i in 125:nrow(df_merge1)) {
  
  # PCA 
  pca_out <- prcomp(df_merge1[1:i, ] %>%
                      # remove non excavation related features
                      select(!matches("Vol|Dev|Pitch|Roll|Yaw|Grout|Chamber|Screw|Gate")) %>%
                      # remove non-predictor columns
                      select(-c(Chainage, DC, DS, DG)), #%>%
                      # remove constant columns
                      # select(-where(~all(length(unique(.)) == 1))),
                    
                    # standardized units
                    scale. = TRUE, center = TRUE)
  
  # PC scores
  pc <- pca_out$x %>% as.data.frame() %>%
    mutate(., Chainage = df_merge1$Chainage[1:i]) 
  
  # LOF
  lof <- lof(pc[1:i, 1:2], minPts = 10)
  
  # LOF at current chainage
  lof_i <- data.frame(Chainage = tail(pc$Chainage, 1),
                      LOF = tail(lof, 1))
  
  # bind results
  results <- rbind(results, lof_i)
  print(paste(i, 'done'))
}

results1 <- results

# ------------------------------------------------------------------------------
# TUNNEL #2
# ------------------------------------------------------------------------------

# initialize
results <- NULL

# compute LOF
for(i in 100:nrow(df_merge2)) {

  # PCA 
  pca_out <- prcomp(df_merge2[1:i, ] %>%
                      # remove non excavation related features
                      select(!matches("Vol|Dev|Pitch|Roll|Yaw|Grout|Chamber|Screw|Gate")) %>%
                      # remove non-predictor columns
                      select(-c(Chainage, DC, DS, DG)), #%>%
                      # remove constant columns
                      # select(-where(~all(length(unique(.)) == 1))),
                    
                    # standardized units
                    scale. = TRUE, center = TRUE)
  
  # PC scores
  pc <- pca_out$x %>% as.data.frame() %>%
    mutate(., Chainage = df_merge2$Chainage[1:i]) 
  
  # LOF
  lof <- lof(pc[1:i, 1:2], minPts = 10)
  
  # LOF at current chainage
  lof_i <- data.frame(Chainage = tail(pc$Chainage, 1),
                      LOF = tail(lof, 1))
  
  # bind results
  results <- rbind(results, lof_i)
  print(paste(i, 'done'))
}

results2 <- results


