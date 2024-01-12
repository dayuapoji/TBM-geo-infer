colors <- c('DC' = "steelblue", 
            'DS' = "yellow",
            'DG'= "tomato")  

tbm_data <- df_merge1

# dominant soil unit
for (i in 1:nrow(tbm_data)) {
  if(is.na(tbm_data[i, c('DC', 'DS', 'DG')])) { 
    tbm_data$SoilUnit[i] = 'Unknown'
  } else {
    tbm_data$SoilUnit[i] = colnames(tbm_data[, c('DC', 'DS', 'DG')])[which.max(tbm_data[i, c('DC', 'DS', 'DG')])]
  }
}

# percentage of dominant soil unit
for (i in 1:nrow(tbm_data)) {
  if(is.na(tbm_data[i, c('DC', 'DS', 'DG')])) { 
    tbm_data$SoilUnitPercent[i] = 0
  } else {
    tbm_data$SoilUnitPercent[i] = tbm_data[i, c(c('DC', 'DS', 'DG'))][which.max(tbm_data[i, c(c('DC', 'DS', 'DG'))])][[1]]
  }
}

# PCA 
pca_out <- prcomp(tbm_data %>% 
                    
                    select(!matches("Vol|Dev|Pitch|Roll|Yaw|Grout|Chamber|Screw|Gate")) %>%
                    # remove non-predictor columns
                    select(!contains('chainage')) %>%
                    select(!contains('dc')) %>%
                    select(!contains('ds')) %>%
                    select(!contains('dg')) %>%
                    select(!contains('soilunit')) %>%
                    select(!contains('soilunitpercent')),
                  
                  # standardized units
                  scale. = T, center = T)

# PC scores
pc <- pca_out$x %>% as.data.frame() %>%
  mutate(., Chainage = tbm_data$Chainage) %>%
  mutate(., SoilUnit = tbm_data$SoilUnit) %>%
  mutate(., SoilUnitPercent = tbm_data$SoilUnitPercent) %>%
  mutate(., DG = tbm_data$DG)

# plot according soil unit
ggsave('../output/pca_tunnel1.pdf',
       
       ggarrange(
         ggplot() +
           geom_point(data = pc,
                      aes(x = PC1, y = PC2, color = SoilUnit, size = SoilUnitPercent), 
                      alpha = 0.5) +
           
           scale_size_continuous(range = c(1, 2.5)) +
           scale_color_manual(values = colors) +
           ggtitle("Tunnel #1") +
           xlim(-5, 5) +
           ylim(-5, 5) +
           theme_bw() +
           theme(panel.grid = element_blank(),
                 legend.position = 'none'),
         
         ggplot() +
           geom_point(data = pc,
                      aes(x = PC1, y = PC2, 
                          color = SoilUnit, 
                          size = SoilUnitPercent), 
                      alpha = 0.5) +
           geom_point(data = pc %>% drop_na(),
                      aes(x = PC1, y = PC2, size = DG), 
                      color = 'tomato', alpha = 0.5) +
           scale_size_continuous(range = c(1, 2.5)) +
           scale_color_manual(values = colors) +
           ggtitle("Plot DG") +
           xlim(-5, 5) +
           ylim(-5, 5) +
           theme_bw() +
           theme(panel.grid = element_blank(),
                 legend.position = 'none'),
         
         nrow = 1),
       width = 7.5,     # Width of the PDF
       height = 3.5,      # Height of the PDF
       units = "in") 

# Plot PCA Variance ------------------------------------------------------------

variance = pca_out$sdev^2 / sum(pca_out$sdev^2)

fig_scree <- ggplot()+
  
  geom_vline(xintercept = 2, linetype = 'dashed') +
  geom_vline(xintercept = 3, linetype = 'dashed') +
  geom_vline(xintercept = 8, linetype = 'dashed') +
  
  geom_segment(aes(x = 2, y = 0.22,
                   xend = 1.5, yend = 0.22),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_segment(aes(x = 3, y = 0.18,
                   xend = 2.5, yend = 0.18),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_segment(aes(x = 8, y = 0.1,
                   xend = 7.5, yend = 0.1),
               arrow = arrow(length = unit(0.02, "npc"))) +
  
  geom_col(mapping = aes(x = 1:ncol(pca_out$x), y = variance)) +
  scale_x_continuous(labels = as.character(1:ncol(pca_out$x)), 
                     breaks = c(1:ncol(pca_out$x))) +
  
  geom_label_repel(aes(x = 2, y = 0.22), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:2]), 2))) +
  geom_label_repel(aes(x = 3, y = 0.18), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:3]), 2))) +
  geom_label_repel(aes(x = 8, y = 0.1), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:8]), 2))) +
  ggtitle("Tunnel #1") +
  xlab('Principal Components') +
  ylab('Variance') +
  # scale_x_discrete(nbreaks = 15) +
  theme_bw() +
  theme(panel.grid = element_blank())


# Plot PCA Contribution --------------------------------------------------------

# biplot
fig_biplot <- fviz_pca_var(pca_out,
                           col.var = "contrib", # Color by contributions to the PC
                           # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = T) +
  scale_color_gradientn(name = 'Contributions', 
                        colours = c('blue', 'orange', 'red')) +
  xlab('PC1') +
  ylab('PC2') +
  guides(fill=guide_legend(title="Contributions")) +
  ggtitle('')

ggsave('../output/pca_interp_tunnel1.pdf',
       ggarrange(fig_scree, fig_biplot, nrow = 1),
       width = 12,
       height = 6)
