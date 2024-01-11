# setup
set_theme <- theme_bw(base_size = 10) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.y = element_blank(),
        legend.position = "none")

ggsave("../output/varimp_tunnel1.pdf", 
       ggarrange(
         ggplot() +
           geom_bar(data = df_varimp_DC1,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DC") +
           set_theme,
         
         ggplot() +
           geom_bar(data = df_varimp_DS1,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DS") +
           set_theme,
         
         ggplot() +
           geom_bar(data = df_varimp_DG1,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DG") +
           set_theme,
         
         nrow = 1
       ),  # Replace ... with your ggarrange code
       width = 7.5,     # Width of the PDF
       height = 5,      # Height of the PDF
       units = "in")    # Units of the width and height

ggsave("../output/varimp_tunnel2.pdf", 
       ggarrange(
         ggplot() +
           geom_bar(data = df_varimp_DC2,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DC") +
           set_theme,
         
         ggplot() +
           geom_bar(data = df_varimp_DS2,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DS") +
           set_theme,
         
         ggplot() +
           geom_bar(data = df_varimp_DG2,
                    mapping = aes(x = Varimp, 
                                  y = reorder(Features, Varimp),
                                  fill = Groups),
                    color = 'white',
                    stat = 'identity') +
           scale_fill_brewer(palette="Set3") +
           xlab("Relative Importance") +
           ggtitle("DG") +
           set_theme,
         
         nrow = 1
       ),  # Replace ... with your ggarrange code
       width = 7.5,     # Width of the PDF
       height = 5,      # Height of the PDF
       units = "in")    # Units of the width and height





