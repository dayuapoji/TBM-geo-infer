ggsave("../output/pca-lof.pdf",
       
       ggarrange(
         
         ggplot() +
           geom_segment(data = results1 %>% filter(LOF <= 5),
                        mapping =  aes(x = Chainage, xend = Chainage,
                                       y = 0, yend = LOF,
                                       color = LOF)) +
           
           scale_color_gradientn(colors = c('blue', 'orange', 'red', 'red'),
                                 values = c(0, 1, 1.2, 5)/5) +
           scale_x_continuous(limit = c(0, 2100),
                              breaks = seq(0, 2100, 200)) +
           scale_y_continuous(limit = c(0, 5),
                              breaks = c(1, 2, 3, 4, 5)) +
           xlab('Chainage (m)') +
           ylab('LOF') +
           ggtitle("Tunnel #1") +
           theme_bw() +
           theme(panel.grid = element_blank(),
                 legend.title = element_blank()),
         
         ggplot() +
           geom_segment(data = results2 %>% filter(LOF <= 5),
                        mapping =  aes(x = Chainage, xend = Chainage,
                                       y = 0, yend = LOF,
                                       color = LOF)) +
           
           scale_color_gradientn(colors = c('blue', 'orange', 'red', 'red'),
                                 values = c(0, 1, 1.2, 5)/5) +
           scale_x_continuous(limit = c(0, 2100),
                              breaks = seq(0, 2100, 200)) +
           scale_y_continuous(limit = c(0, 5),
                              breaks = c(1, 2, 3, 4, 5)) +
           xlab('Chainage (m)') +
           ylab('LOF') +
           ggtitle("Tunnel #2") +
           theme_bw() +
           theme(panel.grid = element_blank(),
                 legend.title = element_blank()),
         
         ncol = 1
       ),
       width = 7.5,     # Width of the PDF
       height = 5,      # Height of the PDF
       units = "in")    # Units of the width and height
       

