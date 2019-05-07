cluster_data_nb <- cluster_data %>%
  select(one_of("country", cluster_vars))
cluster_data_nb <- as.data.frame(cluster_data_nb)
rownames(cluster_data_nb) <- cluster_data_nb$country
cluster_data_nb["country"] <- NULL

recommended_nb <- NbClust::NbClust(cluster_data_nb, distance = "euclidean", 
                                   method = "ward.D", 
                                   index = c("kl", "ch", "cindex", "db", 
                                             "silhouette", "duda", "pseudot2", 
                                             "beale", "ratkowsky", "mcclain",
                                             "ptbiserial", "gap",  "gamma", 
                                             "gplus", "tau", "sdindex", "sdbw"
                                             ))
recommended_nb_hist_data <- data.frame(
  method=names(recommended_nb$Best.nc[1,]),
  clusters=recommended_nb$Best.nc[1,]
  )

recommended_nb_hist <- ggplot(recommended_nb_hist_data, 
                              aes(x=clusters
                                  )) + 
  geom_histogram(binwidth = 1, color="#ffffff") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = 0:8) +
  scale_x_continuous(breaks = 1:16) +
  ggtitle("Nb of clusters based on 17 indicators") +
  xlab("Recommended nb of clusters") +
  ylab("Nb of recommendations") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
recommended_nb_hist
ggsave(plot = recommended_nb_hist, 
       filename = "output/nb_clusters.pdf",
       height = 4, width = 6)
