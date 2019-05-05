#' Clustering
#' 
#' Conducts the clustering
#' @param data_file The file for the clustering, must not contain NA
#' @param clustering_vars Should contain all the variables to be used
#'   in the clustering as strings
#' @param nb_groups The number of groups to be highlighted in the plot
#' @param clustering_method The method to be used in the \code{agnes} function:
#'    'ward', 'single', 'complete', 'average' or 'divisive'.
#' @return List with clustering object, the data used, and the plot.
do_clustering <- function(data_file, 
                          clustering_vars, 
                          nb_groups, 
                          clustering_method="ward"){
  cluster_data <- data_file %>%
    dplyr::select(one_of("country", clustering_vars)) %>%
    dplyr::mutate(country=countrycode(country, 
                                      "country.name", 
                                      "country.name.de"))
  
  cluster_data <- as.data.frame(cluster_data)
  rownames(cluster_data) <- cluster_data$country
  
  cluster_data <- select(cluster_data, -country)
  if (clustering_method=="divisive"){
    clustering_object <- cluster_data %>%
      cluster::diana(.)
  } else {
    clustering_object <- cluster_data %>%
      cluster::agnes(method = clustering_method) # Compute hierachical clustering
  }
  
  cluster_plot <- factoextra::fviz_dend(clustering_object, 
                                        k = nb_groups, 
                                        cex = 0.75, # label size
                                        rect = TRUE, # Add rectangle around groups
                                        rect_fill = TRUE,
                                        color_labels_by_k = TRUE, # color labels by groups
                                        k_colors = RColorBrewer::brewer.pal(
                                          n_groups, "Dark2"),
                                        rect_border = RColorBrewer::brewer.pal(
                                          n_groups, "Dark2"),
                                        horiz = TRUE
  ) + theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  return_list <- list(
    "cluster_obj" = clustering_object,
    "cluster_data" = cluster_data,
    "cluster_plot" = cluster_plot
  )
  return(return_list)
}


#' Compare clustering algorithms
#' 
#' Compares three clustering algorithms by computing their scores and by 
#'   producing a table.
#'   
#'   @param raw_dat The data to be used for the clustering
compare_clustering_types <- function(raw_dat, 
                                     clustering_vars) {
  
  hc_agnes_complete_linkage <- # Hierarchical clustering using Complete Linkage
    do_clustering(raw_dat, clustering_vars, 5, "complete")[["cluster_obj"]]
  hc_agnes_average_linkage <- # Hierarchical clustering using Average Linkage
    do_clustering(raw_dat, clustering_vars, 5, "average")[["cluster_obj"]]
  hc_agnes_single_linkage <- # Hierarchical clustering using single Linkage
    do_clustering(raw_dat, clustering_vars, 5, "single")[["cluster_obj"]]
  hc_agnes_ward <- # Hierarchical clustering using Ward's method
    do_clustering(raw_dat, clustering_vars, 5, "ward")[["cluster_obj"]]
  divisive_cluster <-  # divisive hierarchical clustering
    do_clustering(raw_dat, clustering_vars, 5, "divisive")[["cluster_obj"]]
  
  cluster_type <- c("agnes_complete", "agnes_average", "agnes_single", 
                    "agnes_ward", "diana_divisive")
  fit_coefs <- c(hc_agnes_complete_linkage$ac, hc_agnes_average_linkage$ac, 
                 hc_agnes_single_linkage$ac, hc_agnes_ward$ac, 
                 divisive_cluster$dc)
  info_frame <- data.frame(type_clustering = cluster_type, 
                           dif_coef = fit_coefs) %>%
    dplyr::arrange(dplyr::desc(dif_coef)) %>%
    dplyr::rename(Algorithm=type_clustering,
                  `Clust. coef.`=dif_coef)
  return(info_frame)
}
