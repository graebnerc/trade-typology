icae_cols <- c("#696969", "#888888", "#A0A0A0", 
               "#B0B0B0", "#C8C8C8", "#D8D8D8")

#' Clustering
#' 
#' Conducts the clustering
#' 
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
    dplyr::select(one_of("country", clustering_vars)) 
  
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
  
  if (n_groups==6){
    colors_clustering <- icae_cols
  } else{
    colors_clustering <- RColorBrewer::brewer.pal(n_groups, "Dark2")
  }
  
  cluster_plot <- factoextra::fviz_dend(
    clustering_object, 
    k = nb_groups, 
    cex = 0.75, # label size
    rect = TRUE, # Add rectangle around groups
    rect_fill = TRUE,
    color_labels_by_k = FALSE, # color labels by groups
    k_colors = "black",
    rect_border = colors_clustering,
    horiz = TRUE
    ) + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
          )
  return_list <- list(
    "cluster_obj" = clustering_object,
    "cluster_data" = cluster_data,
    "cluster_plot" = cluster_plot
  )
  return(return_list)
}


#' Save the clustering dendogram
#' 
#' Takes a list of variables to be used for the clustering, then implements 
#'  the clustering using \code{do_clustering}. Saves the resulting dendogram.
#'  
#' @param clustering_variables The original names of the variables to be
#'  used for the clustering. Adds 'z' to the variables automatically.
#' @param number_groups The number of clusters to be highlighted.
#' @param vers Optional; adds a version in brackets to the dendogram title
#'  and adjusts the name of the resulting pdf file. FALSE by default.
#' @return The resulting ggplot2 object of the dendogram, which is also 
#'  saved in the output folder.
save_dendogram <- function(clustering_variables, number_groups, vers=FALSE){
  
  clustering_variables_coded <- paste0("z", clustering_variables)
  
  clustering_list <- do_clustering(
    dplyr::mutate(cluster_data, country=ifelse(
      country=="United Kingdom", "UK", country)),
    clustering_variables_coded, 
    n_groups)
  
  clustering_dendogram <-  clustering_list$cluster_plot + 
    xlab("Countries") + ylab("") +
    theme(axis.title = element_blank())
  
  if (vers){
    clustering_dendogram <- clustering_dendogram +
      ggtitle(paste0("Result of the hierarchical clustering (", vers, ")"))
  }
  
  clustering_dendogram
  
  if (vers){
    file_name <- here(paste0("output/fig_2_clustering_", vers, ".pdf"))
  } else {
    file_name <- here("output/fig_2_clustering.pdf")
  }
  
  ggplot2::ggsave(plot = clustering_dendogram,
                  filename = file_name, 
                  width = 7.5, height = 4)
  
  return(clustering_dendogram)
}


#' Compare clustering algorithms
#' 
#' Compares three clustering algorithms by computing their scores and by 
#'   producing a table.
#'   
#'   @param raw_dat The data to be used for the clustering
compare_clustering_types <- function(raw_dat, 
                                     clustering_vars, 
                                     nb_clusters) {
  
  hc_agnes_complete_linkage <- # Hierarchical clustering using Complete Linkage
    do_clustering(raw_dat, 
                  clustering_vars, nb_clusters, "complete")[["cluster_obj"]]
  hc_agnes_average_linkage <- # Hierarchical clustering using Average Linkage
    do_clustering(raw_dat, 
                  clustering_vars, nb_clusters, "average")[["cluster_obj"]]
  hc_agnes_single_linkage <- # Hierarchical clustering using single Linkage
    do_clustering(raw_dat, 
                  clustering_vars, nb_clusters, "single")[["cluster_obj"]]
  hc_agnes_ward <- # Hierarchical clustering using Ward's method
    do_clustering(raw_dat, 
                  clustering_vars, nb_clusters, "ward")[["cluster_obj"]]
  divisive_cluster <-  # divisive hierarchical clustering
    do_clustering(raw_dat, 
                  clustering_vars, nb_clusters, "divisive")[["cluster_obj"]]
  
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


#' Setup data for cluster taxonomy
#' 
#' Takes the taxonomy data and returns a data frame that can be used
#'   to create figures illustrating the differences among clusters.
#'   
#' @param data_used The data used
#' @param cluster considered The name of the cluster (e.g. C1 or C2); must be
#'   a character with a leading C, as in data_used.
#' @param cluster_variables A list with information about the variables to 
#'   be included in the final data. Names of the list should be clusters as 
#'   in \code{cluster}, the items the names of the variables as strings.
#' @return A data table with the data as to be used by ggplot2.
setup_taxonomy_data <- function(data_used,
                                cluster_considered, 
                                cluster_variables){
  if (!cluster_considered %in% data_used[["cluster"]]){
    stop("Cluster considered not present in data set!")
  }
  cluster_data <- data_used %>%
    dplyr::select(one_of("country", "cluster", 
                         cluster_variables[[cluster_considered]])) %>%
    dplyr::mutate(cluster = ifelse(cluster == cluster_considered, 
                                   cluster_considered, "Rest")) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
    dplyr::ungroup()
  # TODO check for NA
  return(cluster_data)
}


#' Make the taxonomy plots
#' 
#' Creates plots to visualize the descriptive statistics of the trade models.
#'  Takes as input the raw taxonomy data, processes it using the function
#'  \code{setup_taxonoy_data} and then creates the plots. Can return both
#'  a complete plot, or a list of individual plots.
#' 
#' @param data_used The data to be used. Must have a column \code{cluster}
#'  in which 'C1', 'C2', etc. identify the cluster.
#' @param cluster considered A string in the form 'C1', 'C2' etc. to
#'  identify the cluster to be visualized.
#' @param cluster_variables A list in which keys have the same name as the
#'  clusters, and items indicate the relevant variables as strings.
#' @param variable_subset If FALSE (the default) all variables that are 
#'  specified in \code{cluster_variables} are used for the visualization.
#'  Otherwise, you can pass a list of variable names as strings to visualize
#'  onle those.
#' @param return_full_plot If TRUE (the default) function combines the single
#'  plots into one full plot (using \code{ggpubr:ggarrange}. If FALSE a list of
#'  single plots is returned.)
make_plots <- function(data_used,
                       cluster_considered, 
                       cluster_variables,
                       variable_subset=FALSE,
                       return_full_plot=TRUE){
  if (!(FALSE %in% variable_subset)){
    cluster_variables <- variable_subset
  }
  
  cluster_data <-  setup_taxonomy_data(data_used, 
                                       cluster_considered, 
                                       cluster_variables)
  
  plots_to_do <- names(cluster_data)[2:length(names(cluster_data))]
  final_plots <- list()
  
  for (p in plots_to_do){
    print(p)
    final_plots[[p]] <- ggplot(cluster_data,
                               aes_string(x="cluster", 
                                          y=p,
                                          fill="cluster",
                                          color="cluster")) +
      geom_bar(stat = "identity") +
      ggtitle(p) + 
      scale_y_continuous(expand = c(0, 0))
    theme_bw() + 
      theme(panel.border = element_blank(),
            axis.line = element_line(),
            legend.position = "none")
  }
  if (return_full_plot){
    full_plot <- ggpubr::ggarrange(
      plotlist = final_plots, 
      ncol = length(names(final_plots)), 
      legend = "none")
    return(full_plot)
  } else {
    return(final_plots)
  }
}

