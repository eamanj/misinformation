library(cluster)
# Function to print clustering performance stats
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}


add_inferred_treatment_variables <- function(data, method) {
  strength_variables <- paste0("QTIE_STR", c(paste0("0", 1:9), as.character(10:12)))
  # add self reported treatment variables on tie strength and extent of agreement
  data$inferred_group_treatment <- ifelse(data$tie_agree_numeric < median(data$tie_agree_numeric),
                                          "Outgroup", "Ingroup")
  if(is.numeric(method)) {
    data$inferred_tie_treatment <- ifelse(data$average_tie_strength < quantile(data$average_tie_strength, method),
                                          "Weak", "Strong")
    data$inferred_group_treatment <- ifelse(data$tie_agree_numeric < quantile(data$tie_agree_numeric, method),
                                            "Outgroup", "Ingroup")
  } else if(method == "median") {
    data$inferred_tie_treatment <- ifelse(data$average_tie_strength < median(data$average_tie_strength),
                                          "Weak", "Strong")
  } else if(method == "k-mode") {
    # K-modes clustering
    clusters <- kmodes(as.matrix(data[,strength_variables]), 2, iter.max = 100000, weighted = FALSE )
    data$inferred_tie_treatment <- clusters$cluster
    strong_clusters_counts <- table(data$inferred_tie_treatment[data$tie_treatment == "Strong"])
    strong_cluster <- ifelse(strong_clusters_counts[1] > strong_clusters_counts[2], 1, 2)
    data$inferred_tie_treatment <- ifelse(data$inferred_tie_treatment == strong_cluster, "Strong", "Weak")
  } else if(method == "divisive") {
    # hierarchical clustering using divisive method
    gower.dist <- daisy(data[,strength_variables], metric = c("gower"))
    divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = FALSE)
    #cstats.table(gower.dist, divisive.clust, 4)
    
    data$inferred_tie_treatment <- cutree(divisive.clust, k=2)
    strong_clusters_counts <- table(data$inferred_tie_treatment[data$tie_treatment == "Strong"])
    strong_cluster <- ifelse(strong_clusters_counts[1] > strong_clusters_counts[2], 1, 2)
    data$inferred_tie_treatment <- ifelse(data$inferred_tie_treatment == strong_cluster, "Strong", "Weak")
  } else if(method == 'agglomorative') {
    # hierarchical clustering using divisive method
    gower.dist <- daisy(data[,strength_variables], metric = c("gower"))
    aggl.clust.c <- hclust(gower.dist, method = "complete")
    #cstats.table(gower.dist, aggl.clust.c, 4)
    data$inferred_tie_treatment <- cutree(aggl.clust.c, k=2)
    strong_clusters_counts <- table(data$inferred_tie_treatment[data$tie_treatment == "Strong"])
    strong_cluster <- ifelse(strong_clusters_counts[1] > strong_clusters_counts[2], 1, 2)
    data$inferred_tie_treatment <- ifelse(data$inferred_tie_treatment == strong_cluster, "Strong", "Weak")
  } else {
    print(paste("Bad method:", method, " provided."))
    return(data)
  }
  data$inferred_tie_group_treatment <- paste(data$inferred_tie_treatment,
                                             data$inferred_group_treatment, sep="-")
  data$inferred_tie_group_format_treatment <- paste(data$inferred_tie_treatment,
                                                    data$inferred_group_treatment,
                                                    data$format_treatment, sep="-")
  
  # Numeric version of inferred treatment variables 
  data$inferred_tie_treatment_numeric <- ifelse(data$inferred_tie_treatment == "Strong", 0, 1)
  data$inferred_group_treatment_numeric <- ifelse(data$inferred_group_treatment == "Ingroup", 0, 1)
  
  print("Inferred Tie strength treatment conditional on treatment:")
  print(round(prop.table(table(data$tie_treatment, data$inferred_tie_treatment, dnn=c("Treatment", "Self_reported")),
              margin=1), 3))
  print("Inferred Tie group treatment conditional on treatment:")
  print(round(prop.table(table(data$group_treatment, data$inferred_group_treatment, dnn=c("Treatment", "Self_reported")),
              margin=1), 3))
  return(data)
}

median.ordered <- function(x) {
  levs <- levels(x)
  m <- median(as.integer(x))
  if(floor(m) != m) {
    warning("Median is between two values; using the first one")
    m <- floor(m)
  }
  return(ordered(m, labels = levs, levels = seq_along(levs)))
}
