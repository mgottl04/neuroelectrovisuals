# File for hive plot creation
############################################################################################
makeHivePlot = function(bigData = bigData) { 
#   plot.d <- subset(bigData, select = c("NeuronName"))
  bigData[is.na(bigData$BrainRegion),] <- "Other"
  plot.d <- subset(bigData, select = c("BrainRegion"))
  
  plot.d$id <- 1:nrow(plot.d)
  plot.d$value <- plot.d$BrainRegion
  
  plot.d.cast <- dcast(data = plot.d, formula = id ~ BrainRegion, value.var = "value")
  plot.d.cast <- as.data.frame(plot.d.cast[,!(names(plot.d.cast) %in% c("id"))])
  
  if (ncol(plot.d.cast) == 1) {
    colnames(plot.d.cast) <- plot.d.cast[1,1]
  }
    
  plot.d  <- subset(bigData, select = c(ephys_props, metadata))
  plot.d <- cbind(plot.d, plot.d.cast)
  
  plot.d.pairs <- count.pairwise(plot.d, diagonal = FALSE)
  
  dataSet1 <- expand.grid(V1 = unique(bigData[,"BrainRegion"]), V2 = c(ephys_props, metadata), stringsAsFactors = FALSE)
  
  dataSet2 <- expand.grid(V1 = ephys_props, V2 = metadata, stringsAsFactors = FALSE)
  dataSet <- rbind(dataSet1, dataSet2)
  
  dataSet$V3 <- apply(dataSet, 1, function(x) {
    plot.d.pairs[x[["V1"]], x[["V2"]]]
  })
  
  dataSet <- dataSet %>% filter(V3 != 0)
  
  rm(plot.d, plot.d.pairs, plot.d.cast, dataSet1, dataSet2)

  ############################################################################################
  # Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
  
  gD <- simplify(graph.data.frame(dataSet, directed=FALSE))
  
  # Calculate some node properties and node similarities that will be used to illustrate different plotting abilities
  
  # Calculate degree for all nodes
  degAll <- degree(gD, v = V(gD), mode = "all")
  
  # Calculate betweenness for all nodes
  #betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
  #betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
  
  #node.list <- data.frame(name = V(gD)$name, degree = degAll, betw = betAll.norm)
  node.list <- data.frame(name = V(gD)$name, degree = degAll)
  
  # Calculate Dice similarities between all pairs of nodes
  dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")
  
  # Calculate edge weight based on the number of evidence lines
  normalize <- function(x, ...) {
    (x - min(x, ...)) / (max(x, ...) - min(x, ...))
  }
  dataSet.ext <- cbind(dataSet, V4 = normalize(dataSet$V3, na.rm = T))
  
  rm(degAll, F1, dsAll, gD, V4)
  ############################################################################################
  #Determine node/edge color based on the properties
  
  # Calculate node size
  # We'll interpolate node size based on the node betweenness centrality, using the "approx" function
  # And we will assign a node size for each node based on its betweenness centrality
  #approxVals <- approx(c(0.5, 1.5), n = length(unique(node.list$bet)))
  #nodes_size <- sapply(node.list$bet, function(x) approxVals$y[which(sort(unique(node.list$bet)) == x)])
  
  # Bcentrality does not tell us much for our data, so let's just keep all nodes at the same size for now.
  node.list <- cbind(node.list, size = rep(1, nrow(node.list)))
  #rm(approxVals, nodes_size)
  
  # Define node color
  # We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
  # This function returns a function corresponding to a color pallete of "bias" number of elements
  F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"), bias = length(unique(node.list$degree)), space = "rgb", interpolate = "linear")
  # Now we'll create a color for each degree
  colCodes <- F2(length(unique(node.list$degree)))
  # And we will assign a color for each node based on its degree
  nodes_col <- sapply(node.list$degree, function(x) colCodes[which(sort(unique(node.list$degree)) == x)])
  node.list <- cbind(node.list, color = nodes_col)
  rm(F2, colCodes, nodes_col)
  
  # Assign visual attributes to edges using the same approach as we did for nodes
  F2 <- colorRampPalette(rev(brewer.pal(n = 7, name ="Blues")), bias = length(unique(dataSet.ext$V4)), space = "rgb", interpolate = "linear")
#   F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(dataSet.ext$V4)), space = "rgb", interpolate = "linear")
  colCodes <- F2(length(unique(dataSet.ext$V4)))
  edges_col <- sapply(dataSet.ext$V4, function(x) colCodes[which(sort(unique(dataSet.ext$V4)) == x)])
  dataSet.ext <- cbind(dataSet.ext, color = edges_col)
  rm(F2, colCodes, edges_col)
  
  ############################################################################################
  # Assign nodes to axes
  
  num_neurons <- length(unique(bigData[,"BrainRegion"]))
  nodeAxis <- integer(nrow(node.list))
  nodeAxis[1 : num_neurons] <- as.integer(1)
  nodeAxis[(num_neurons + 1) : (num_neurons + nrow(subset(node.list, name %in% ephys_props)))] <- as.integer(2)
  nodeAxis[(num_neurons + nrow(subset(node.list, name %in% ephys_props)) + 1) : nrow(node.list)] <- as.integer(3)
  node.list <- cbind(node.list, axis = nodeAxis)
  rm(nodeAxis, num_neurons)
  
  ############################################################################################
  #Create a hive plot
  
  hive1 <- mod.edge2HPD(edge_df = dataSet.ext[,1:2], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.axis = node.list[,c("name", "axis")])
  
  # Assign position on the axis to nodes (sort by number of connections, outer nodes have the most connections)
  hive2 <- mod.mineHPD(hive1, "rad <- tot.edge.count")
  p <- plotHive(hive2, method = "abs", bkgnd = "gray", axLabs = c("Brain region", "Ephys. property", "Metadata"), axLab.pos = 5)
  return(p)
}

########################################
# Based on hierarchical cluestering
#d <- dist(dsAll)
#hc <- hclust(d)
#plot(hc)
#nodeAxis <- cutree(hc, k = 6)
#node.list <- cbind(node.list, axisCl = nodeAxis)
#rm(nodeAxis)

#hive1 <- mod.edge2HPD(edge_df = dataSet.ext[, 1:2], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.axis = node.list[,c("name", "axisCl")])
#sumHPD(hive1)

#hive2 <- mineHPD(hive1, option = "remove zero edge")

#plotHive(hive2, method = "abs", bkgnd = "black",  axLab.pos = 1)