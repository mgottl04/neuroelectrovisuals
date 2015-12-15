# File for hive plot creation
############################################################################################
makeHivePlot = function(bigData = bigData) { 

  #bigData[is.na(bigData$BrainRegion),] <- "Other"
  #bigData$BrainRegion <- gsub(" ", ".", bigData$BrainRegion)
  
  plot.d <- subset(bigData, select = c("BrainRegion"))
  
  plot.d$id <- 1 : nrow(plot.d)
  plot.d$value <- plot.d$BrainRegion
  
  plot.d$value[!plot.d$BrainRegion %in% bigData$BrainRegion] <- NA
  
  plot.d.cast <- dcast(data = plot.d, formula = id ~ BrainRegion, value.var = "value")
  plot.d.cast <- as.data.frame(plot.d.cast[,!(names(plot.d.cast) %in% c("id"))])
  
  if (ncol(plot.d.cast) == 1) {
    colnames(plot.d.cast) <- plot.d.cast[1,1]
  }
  
  #bigDataTest <- rbind(bigData, data.frame(matrix(rep(NA, (nrow(plot.d) - nrow(bigData)) * ncol(bigData)))))
  
  plot.d  <- subset(bigData, select = c(ephys_props, metadata))
  plot.d <- cbind(plot.d, plot.d.cast)
  
  plot.d.pairs <- count.pairwise(plot.d, diagonal = FALSE)
  
  dataSet1 <- expand.grid(V1 = unique(bigData[,"BrainRegion"]), V2 = c(ephys_props, metadata), stringsAsFactors = FALSE)
  
  dataSet2 <- expand.grid(V1 = ephys_props, V2 = metadata, stringsAsFactors = FALSE)
  dataSet <- rbind(dataSet1, dataSet2)
  
  dataSet$V3 <- apply(dataSet, 1, function(x) {
    plot.d.pairs[x[["V1"]], x[["V2"]]]
  })
  
  dataSet <- dataSet %>% filter(V3 > 10)
  
  ############################################################################################
  # Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
  gD <- simplify(graph.data.frame(dataSet, directed=FALSE))
  
  # Calculate some node properties and node similarities that will be used to illustrate different plotting abilities
  
  # Calculate degree for all nodes
  degAll <- degree(gD, v = V(gD), mode = "all")
  
  # Calculate betweenness for all nodes


  node.list <- data.frame(name = V(gD)$name, degree = degAll)
  
  # Calculate Dice similarities between all pairs of nodes
  dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")
  
  # Calculate edge weight based on the number of evidence lines
  normalize <- function(x, ...) {
    (x - min(x, ...)) / (max(x, ...) - min(x, ...))
  }
  dataSet.ext <- cbind(dataSet, V4 = normalize(dataSet$V3, na.rm = T))
  
  rm(degAll, dsAll, gD)
  ############################################################################################
  #Determine node/edge color based on the properties
  
  # Calculate node size
  # We'll interpolate node size based on the node betweenness centrality, using the "approx" function
  # And we will assign a node size for each node based on its betweenness centrality

  
  # B-centrality does not tell us much for our data, so let's just keep all nodes at the same size for now.
  node.list <- cbind(node.list, size = rep(1, nrow(node.list)))

  
  # Define node color
  # We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
  # This function returns a function corresponding to a color pallete of "bias" number of elements
  F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"))
  # Now we'll create a color for each degree
  colCodes <- F2(length(unique(node.list$degree)))
  # And we will assign a color for each node based on its degree
  nodes_col <- sapply(node.list$degree, function(x) colCodes[which(sort(unique(node.list$degree)) == x)])
  node.list <- cbind(node.list, color = nodes_col)
  rm(F2, colCodes, nodes_col)
  
  # Assign visual attributes to edges using the same approach as we did for nodes
  #dataSet.ext <- dataSet.ext[order(dataSet.ext$V4),]
  F2 <- colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))
#   F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(dataSet.ext$V4)), space = "rgb", interpolate = "linear")
  colCodes <- F2(length(unique(dataSet.ext$V4)))
  edges_col <- sapply(dataSet.ext$V4, function(x) colCodes[which(sort(unique(dataSet.ext$V4)) == x)])
  dataSet.ext <- cbind(dataSet.ext, color = edges_col)

  
  ############################################################################################
  # Assign nodes to axes
  #browser()
  nodeAxis <- integer(nrow(node.list))

  nodeAxis[node.list$name %in% regions] <- as.integer(1)
  nodeAxis[node.list$name %in% ephys_props] <- as.integer(2)
  nodeAxis[nodeAxis == 0] <- as.integer(3)

#   nodeAxis[1 : num_neurons] <- as.integer(1)
#   nodeAxis[(num_neurons + 1) : (num_neurons + num_ephys)] <- as.integer(2)
#   nodeAxis[(num_neurons + num_ephys + 1) : nrow(node.list)] <- as.integer(3)
  node.list <- cbind(node.list, axis = nodeAxis)

  ############################################################################################
  # Set radius for each node (distance along the axis)
  num_neurons <- length(node.list$name[node.list$name %in% regions])
  num_ephys <- length(node.list$name[node.list$name %in% ephys_props])

  nodeRadius <- integer(nrow(node.list))
  nodeRadius[1 : num_neurons] <- 2 * (1 : num_neurons)
  nodeRadius[(num_neurons + 1) : (num_neurons + num_ephys)] <- 3 * (1 : num_ephys)
  nodeRadius[(num_neurons + num_ephys + 1) : nrow(node.list)] <- 3 * (1 : (nrow(node.list) - num_neurons - num_ephys))
  node.list <- cbind(node.list, radius = nodeRadius)

  ############################################################################################
  # Craete a csv file for node labels

  nodeAngle <- numeric(nrow(node.list))
  nodeAngle[1 : num_neurons] <- 20
  nodeAngle[(num_neurons + 1) : (num_neurons + num_ephys)] <- 120
  nodeAngle[(num_neurons + num_ephys + 1) : nrow(node.list)] <- 245

  nodeLabRadius <- numeric(nrow(node.list))
  nodeLabRadius[1 : num_neurons] <- 3 * (num_neurons : 1) - 2
  nodeLabRadius[(num_neurons + 1) : (num_neurons + num_ephys)] <- 3 * (num_ephys : 1) + 5
  nodeLabRadius[(num_neurons + num_ephys + 1) : nrow(node.list)] <- 3 * ((nrow(node.list) - num_neurons - num_ephys) : 1) + 5

  nodeLabels <- data.frame(node.lab = as.character(node.list$name), 
                           node.text = gsub("\\.", " ", as.character(node.list$name)), 
                           angle = nodeAngle, 
                           radius = nodeLabRadius, 
                           offset = 1, 
                           hjust = 0.5, 
                           vjust = 0, 
                           stringsAsFactors = F)

  write.table(nodeLabels, "~/Documents/neuroelectrovisuals/data/nodeLabels.csv", sep = ",", row.names = FALSE)
  rm(nodeAxis, num_neurons, nodeRadius, num_ephys, nodeAngle, nodeLabRadius, nodeLabels)


  ############################################################################################
  # Create a hive plot
  hive1 <- mod.edge2HPD(edge_df = dataSet.ext[,1:2],
                        edge.color = dataSet.ext[, 5],
                        edge.weight = 3*log10(dataSet.ext[,3])/max(log10(dataSet.ext[,3])),
                        node.radius = node.list[,c("name", "radius")],
                        node.color = node.list[,c("name", "color")],
                        node.size = node.list[,c("name", "size")],
                        node.axis = node.list[,c("name", "axis")])  
  

  # Assign position on the axis to nodes (sort by number of connections, outer nodes have the most connections)
  #hive2 <- mod.mineHPD(hive1, "rad <- tot.edge.count")
  p <- plotHive(hive1, method = "abs", bkgnd = "black", axLabs = c("Brain region", "Ephys. property", "Metadata"), axLab.pos = 3, anNodes = "~/Documents/neuroelectrovisuals/data/nodeLabels.csv")
  return(p)
}