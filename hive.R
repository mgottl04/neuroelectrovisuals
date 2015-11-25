library("igraph")
library("plyr")
library("HiveR")
library("RColorBrewer")
library("grDevices")
############################################################################################

dataSet <- data.frame(src = character(), sink = character(), count = numeric(), stringsAsFactors = FALSE)

ephys_props <- c("input.resistance","resting.membrane.potential","spike.threshold","spike.amplitude","spike.half.width","membrane.time.constant",
                 "AHP.amplitude","spike.width","cell.capacitance","AHP.duration","rheobase","firing.frequency",
                 "adaptation.ratio","sag.ratio","fast.AHP.amplitude","spike.peak","maximum.firing.rate","other",
                 "spontaneous.firing.rate","FI.slope","first.spike.latency","slow.AHP.amplitude","spike.max.rise.slope","ADP.amplitude",
                 "sag.amplitude","spike.max.decay.slope","spike.rise.time","fast.AHP.duration","spike.decay.time","access.resistance",
                 "slow.AHP.duration","cell.diameter","medium.AHP.amplitude","medium.AHP.duration","ADP.duration","cell.surface.area")
metadata <- c("Species", "Strain", "ElectrodeType", "PrepType", "JxnPotential", "JxnOffset", "RecTemp", "AnimalAge", "AnimalWeight", "ExternalSolution", "InternalSolution")

for (nt in unique(bigData[,"NeuronName"])) {
  for (ep in ephys_props) {
    dataSet[nrow(dataSet) + 1,] <- list(nt, ep, 0)
  }
}

for (ep in ephys_props) {
  for (meta in metadata) {
    dataSet[nrow(dataSet) + 1,] <- list(ep, meta, 0)
  }
}

increment_count <- function(src_ob, sink_ob) {
  dataSet[dataSet$src == src_ob & dataSet$sink == sink_ob,]$count <<- dataSet[dataSet$src == src_ob & dataSet$sink == sink_ob,]$count + 1
}

for (i in 1 : nrow(bigData)) {
  for (ep in ephys_props) {
    if (!is.na(bigData[i, ep])) {
      increment_count(bigData[i, "NeuronName"], ep)
      for (meta in metadata) {
        if (!is.na(bigData[i, meta])) {
          increment_count(ep, meta)
        }
      }
    }
  }
}

############################################################################################
# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
colnames(dataSet) <- c("V1","V2","V3")
dataSet <- subset(dataSet, V3 > 0)

gD <- simplify(graph.data.frame(dataSet, directed=FALSE))

# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities

# Calculate degree for all nodes
degAll <- degree(gD, v = V(gD), mode = "all")

# Calculate betweenness for all nodes
betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))

node.list <- data.frame(name = V(gD)$name, degree = degAll, betw = betAll.norm)

# Calculate Dice similarities between all pairs of nodes
dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")

# Calculate edge weight based on the node similarity
F1 <- function(x) {data.frame(V4 = dsAll[which(V(gD)$name == as.character(x$V1)), which(V(gD)$name == as.character(x$V2))])}
dataSet.ext <- ddply(dataSet, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))

rm(degAll, betAll, betAll.norm, F1)
############################################################################################
#Determine node/edge color based on the properties

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(node.list$bet)))
nodes_size <- sapply(node.list$bet, function(x) approxVals$y[which(sort(unique(node.list$bet)) == x)])
node.list <- cbind(node.list, size = nodes_size)
rm(approxVals, nodes_size)

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
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(dataSet.ext$V4)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(dataSet.ext$V4)))
edges_col <- sapply(dataSet.ext$V4, function(x) colCodes[which(sort(unique(dataSet.ext$V4)) == x)])
dataSet.ext <- cbind(dataSet.ext, color = edges_col)
rm(F2, colCodes, edges_col)

############################################################################################
# Assign nodes to axes

num_neurons <- length(unique(bigData[,"NeuronName"]))
nodeAxis <- integer(nrow(node.list))
nodeAxis[1 : num_neurons] <- as.integer(1)
nodeAxis[(num_neurons + 1) : (num_neurons + length(ephys_props))] <- as.integer(2)
nodeAxis[(num_neurons + length(ephys_props) + 1) : nrow(node.list)] <- as.integer(3)
node.list <- cbind(node.list, axis = nodeAxis)
rm(nodeAxis)

############################################################################################
#Create a hive plot

source("mod.edge2HPD.R")
source("mod.mineHPD.R")

#hive1 <- mod.edge2HPD(edge_df = dataSet.ext[,1:2], edge.weight = dataSet.ext[, 3], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "degree")], node.axis = node.list[,c("name", "axis")])
#sumHPD(hive1)

hive1 <- mod.edge2HPD(edge_df = dataSet.ext[,1:2], edge.color = dataSet.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.axis = node.list[,c("name", "axis")])

plotHive(hive1, method = "abs", bkgnd = "black", axLabs = c("Neuron Type", "Ephys. property", "Metadata"), axLab.pos = 1)


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
