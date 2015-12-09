library(dplyr)
library(ggvis)
library(grDevices)
library(HiveR)
library(igraph)
library(plyr)
library(psych)
library(RColorBrewer)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyTree)
library(V8)

source("./mod.edge2HPD.R")
source("./hive.R")
source("./hive_mike.R")

load('data//hive_node_data.rda')
hive_data <- read.csv(file='data/hive_data.csv')
bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',stringsAsFactors = FALSE, row.names = 1)

# *** Data clean-up ***

# Change ephys_info rownames to match bigData column names
rownames(ephys_info) <- gsub(" ", ".", rownames(ephys_info))
rownames(ephys_info) <- gsub("-", ".", rownames(ephys_info))

# Drop "other" from ephys
ephys_info <- ephys_info[rownames(ephys_info) != "other",]

# Fix nonsense levels
bigData[bigData$Species %in% c("Rats, Mice","Mice, Xenopus"),c("Species")] <- "Other"

# Add log10 to units of log10 transformed props
ephys_info[ephys_info$Transform == "log10",c("usual.units")] <- paste("log10",ephys_info[ephys_info$Transform != "linear",c("usual.units")])

# Change min/max ranges to reflect actual data
for (x in rownames(ephys_info)) {
  ephys_info[x,"Min.Range"] <- floor(min(na.omit(bigData[,x])))
  ephys_info[x,"Max.Range"] <- ceiling(max(na.omit(bigData[,x])))
}

# Add NA to levels for some reason?
for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
    }
}

# Disallow big borgin' bompers on axes  
axis_names <- sapply(colnames(bigData),function(x){
  x != "other" & ((!(is.character(bigData[,x]) & length(unique(bigData[,x])) > 15) | x == "NeuronName"))
})

bigData$key <-(1:nrow(bigData))
bigData[bigData$key,]

# Neuron types
neuron_types <- na.omit(unique(bigData[,c('NeuronName','BrainRegion')]))
regions <- levels(as.factor(neuron_types$BrainRegion))
region_groups <- lapply(regions, function(x) {
  as.list(setNames(neuron_types[neuron_types$BrainRegion == x,c('NeuronName')],
                   neuron_types[neuron_types$BrainRegion == x,c('NeuronName')]))})
region_groups <- as.list(setNames(region_groups, regions))

# Organism metadata
species <- levels(as.factor(bigData$Species))
misc_species <- species[!species %in% c("Rats", "Mice")]
age <- na.omit(bigData$AnimalAge)
age_max <- 2^ceiling(log2(max(age))) # max val for range slider

# Ephys props
props <- ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range"),drop=FALSE]
prop_names <- rownames(props)

# Three groups for ephys props panels
g1_start <- 1 
g1_end <-  length(prop_names[grepl("^[a-fA-F]", prop_names)])
g2_start <- g1_end + 1
g2_end <- g2_start + length(prop_names[grepl("^[g-rG-R]|^[Ss][a-lA-L]", prop_names)]) - 1
g3_start <- g2_end + 1
g3_end <- g3_start + length(prop_names[grepl("^[Ss][m-zM-Z]|^[t-zT-Z]", prop_names)]) - 1

