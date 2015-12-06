library(shiny)
library(shinyBS)
library(shinyTree)
library(ggvis)
library(dplyr)

biggerData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
bigData <- biggerData[,unlist(lapply(biggerData, function(x){length(levels(x)) < 20}))]
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',row.names = 1)

for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
    }
  }

bigData$key <-(1:nrow(bigData))
bigData[bigData$key,]

# Neuron types
neuron_types <- na.omit(unique(biggerData[,c('NeuronName','BrainRegion')]))
regions <- levels(as.factor(neuron_types$BrainRegion))
region_groups <- lapply(regions, function(x) {
  structure(as.list(setNames(neuron_types[neuron_types$BrainRegion == x,c('NeuronName')],
                   neuron_types[neuron_types$BrainRegion == x,c('NeuronName')])),stselected=TRUE,stopened=FALSE)})
region_groups <- structure(as.list(setNames(region_groups, regions)),stselected=TRUE)

# Ephys props
props <- na.omit(ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range")])
prop_names <- rownames(props)

# Organism metadata
species <- levels(as.factor(biggerData$Species))
species <- species[!species %in% c("Rats, Mice","Mice, Xenopus","Other")] # Removes garbage levels
misc_species <- species[!species %in% c("Rats", "Mice")]
age <- na.omit(biggerData$AnimalAge)

