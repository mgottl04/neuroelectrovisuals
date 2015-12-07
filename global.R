library(shiny)
library(shinyBS)
library(shinyTree)
library(ggvis)
library(dplyr)
library(shinyjs)

bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',row.names = 1)

# Data clean up - remove data with nonsense levels
bigData <- bigData[!bigData$Species %in% c("Rats, Mice","Mice, Xenopus"),]

for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
    }
}
  
axis_names <- sapply(colnames(bigData),function(x){
  !(is.character(bigData[,x]) & length(unique(bigData[,x])) > 15)
})

bigData$key <-(1:nrow(bigData))
bigData[bigData$key,]

# Neuron types
neuron_types <- na.omit(unique(bigData[,c('NeuronName','BrainRegion')]))
regions <- levels(as.factor(neuron_types$BrainRegion))
region_groups <- lapply(regions, function(x) {
  structure(as.list(setNames(neuron_types[neuron_types$BrainRegion == x,c('NeuronName')],
                   neuron_types[neuron_types$BrainRegion == x,c('NeuronName')])),stselected=TRUE)})
region_groups <- structure(as.list(setNames(region_groups, regions)),stselected=TRUE)

# Organism metadata
species <- levels(as.factor(bigData$Species))
misc_species <- species[!species %in% c("Rats", "Mice")]
age <- na.omit(bigData$AnimalAge)

# Ephys props
props <- na.omit(ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range")])
prop_names <- rownames(props)

