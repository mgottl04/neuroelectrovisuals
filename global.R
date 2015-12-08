library(shiny)
library(shinyBS)
library(shinyTree)
library(ggvis)
library(dplyr)
library(shinyjs)
library(V8)

bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',row.names = 1)
rownames(ephys_info) <- gsub(" ", ".", rownames(ephys_info))
rownames(ephys_info) <- gsub("-", ".", rownames(ephys_info))

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
  as.list(setNames(neuron_types[neuron_types$BrainRegion == x,c('NeuronName')],
                   neuron_types[neuron_types$BrainRegion == x,c('NeuronName')]))})
region_groups <- as.list(setNames(region_groups, regions))

# Organism metadata
species <- levels(as.factor(bigData$Species))
misc_species <- species[!species %in% c("Rats", "Mice")]
age <- na.omit(bigData$AnimalAge)
age_max <- 2^ceiling(log2(max(age))) # max val for range slider

# Ephys props
props <- na.omit(ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range")])
prop_names <- rownames(props)

