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

neuron_types <- na.omit(unique(biggerData[,c('NeuronName','BrainRegion')]))
regions <- levels(as.factor(neuron_types$BrainRegion))
region_groups <- lapply(regions, function(x) {
  as.list(setNames(neuron_types[neuron_types$BrainRegion == x,c('NeuronName')],
                   neuron_types[neuron_types$BrainRegion == x,c('NeuronName')]))})
region_groups <- as.list(setNames(region_groups, regions))

# Ephys props
props <- na.omit(ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range")])
prop_names <- rownames(props)

