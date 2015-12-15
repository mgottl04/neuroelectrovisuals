library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
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
library(pheatmap)
source("./mod.edge2HPD.R")
source("./mod.mineHPD.R")
source("./hive.R")
source('./lib/make_frequency_matrix.R')


#load('data/hive_plot_data.RData')
#hive_data <- read.csv(file='data/hive_data.csv')
bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',stringsAsFactors = FALSE, row.names = 1)
plottables <- scan('data/plotting_attribs.csv',what="",sep=",")

# *** Data clean-up ***

# Change ephys_info rownames to match bigData column names
rownames(ephys_info) <- gsub(" ", ".", rownames(ephys_info))
rownames(ephys_info) <- gsub("-", ".", rownames(ephys_info))

# Drop "other" from ephys
ephys_info <- ephys_info[rownames(ephys_info) != "other",]

# Fix nonsense levels
bigData[bigData$Species %in% c("Rats, Mice","Mice, Xenopus"),c("Species")] <- "Other"

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

# *** End data clean-up

# Get names to put on axes
axis_names <- sapply(colnames(bigData),function(x){
  x %in% plottables
})

# Get properties that need log transformation of axes
log_transform <- rownames(ephys_info[ephys_info$Transform == "log10",])

bigData$key <-(1:nrow(bigData))
bigData$superkey <- paste(paste0('Title: ',bigData$Title,sep=''),paste0('PubMed ID: ',bigData$Pmid,sep=''),paste0('Table ID: ',bigData$key,sep=''),sep='@')
bigData$allNeurons <- unlist(lapply(bigData$Pmid,function(x){
  paste(unlist(unique(bigData[which(bigData$Pmid == x), 'NeuronName'])),collapse=',  ')
}))
bigData$allSpecies <- unlist(lapply(bigData$Pmid,function(x){
  paste(unlist(unique(bigData[which(bigData$Pmid == x), 'Species'])),collapse=',\n ')
})) 
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
weight <- na.omit(bigData$AnimalWeight)
temp <- na.omit(bigData$RecTemp)
metadata_units <- list(AnimalAge = "days", AnimalWeight = "grams", RecTemp="C")

# Ephys props
props <- ephys_info[order(rownames(ephys_info)),c("usual.units","Min.Range","Max.Range"),drop=FALSE]
prop_names <- rownames(props)

# Four groups for ephys props panels
g1_start <- 1 
g1_end <-  length(prop_names[grepl("^[a-cA-C]", prop_names)])
g2_start <- g1_end + 1
g2_end <- g2_start + length(prop_names[grepl("^[d-lD-L]", prop_names)]) - 1
g3_start <- g2_end + 1
g3_end <- g3_start +length(prop_names[grepl("^[m-rM-R]|^[Ss][a-lA-L]", prop_names)]) - 1
g4_start <- g3_end + 1
g4_end <- g4_start + length(prop_names[grepl("^[Ss][m-zM-Z]|^[t-zT-Z]", prop_names)]) - 1

# Useful lists
ephys_props <- c("input.resistance","resting.membrane.potential","spike.threshold","spike.amplitude","spike.half.width","membrane.time.constant",
                 "AHP.amplitude","spike.width","cell.capacitance","AHP.duration","rheobase","firing.frequency",
                 "adaptation.ratio","sag.ratio","fast.AHP.amplitude","spike.peak","maximum.firing.rate","other",
                 "spontaneous.firing.rate","FI.slope","first.spike.latency","slow.AHP.amplitude","spike.max.rise.slope","ADP.amplitude",
                 "sag.amplitude","spike.max.decay.slope","spike.rise.time","fast.AHP.duration","spike.decay.time","access.resistance",
                 "slow.AHP.duration","cell.diameter","medium.AHP.amplitude","medium.AHP.duration","ADP.duration","cell.surface.area")
metadata <- c("Species", "Strain", "ElectrodeType", "PrepType", "JxnPotential", "JxnOffset", "RecTemp", "AnimalAge", "AnimalWeight", "ExternalSolution", "InternalSolution")

