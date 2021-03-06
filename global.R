libs <- c('dplyr','ggvis','RColorBrewer','reshape2','shiny','DT','shinyBS','shinyjs','shinyTree','pheatmap')
for (L in libs){
if(!require(L,character.only=TRUE)){
  install.packages(L)
}

}

source('./lib/make_frequency_matrix.R')

bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
ephys_info <- read.csv('data/ephys_prop_definitions.csv',sep = '\t',stringsAsFactors = FALSE, row.names = 1)
plottables <- scan('data/plotting_attribs.csv',what="",sep=",")

# *** Data clean-up ***

# Change ephys_info rownames to match bigData column names
rownames(ephys_info) <- gsub(" ", ".", rownames(ephys_info))
rownames(ephys_info) <- gsub("-", ".", rownames(ephys_info))

# Drop "other" from ephys
ephys_info <- ephys_info[rownames(ephys_info) != "other",]

# Add "other" brain region for all the neuron types that don't have one yet
bigData$BrainRegion[is.na(bigData$BrainRegion)] <- "Other Region"

# Fix nonsense levels
bigData[bigData$Species %in% c("Rats, Mice","Mice, Xenopus"),c("Species")] <- "Other"

# Change min/max ranges to reflect actual data
for (x in rownames(ephys_info)) {
  ephys_info[x,"Min.Range"] <- floor(min(na.omit(bigData[,x])))
  ephys_info[x,"Max.Range"] <- ceiling(max(na.omit(bigData[,x])))
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
props <- props[rownames(props) %in% plottables,]

# Two groups for ephys props panel
g1_start <- 1 
g1_end <-  length(rownames(props)[grepl("^[a-mA-M]", rownames(props))])
g2_start <- g1_end + 1
g2_end <- g2_start + length(rownames(props)[grepl("^[n-zN-Z]", rownames(props))]) - 1

# Useful lists
metadata <- c("Species", "Strain", "ElectrodeType", "PrepType", "JxnPotential", "JxnOffset", "RecTemp", "AnimalAge", "AnimalWeight", "ExternalSolution", "InternalSolution")
ephys_props <- c("input.resistance","resting.membrane.potential","spike.threshold",
                 "spike.amplitude","spike.half.width","membrane.time.constant",
                 "AHP.amplitude","cell.capacitance", "rheobase","maximum.firing.rate")
brain_regions <- unique(bigData$BrainRegion)

# raw indicator matrix used to build frequency matrix
frequency_data <- as.data.frame(cbind(key = bigData$key,
  sapply(c(ephys_props, metadata), function(x) as.integer(!is.na(bigData[x]))),
  sapply(brain_regions, function(b) as.integer(bigData$BrainRegion == b))))                 