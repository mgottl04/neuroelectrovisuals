fartData <- bigData[ ,c(ephys_props,metadata,'BrainRegion','key')]

f<- as.data.frame(sapply(colnames(fartData),function(x){
  if (x %in% c('BrainRegion','key')){
    fartData[,x]
  }else{
    fartData[,x][!is.na(fartData[,x])] <- 1
    fartData[,x][is.na(fartData[,x])] <- 0 
    
    as.numeric(fartData[,x])
  }
}),stringsAsFactors = FALSE)
f <- f[which(!is.na(f$BrainRegion)),]
for (i in unique(f$BrainRegion)){
  
  f[,i] <- 0 
  f[f$BrainRegion == i, i] <- 1

}
f$BrainRegion <- NULL

for (i in 1:29){
  f[,i] <- as.numeric(f[,i])
}

frequency_data <- f 
save(frequency_data, file ='matrix.rda')
