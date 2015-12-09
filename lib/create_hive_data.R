fartData <- bigData[ ,c(ephys_props,metadata,'NeuronName','key')]

f<- as.data.frame(sapply(colnames(fartData),function(x){
  if (x == 'NeuronName'){
    fartData[,x]
  }else{
    fartData[,x][!is.na(fartData[,x])] <- 1
    fartData[,x][is.na(fartData[,x])] <- 0 
    
    fartData[,x]
  }
}))

write.csv(f,file='data/hive_data.csv')