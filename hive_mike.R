

makeHivePlot_mike = function(keys) { 
 
  f <- hive_data[which(hive_data$key %in% keys),]
  ephys_props <- c("input.resistance","resting.membrane.potential","spike.threshold","spike.amplitude","spike.half.width","membrane.time.constant",
                   "AHP.amplitude","spike.width","cell.capacitance","AHP.duration","rheobase","firing.frequency",
                   "adaptation.ratio","sag.ratio","fast.AHP.amplitude","spike.peak","maximum.firing.rate","other",
                   "spontaneous.firing.rate","FI.slope","first.spike.latency","slow.AHP.amplitude","spike.max.rise.slope","ADP.amplitude",
                   "sag.amplitude","spike.max.decay.slope","spike.rise.time","fast.AHP.duration","spike.decay.time","access.resistance",
                   "slow.AHP.duration","cell.diameter","medium.AHP.amplitude","medium.AHP.duration","ADP.duration","cell.surface.area")
  metadata <- c("Species", "Strain", "ElectrodeType", "PrepType", "JxnPotential", "JxnOffset", "RecTemp", "AnimalAge", "AnimalWeight", "ExternalSolution", "InternalSolution")
  
  
 
  get_counts <- function(df,col1,col2){
    res <- count(df,c(col1,col2))
    names(res)<- c('V1','V2','V3')
    res <- res %>% filter(V2 != 0) %>% filter(V1 != 0)
    if (col1 == 'NeuronName'){
      if (nrow(res) == 0){
        res<- data.frame(V1 = col1,V2 = col2,V3 = 0,stringsAsFactors = FALSE)
     
      } else {
        res$V2 <- col2
      }
    } else {
      if(nrow(res) == 0){
        res<- data.frame(V1 = col1,V2 = col2,V3 = 0,stringsAsFactors = FALSE)
      } else{
        res$V1 <- col1
        res$V2 <- col2
      }
    }
    res
  } 

  f0 <- adply(c(metadata,ephys_props),1,function(x){
    get_counts(f,'NeuronName',x[[1]])
    
  })
  
  ephys_metadat_combinations <- expand.grid(V1 = ephys_props,V2 =metadata,stringsAsFactors = FALSE)
  
  f1 <- adply(ephys_metadat_combinations,1,function(x){
    get_counts(f,x[[1]],x[[2]])
  })
  
  association_stack <- rbind(f0[,-1],f1) 
  association_stack <- association_stack %>% filter(V3 != 0)
  association_stack <- association_stack[order(association_stack$V3),]
  association_stack$V4 <- colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))(nrow(association_stack))

  hive_mike <- mod.edge2HPD(edge_df = association_stack[,1:2],
                            edge.color = association_stack[,4],
                            edge.weight = log2(association_stack[,3]+1),  
                            node.color = node.list[,c("name", "color")],
                            node.size = node.list[,c("name", "size")],
                            node.axis = node.list[,c("name", "axis")]
                            )
                            
  
  plotHive(hive_mike, method = "abs",
           bkgnd = "black", 
           axLabs = c("Neuron Type", "Ephys. property", "Metadata"),
           axLab.pos = 1) 
}