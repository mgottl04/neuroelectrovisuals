make_frequency_matrix <- function(keys){
load('data/matrix.rda')
f <- frequency_data[which(frequency_data$key %in% keys),]
f$key <- NULL
fab <-adply(f,2,function(x){
  
  colSums(f[which(x == 1),])
  
  
})
rownames(fab) <- fab$X1
fab$X1 <- NULL
plz <- data.frame(c(rep('Ephys',10),rep('Metadata',7),rep('Brain Region',11)))
rownames(plz) <- rownames(fab)
names(plz) <- 'Data Type'

pheatmap(as.matrix(fab),cluster_rows = FALSE, cluster_cols = FALSE,
         color = colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))(100),
         annotation_row = plz,annotation_col = plz, display_numbers = TRUE,number_format = "%.0f", number_color = 'black')

}
