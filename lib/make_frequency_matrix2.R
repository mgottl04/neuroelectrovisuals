

make_frequency_matrix <- function(keys){
  load('data/frequence_matrices.rda')
  load('data/matrix.rda')
  
  
  fab <- as.data.frame(matrix(data = 0, nrow = 28, ncol = 28))
  colnames(fab) <- colnames(frequency_data)[-18]
  
  fab <-Reduce('+',c(fab,matrices[intersect(frequency_data$key,keys)]))
  rownames(fab) <- colnames(frequency_data)[-18]
  
  plz <- data.frame(c(rep('Ephys',10),rep('Metadata',7),rep('Brain Region',11)))
  rownames(plz) <- rownames(fab)
  names(plz) <- 'Data Type'
  
  pheatmap(fab2,cluster_rows = FALSE, cluster_cols = FALSE,
           color = colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))(801),
           breaks = c(1:800),
           annotation_row = plz,annotation_col = plz, display_numbers = TRUE,number_format = "%.0f", number_color = 'black')
  
}
