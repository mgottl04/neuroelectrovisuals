make_frequency_matrix <- function(keys) {  
  f <- as.matrix(subset(frequency_data, key %in% keys, -key))
  f <- crossprod(f, f) # equivalent to t(f) %*% f, ie, dot products between pairs of columns

  anno <- data.frame('Data Type' = c(
      rep('Ephys', length(ephys_props)), 
      rep('Metadata', length(metadata)),
      rep('Brain Region', length(brain_regions))),
    row.names = rownames(f), check.names = FALSE)

  n <- floor(nrow(frequency_data) * 1.1)
  pheatmap(f, cluster_rows = FALSE, cluster_cols = FALSE, annotation_row = anno, annotation_col = anno,
    color = colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))(n+1), breaks = c(-1:n),
    display_numbers = TRUE, number_format = "%d", number_color = '#ffffff') # '#edf8b1'
}
