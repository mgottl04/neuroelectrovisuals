make_frequency_matrix <- function(keys) {  
  f <- frequency_data[frequency_data$key %in% keys,]
  f$key <- NULL
  f <- as.matrix(f)
  f <- crossprod(f, f) # equivalent to t(f) %*% f, ie, dot products between pairs of columns

  plz <- data.frame('Data Type' = c(rep('Ephys',10),rep('Metadata',7),rep('Brain Region',11)),
    row.names = rownames(f), check.names = FALSE)

  pheatmap(f, cluster_rows = FALSE, cluster_cols = FALSE, annotation_row = plz, annotation_col = plz,
          color = colorRampPalette(rev(brewer.pal(n = 7, name ="YlGnBu")))(801), breaks = c(-1:800),
          display_numbers = TRUE, number_format = "%.0f", number_color = '#edf8b1')
}
