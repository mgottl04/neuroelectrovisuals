bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1, na.strings = c('NA',''))
for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
  }
}
