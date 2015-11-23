bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = TRUE, na.strings = c('NA',''))
bigData <- bigData[,unlist(lapply(bigData, function(x){length(levels(x)) < 20}))]


for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
    }
  }

bigData$key <-(1:nrow(bigData))
bigData[bigData$key,]
