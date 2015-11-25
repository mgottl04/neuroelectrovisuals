bigData <- read.csv('./data/article_ephys_metadata_curated.csv',sep = '\t',row.names = 1,stringsAsFactors = FALSE, na.strings = c('NA',''))
bigData <- bigData[,unlist(lapply(bigData, function(x){length(levels(x)) < 20}))]


for (i in 1:ncol(bigData)){
  if( is.factor(bigData[,i])){
    levels(bigData[,i]) <- c(levels(bigData[,i]),NA)
    }
  }

bigData$key <-(1:nrow(bigData))
bigData[bigData$key,]

# Emily's Dummy Data

TreeViewNode <- function(id,is_leaf,children) {
  me <- list(id=id,is_leaf=is_leaf,children=children)
  class(me) <- append(class(me),"TreeViewNode")
  return(me)
}

CheckBoxNode <- function(id,is_selected) {
  me <- list(id=id,is_selected=is_selected)
  class(me) <- append(class(me),"CheckBoxNode")
  return(me)
}

bertha <- CheckBoxNode("bertha",TRUE)
barb <- CheckBoxNode("barb",FALSE)
carl <- CheckBoxNode("carl",TRUE)
craig <- CheckBoxNode("craig",TRUE)
cunt <- CheckBoxNode("cunt",TRUE)

Bs <- TreeViewNode("Bs",TRUE,list(bertha,barb))
Cs <- TreeViewNode("Cs",TRUE,list(carl,craig,cunt))
directory3 <- TreeViewNode("directory3",FALSE,list(Bs,Cs))
directory2 <- TreeViewNode("directory2",FALSE,list(Bs,Cs))
directory <- TreeViewNode("directory",FALSE,list(directory2,directory3))

all_nodes <- list(bertha,barb,Bs,carl,craig,cunt,Cs,directory,directory2,directory3)
names(all_nodes) = c("bertha","barb","Bs","carl","craig","cunt","Cs","directory","directory2","directory3")

expanded_init = rep(FALSE,length(all_nodes))
names(expanded_init) = names(all_nodes)