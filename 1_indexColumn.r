#!/usr/bin/env Rscript
#find common columns and test them if their level of duplication
# extension 1. integrate different ways ot parse the data
folder="../johndataInput"
dirs=dir(folder,pattern="*.tsv",full.names=T,recursive=T)
allColumns=NULL
for(dir in dirs){
	tb<-read.table(dir,sep="\t",header=T,nrows=1) # could change according to parser profile
	allColumns<-rbind(allColumns,cbind(dir,colnames(tb)))
	print(dir)
}
colnames(allColumns)=c("fileName","colname")
allColumns=as.data.frame(allColumns)
tableOfCol<-table(allColumns$colname)
indexColumns<-names(tableOfCol)[tableOfCol>1]
index1<-allColumns[allColumns$colname %in% indexColumns,]
index1$anyDuplicated=-1
for(dir in unique(index1$fileName)){
	columns=index1$colname[index1$fileName==dir]
	tb<-read.table(dir,sep="\t",header=T) # could change according to parser profile
	for(column in columns){
		index1$anyDuplicated[index1$fileName==dir & index1$colname==column]=anyDuplicated(tb[,column])
		
	}
}
write.table(index1,file="index/indexColumns.tsv",sep="\t",row.names =F)
