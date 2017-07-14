#!/usr/bin/env Rscript
#merge the table with unique index to integrate data
#extension 1. merge more than one index (e.g. values to map categories
index1<-read.table("index/indexColumns.tsv",sep="\t",header=T)
for(indexColumn in levels(index1$colname)){
	print(indexColumn)
	filesOfMapping=index1$fileName[index1$colname==indexColumn & index1$anyDuplicated==0]
	if(length(filesOfMapping)>0){
		dir=as.character(filesOfMapping[1])
		tb<-read.table(dir,sep="\t",header=T)
		colnames(tb)[colnames(tb)!=indexColumn]=paste(dir,colnames(tb)[colnames(tb)!=indexColumn],sep="._.")
		merged=tb
		for(dir in filesOfMapping[-1]){
			tb<-read.table(dir,sep="\t",header=T,check.names=F) # could change according to parser profile
			colnames(tb)[colnames(tb)!=indexColumn]=paste(dir,colnames(tb)[colnames(tb)!=indexColumn],sep="._.")
			merged<-merge(merged,tb,by=indexColumn,all.x=T)
			print(dir)
			print(nrow(merged))
		}
		colnames(merged)=sub("^../johndataInput/","",colnames(merged))
		colnames(merged)=sub("^data/","",colnames(merged))
		colnames(merged)=sub(".tsv.",".",colnames(merged))
		write.table(merged,paste("index/uniqueIndexed.",indexColumn,".tsv",sep=""),sep="\t",row.names=F)
	}
	
}
