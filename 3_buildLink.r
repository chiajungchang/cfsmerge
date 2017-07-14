#!/usr/bin/env Rscript
library(data.table)	
library("psych")
library("softImpute")
merged<-read.delim("index/uniqueIndexed.SampleID.tsv",check.names=F)


merged<-merged[,-1]
f <- sapply(merged, is.factor)
flevels=sapply(merged[,which(f)],levels)
for(fac in which(f)){merged[,fac]=as.numeric(merged[,fac])}
mm<-as.matrix(merged)
fit1=softImpute(mm,rank=29,lambda=29,type="als",maxit=1000)
fmm=complete(mm,fit1)
a<-apply(fmm,2,var)
fmm<-fmm[,a!=0]
x=as.data.frame(fmm)
for(i in 1:ncol(flevels)){
	x[,colnames(flevels)[i]]=flevels[x[,colnames(flevels)[i]],i]
}
write.table(x,"index/uniqueIndexed.m.SampleID.tsv",sep="\t",row.names=F)
b<-as.data.frame(t(matrix(unlist(strsplit(colnames(fmm),"._.",fixed=T)),nrow=2)))
rownames(b)=colnames(fmm)
colnames(b)=c("category","names")
write.table(b,"index/uniqueIndexed.SampleID.name",sep="\t")

cats=levels(b$category)

dat <- data.frame(i=NULL,j=NULL,type=NULL,value=NULL,pvalue=NULL)
for(i in 1:(length(cats))){
	for(j in (i):length(cats)){
		print(j)
		ii=which(b$category==cats[i])
		jj=which(b$category==cats[j])
		cc=corr.test(fmm[,ii,drop=F],fmm[,jj,drop=F],method="spearman",adjust="fdr")
		re=-log10(cc$p)
		ind=which(re>=1 ,arr.ind=T)
		if(nrow(ind)>0){
			d=data.frame(i=ii[ind[,1]],j=jj[ind[,2]],type="cor",value=cc$r[ind],pvalue=re[ind])
			dat <- rbindlist(list(dat,d))		
		}


	}
}
dat=dat[dat$i!=dat$j,]
dat$pvalue[dat$pvalue==Inf]=max(dat$pvalue[dat$pvalue!=Inf])+1
write.table(dat,"index/uniqueIndexed.SampleID.link",sep="\t",row.names=F)
