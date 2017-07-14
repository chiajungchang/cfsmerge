library(shiny)
library("networkD3")
library("igraph")
library("ggplot2")
library("bnlearn")
library("visNetwork")

#deal with file network 
indexCol<-read.table("index/indexColumns.tsv",sep="\t",header=T)
colnodes=data.frame(names=levels(indexCol$colname),types="field")
filenodes=data.frame(names=levels(indexCol$fileName),types="file")
nodes=rbind(colnodes,filenodes)
indexCol$i=match(indexCol$fileName,nodes$names)-1
indexCol$j=match(indexCol$colname,nodes$names)-1
indexCol$value=ifelse(indexCol$anyDuplicated==0,1,10)

#load pre computed gbc3net
load("bc3net.RData")
vbc3net<- toVisNetworkData(gbc3net)


# Define server logic required to draw a histogram
function(input, output,session) {


#for talbles to be loaded
	renameTable<-reactive({
		nameTable=read.table(paste("index/uniqueIndexed.",input$indexField,".name",sep=""),sep="\t",header=T)
#		rownames(nameTable)=make.names(rownames(nameTable))
		colors=rainbow(length(levels(nameTable$category)))
		nameTable$color=colors[as.numeric(nameTable$category)]
		nameTable$idd=1:nrow(nameTable)
		nameTable
	})
	dataTable<-reactive({
		dTable=read.table(paste("index/uniqueIndexed.m.",input$indexField,".tsv",sep=""),sep="\t",header=T,check.names=F)
	})
	refilteredLink<-reactive({
		linkTable=read.table(paste("index/uniqueIndexed.",input$indexField,".link",sep=""),sep="\t",header=T)
		linkTable$i=linkTable$i
		linkTable$j=linkTable$j
		linkTable[linkTable$pvalue>1,]
	})

#apply the filters
	shinyFilteredNode<-eventReactive(input$go,{
		filteredLink=refilteredLink()
		ii=1:nrow(renameTable())
		nodeindex=grep(input$pattern,rownames(renameTable()),perl=T,ignore.case=T)
		if(input$pm=="n"){
			nodeindex=ii[-nodeindex]
		}
		childindex=grep(input$cpattern,rownames(renameTable()),perl=T,ignore.case=T)
		if(input$cpm=="n"){
			childindex=ii[-childindex]
		}
		
		for(i in 1:input$connlevel){
			tmpnode=unique(filteredLink$j[filteredLink$i %in% nodeindex & filteredLink$pvalue>input$pvalue])
			tmpnode=unique(c(tmpnode,unique(filteredLink$i[filteredLink$j %in% nodeindex & filteredLink$pvalue>input$pvalue])))
			tmpnode=intersect(tmpnode,childindex)
			nodeindex=unique(c(nodeindex,tmpnode))
		}
		nodeindex

	})
	shinyFilteredEdge<-eventReactive(input$go,{
		filteredLink=refilteredLink()
		firstFiltered=filteredLink[filteredLink$pvalue>input$pvalue,]
		nodeindex=shinyFilteredNode()
		secondFiltered=firstFiltered[firstFiltered$i %in% nodeindex & firstFiltered$j %in% nodeindex & firstFiltered$pvalue>input$pvalue,]
		secondFiltered$i=match(secondFiltered$i,nodeindex)-1
		secondFiltered$j=match(secondFiltered$j,nodeindex)-1
		secondFiltered

	})
	shinyFilteredNodeTable<-reactive({
		renameTable()[shinyFilteredNode(),]
	})

############# for scatter plot
	pairData<-eventReactive(input$go2,{
		dataTable()[,c(as.numeric(input$xpos),as.numeric(input$ypos))]
	})
#get pre computed network using filters
	bc3table<-eventReactive(input$getbc3,{
		data=vbc3net
		data$edges=data$edges[data$edges$from %in% rownames(shinyFilteredNodeTable()) & data$edges$to %in% rownames(shinyFilteredNodeTable()),]
		nn<-union(data$edges$from,data$edges$to)
		data$nodes=data$nodes[data$nodes$id %in% nn,]
		data$nodes$label=shinyFilteredNodeTable()[data$nodes$id,"names"]
		data$nodes$group=shinyFilteredNodeTable()[data$nodes$id,"category"]
		data
	})
#dynamic computing bn network
	bntable<-eventReactive(input$calbn,{
		dd<-data.matrix(dataTable()[,shinyFilteredNode()])
		
		mm<-apply(dd,2,median,na.rm=T)
		for(i in 1:length(mm)){
    		dd[is.na(dd[,i]),i]=mm[i]
		}
		data=data.frame(dd)
		#remove constant
		x=apply(data,2,var)
		if(length(which(x!=0))>0){
			data<-data[,x!=0]
		}
	#	if(ncol(data)>50){
	#		return NULL
	#	}
		if(input$alg=="rsmax2"){
			grsmax2=rsmax2(data,test=input$itest)
		}
		else if(input$alg=="mmhc"){
			grsmax2=mmhc(data,test=input$itest)
		}
		else if(input$alg=="hc"){
			grsmax2=hc(data)
		}
		else if(input$alg=="tabu"){
			grsmax2=tabu(data)
		}
		else if(input$alg=="gs"){
			grsmax2=gs(data,test=input$itest)
		}
		else if(input$alg=="iamb"){
			grsmax2=iamb(data,test=input$itest)
		}
		else if(input$alg=="fast.iamb"){
			grsmax2=fast.iamb(data,test=input$itest)
		}
		else if(input$alg=="inter.iamb"){
			grsmax2=inter.iamb(data,test=input$itest)
		}
		else if(input$alg=="mmpc"){
			grsmax2=mmpc(data,test=input$itest)
		}
		else if(input$alg=="si.hiton.pc"){
			grsmax2=si.hiton.pc(data,test=input$itest)
		}
		nodes<-data.frame(id=names(grsmax2$nodes),label=names(grsmax2$nodes),stringsAsFactors=F)
		edges<-as.data.frame(grsmax2$arcs)
		data<-list(nodes=nodes,edges=edges)
		nn<-union(data$edges$from,data$edges$to)
		data$nodes=data$nodes[data$nodes$id %in% nn,]
		b<-as.data.frame(t(matrix(unlist(strsplit(data$node$id,"._.",fixed=T)),nrow=2)))
		data$nodes$label=b$V2
		data$nodes$group=b$V1
		data

	})
#### plot file network
  output$fileNetwork <- renderForceNetwork({
		forceNetwork(Links =indexCol , Nodes = nodes,
             Source = "i", Target = "j",
             NodeID = "names",
             Group = "types", # color nodes by betweeness calculated earlier
             charge = -70, # node repulsion
             linkDistance = 25,
             zoom = T,legend = T,opacity=0.8,Value="value")
  })
### casual network
	output$bnn<-renderVisNetwork({
	data=bntable()
	visNetwork(nodes = data$nodes, edges = data$edges) %>% visEdges(arrows = 'to') %>% visLegend(width=0.2)
  })
	output$vnbc3net<-renderVisNetwork({
	data=bc3table()
	visNetwork(nodes = data$nodes, edges = data$edges) %>% visEdges(arrows = 'to') %>% visLegend(width=0.2)
  })

### plot correlation network
  output$fieldNetwork<- renderForceNetwork({
		MyClickScript <- 
  ' if($("#saxis").val()=="x"){
		$("#xpos").val(d.idd);
		Shiny.onInputChange("xpos", d.idd);
	}
	else{
		$("#ypos").val(d.idd);
		Shiny.onInputChange("ypos", d.idd);
	}
'
		
		if(nrow(shinyFilteredNodeTable())==0 || nrow(shinyFilteredEdge())==0){
			stop("No link is found with given conditions")
		}
		fn=forceNetwork(Links = shinyFilteredEdge(), Nodes = shinyFilteredNodeTable(),
                  Source = "i", Target = "j",
                  NodeID = "names",
                  Group = "category", # color nodes by betweeness calculated earlier
                  charge = -70, # node repulsion
                  linkDistance = 25,height=1024,
                  zoom = T,legend = T,colourScale=JS("d3.scale.category10()"),opacity=0.8,clickAction = MyClickScript)
		fn$x$nodes$idd=shinyFilteredNodeTable()$idd
		fn
  })

#scatter plot
	output$scatter.plot<- renderPlot({
		dd=pairData()
		nms <- paste("`", colnames(dd), "`", sep="")
	  p<-ggplot(dd,mapping=aes_string(x=nms[1], y=nms[2]))+
		 geom_point()+geom_smooth(method=lm)
	   #  geom_boxplot(aes_string(fill=input$group_by))+
	    # geom_jitter(aes(samid=SampleID),width=0.1,height=0) 
	     # use geom_jitter instead of geom_point so that points with the same value could be separated
		p
  }) 
#download names
	output$downloadnode<- downloadHandler(
    filename = function() {
		"node.tsv"
    },
    content = function(file) {
		file.copy("index/uniqueIndexed_by_SampleID.name",file)
    }
  )
	output$grouplink<-renderUI({
		cc=renameTable()$category[as.numeric(input$xpos)]
		query=""
		nn=renameTable()$names[as.numeric(input$xpos)]
		cs=unlist(strsplit(as.character(cc),"/"))
		if(length(cs)>2){
			query=paste("?folder=",cs[1],"&subfolder=",cs[2],"&col=",nn,sep="")
		}
		else{
			query=""
		}
		a("link to group", href=paste("http://igenomed4.stanford.edu/shiny/shinyapp/johngroup/",query,sep=""),target="_blank")
	})
#	output$table<-renderDataTable(shinyFilteredNodeTable())
}
