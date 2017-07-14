library(shiny)
library(networkD3)
library(visNetwork)
library("ggplot2")

indexCol<-read.table("index/builtColumns.tsv",sep="\t",header=T)
# Define UI for application that draws a histogram
fluidPage(

  # Application title
  titlePanel("Merge for CFS"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
	  conditionalPanel(condition = "input.tabs1 == 'Field Network'",
		selectInput("indexField","Index Field",levels(indexCol$colname)),
		selectInput("pm","Pattern include?",choices = c("y","n"),selected="y"),
		textInput("pattern", "Parent Patterns", "CFS"),
		selectInput("cpm","Pattern include?",choices = c("y","n"),selected="y"),
		textInput("cpattern", "Childern Patterns", "."),
		h4("example1:Microbiome.*taxa_L2"),
		h4("example2:Metabolomics.*pca"),
		h4("example3:CFS|gender"),
		downloadButton("downloadnode", "Download all nodes"),
      	sliderInput("connlevel",
                  "Connection Level",
                  min = 1,
                  max = 3,
                  value = 1),
      	sliderInput("pvalue",
                  "-log10 p-value",
                  min = 1,
                  max = 30,
                  value = 2),
		actionButton("go", "Go"),
		selectInput("saxis","Select as axis",choices = c("x","y"),selected="x"),
		textInput("xpos", "x index", "1"),
		textInput("ypos", "y indexfunc", 1),
		actionButton("go2", "Scatter Plot"),
		uiOutput("grouplink")
		),
	  conditionalPanel(condition = "input.tabs1 == 'Causal Network'",
	
		selectInput("alg","algorithm",choices = c("rsmax2","mmhc","hc","tabu","gs","iamb","fast.iamb","inter.iamb","mmpc","si.hiton.pc"),selected="rsmax2"),
		selectInput("itest","independence tests",choices = c("mi-g","mc-mi-g","smc-mi-g","mi-g-sh","cor","mc-cor","smc-cor","mi-cg"),selected="mi-g"),
		h4("see R package bnlearn for details"),
		actionButton("calbn", "Calculate BN")
		),
	  conditionalPanel(condition = "input.tabs1 == 'bc3net'",
		actionButton("getbc3", "Get pre-computed network")
		),
		width=3
    ),

    # Show a plot of the generated distribution
    mainPanel(
		tabsetPanel(
        tabPanel("File Network", forceNetworkOutput("fileNetwork")),
        tabPanel("Field Network",forceNetworkOutput("fieldNetwork",height="1024px"),
			plotOutput("scatter.plot",width = "512px", height = "512px") ),
		tabPanel("Causal Network", visNetworkOutput("bnn", height = "1024px")),
		tabPanel("bc3net", visNetworkOutput("vnbc3net", height = "1024px"))
		,id="tabs1"
      ),width=9
    )
  )
)
