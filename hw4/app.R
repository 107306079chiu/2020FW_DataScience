#load dataset
data(iris)
#log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
#apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
#prepare the dataframe of original data for displaying in output
b<-c(iris[,5])
for(q in c(1:length(b))){
    if(b[q]==1){b[q]<-'setosa'}
    else if(b[q]==2){b[q]<-'versicolor'}
    else{b[q]<-'virginica'}}
a<-data.frame(
    sepal.length=iris[,1],sepal.width=iris[,2],
    petal.length=iris[,3],petal.width=iris[,4],
    species=b)

library(ggbiplot)
#install.packages("remotes")
#remotes::install_github("vqv/ggbiplot")
#install.packages("shiny")
library(shiny)

#define UI 
ui<-fluidPage(
    #title
    titlePanel("Hello iris!"),
    #UI of input 
    column(
        #displaying size, between 1-12
        3,
        #grouped checkbox which specifies components to be showed
        radioButtons(
            "inputPCs",
            h4("Components:"),
            choices=list(
                'PC1 & PC2'=1,
                'PC1 & PC3'=2,
                'PC1 & PC4'=3,
                'PC2 & PC3'=4,
                'PC2 & PC4'=5,
                'PC3 & PC4'=6
            ),
            selected=1
        ),
        #display variance
        h4("PC's variance:"),
        tableOutput('dataVar'),
        #takes numeric input and display that number of original data
        numericInput(
            'num',
            h4('Number of displayed data:'),
            value=10
        )
    ),
    #UI of output
    column(
        9,
        #show plot
        h4('PCA:'),
        plotOutput(outputId='distPlot'),
        #show dataset
        h4('Iris dataset:'),
        tableOutput('dataTable')
    ),
)

#define server logic required to output needed gram & table
server<-function(input, output){
    #output plot of PCA analysis
    output$distPlot<-renderPlot(
        {
            userInput<-input$inputPCs
            #determine which two PCs to be compared
            if(userInput==1){PClist<-c(1,2)}
            else if(userInput==2){PClist<-c(1,3)}
            else if(userInput==3){PClist<-c(1,4)}
            else if(userInput==4){PClist<-c(2,3)}
            else if(userInput==5){PClist<-c(2,4)}
            else{PClist<-c(3,4)}
            #plot
            g <- ggbiplot(ir.pca, choices = PClist, obs.scale = 1, var.scale = 1, groups = ir.species)
            g <- g + scale_color_discrete(name = '')
            g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
            plot(g)
        }
    )
    #output table of iris data
    output$dataTable<-renderTable(
        {
            numInput<-input$num
            if(numInput>150){numInput<-150}else if(numInput<=0){numInput<-0}
            #get specific number of data
            dis<-a[0:numInput,]
            dis
        },
        rownames = TRUE
    )
    #output table of PC's variance
    output$dataVar<-renderTable(
        {
            N_metrics<-matrix(c(ir.pca$sdev[1]^2,ir.pca$sdev[2]^2,ir.pca$sdev[3]^2,ir.pca$sdev[4]^2), ncol=1)
            colnames(N_metrics)<-c('variance')
            row.names(N_metrics)<-c('PC1','PC2','PC3','PC4')
            N_metrics
        },
        rownames = TRUE
    )
}

#run the application 
shinyApp(ui=ui,server=server)