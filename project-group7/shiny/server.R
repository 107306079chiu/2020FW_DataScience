library(shiny)
library(rsconnect)

transction <- read.csv('train-2.csv', stringsAsFactors = FALSE)
identity <- read.csv('identity.csv', stringsAsFactors = FALSE)

# Define server logic required to generate and plot a random distribution
server <- function(input, output) {
  
  transction2 = transction[sample(nrow(transction), 50), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(transction2, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })

  identity2 = identity[sample(nrow(identity), 50), ]
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(identity2, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$structure1 <- renderPrint({
    str(transction)
  })
  
  output$structure2 <- renderPrint({
    str(identity)
  })
  
  output$fraud <- renderImage({
    list(src = 'image1.jpeg')
  }, deleteFile = FALSE)
  
  output$model1 <- renderImage({
    list(src = 'model1.jpeg',
         width = 500,
         height = 500)
  }, deleteFile = FALSE)
  
  output$model2 <- renderImage({
    list(src = 'model2.jpeg',
         width = 500,
         height = 500)
  }, deleteFile = FALSE)
  
  output$model3 <- renderImage({
    list(src = 'model3.jpeg',
         width = 500,
         height = 500)
  }, deleteFile = FALSE)
  
  model <- c("KNN", "Naviebayes", "Null", "LGB")
  accuracy <- c("0.9613", "0.774", "0.9425", "0.982")
  precision <- c("0","0.982","0.0314","0.7716")
  sensitivity <- c("0","0.78","0.234","0.033")
  speficity <- c("0.9615","0.615","0.998","0.969")
  recall <- c("0","0.78","0.235","0.0337")
  F1 <- c("0","0.869","0.36","0.0325")
  kappa <- c("-0.00059","0.105","0.3536","0.0029")
  df <- data.frame(model=model,
                            accuracy=accuracy,
                            precision=precision,
                            sensitivity=sensitivity, 
                            speficity=speficity,
                            recall=recall,
                            F1=F1,
                            kappa=kappa)
  
  output$df <- DT::renderDataTable({
    DT::datatable(df)
  })
  
}



