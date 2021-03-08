library(shiny)
library(shinythemes)

  ui = fluidPage(
    theme = shinytheme("yeti"),
    headerPanel(title='IEEE CIS fraud detection'),
    mainPanel(
      tabsetPanel(
        tabPanel("homepage", includeMarkdown("intro.md")),
        tabPanel("transction", DT::dataTableOutput("mytable1")),
        tabPanel("identity", DT::dataTableOutput("mytable2")),
        tabPanel("transaction structure", verbatimTextOutput("structure1")),
        tabPanel("identity structure", verbatimTextOutput("structure2")),
        tabPanel("isFraud", imageOutput("fraud")),
        tabPanel("model result plot", imageOutput("model1"), imageOutput("model2"), imageOutput("model3")),
        tabPanel("model result table", DT::dataTableOutput("df"))
      )
  )
)

