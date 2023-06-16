#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("parameters.R")
source("plotBothContinuous.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  observeEvent(input$submitchoice, {
    
    if(input$variable_1 == "Select an option" || input$variable_2 == "Select an option"){
      output$cplot <- renderUI({
        NULL
      })
      output$tvmessage <- renderUI({
        tags$h4("Please select two variables to get a correlation")
      })
    }
    
    Variable_1 <- variables[as.numeric(input$variable_1)]
    if(Variable_1 %in% continuousVariables){
      Variable_1_Type <- "Continuous"
      }else{
      Variable_1_Type <- "Categorical"
      }
    Variable_2 <- variables[as.numeric(input$variable_2)]
    if(Variable_2 %in% continuousVariables) {
      Variable_2_Type <- "Continuous"
    }else{
      Variable_2_Type <- "Categorical"
    }
    
    if(Variable_1_Type == "Continuous" && Variable_2_Type == "Continuous"){
      output$tvmessage <- renderUI({
      })
      output$cplot <- renderPlot({
        plotBothContinuous(Variable_1,Variable_2,input$stratification_variable_1,input$stratification_variable_2)
      })
    }
    
  })
})

