#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("global.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  observeEvent(input$submitchoice, {
    
    if(input$variable_1 == "Select an option" || input$variable_2 == "Select an option"){
      output$cplot <- renderUI({
        NULL
      })
      output$cplotonecolor <- renderUI({
        NULL
      })
      output$cplotonefacet <- renderUI({
        NULL
      })
      output$tvmessage <- renderUI({
        tags$h4("Please select two variables to get a correlation")
      })
    }else{
      
      Variable_1 <- variable_info$variables[as.numeric(input$variable_1)]
      Variable_1_Type <- (variable_info[variable_info$variables == Variable_1, ])$varType
      Variable_2 <- variable_info$variables[as.numeric(input$variable_2)]
      Variable_2_Type <- (variable_info[variable_info$variables == Variable_2, ])$varType
      
      if(Variable_1_Type == "continuous" && Variable_2_Type == "continuous"){
        output$tvmessage <- renderUI({
        })
        
        if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          output$cplotonecolor <- renderUI({
            NULL
          })
          output$cplotonefacet <- renderUI({
            NULL
          })
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
        }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
          
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
          
          output$cplotonecolor <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var)
          })
          
          
          
          output$cplotonefacet <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
          })
          
          
        }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
          
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
          
          output$cplotonecolor <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var)
          })
          
          
          
          output$cplotonefacet <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
          })
          
        }else{
          
        }
        
        
      }else if(Variable_1_Type == "categorical" && Variable_2_Type == "categorical"){
        output$tvmessage <- renderUI({
        })
        output$caplot <- renderPlot({
          plotBothCategorical(Variable_1,Variable_2,input$stratification_variable_1,input$stratification_variable_2)
        })
      }else{
        output$tvmessage <- renderUI({
        })
        output$ccaplot <- renderPlot({
          plotConCat(Variable_1,Variable_2,input$stratification_variable_1,input$stratification_variable_2)
        })
      }
      
    }
    
    
    
  })
})

