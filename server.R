#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
source("global.R")





# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  clearLayout <- function(){
    
    output$cplot <- renderUI({
      NULL
    })
    
    hide("cplot")
    
    output$cplotonecolor <- renderUI({
      NULL
    })
    
    hide("cplotonecolor")
    
    output$cplotonefacet <- renderUI({
      NULL
    })
    
    hide("cplotonefacet")
    
    output$cplotonecolor2 <- renderUI({
      NULL
    })
    
    hide("cplotonecolor2")
    
    output$cplotonefacet2 <- renderUI({
      NULL
    })
    
    hide("cplotonefacet2")
    
    output$tvmessage <- renderUI({
      NULL
    })
    
    hide("tvmessage")
    
    output$cplottwostr <- renderUI({
      NULL
    })
    
    hide("cplottwostr")
    
    output$cplottwostralt <- renderUI({
      NULL
    })
    
    hide("cplottwostralt")
    
    output$cplottwostrfacet <- renderUI({
      NULL
    })
    
    hide("cplottwostrfacet")
    
    output$caplotNoStr <- renderUI({
      NULL
    })
    
    hide("caplotnostr")
    
    output$caplotonestr <- renderUI({
      NULL
    })
    
    hide("caplotonestr")
    
    output$caplotonestr2 <- renderUI({
      NULL
    })
    
    hide("caplotonestr2")
    
    output$caplottwostr <- renderUI({
      NULL
    })
    
    hide("caplottwostr")
  
    output$caplotmosaic <- renderUI({
      NULL
    })
    
    hide("caplotmosaic")
    
    output$caplotcount <- renderPlot({
      NULL
    })
    
    hide("caplotcount")
    
    output$caplotjitter <- renderPlot({
      NULL
    })
    
    hide("caplotjitter")
    
    
  }
  
  
  observeEvent(input$submitchoice, {
    
    if(input$variable_1 == "Select an option" || input$variable_2 == "Select an option"){
      
      clearLayout()
      show("tvmessage")
      output$tvmessage <- renderUI({
        tags$h4("Please select two variables to get a correlation")
      })
    }else{
      
      clearLayout()
      Variable_1 <- variable_info$variables[as.numeric(input$variable_1)]
      Variable_1_Type <- (variable_info[variable_info$variables == Variable_1, ])$varType
      Variable_2 <- variable_info$variables[as.numeric(input$variable_2)]
      Variable_2_Type <- (variable_info[variable_info$variables == Variable_2, ])$varType
      
      if(Variable_1_Type == "continuous" && Variable_2_Type == "continuous"){
        
        
        if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          show("cplot")
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
        }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
          
          show("cplot")
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
          show("cplotonecolor")
          output$cplotonecolor <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var)
          })
          
          
          show("cplotonefacet")
          output$cplotonefacet <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
          })
          
          
        }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
          
          show("cplot")
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
          show("cplotonecolor")
          output$cplotonecolor <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var)
          })
          
          
          show("cplotonefacet")
          output$cplotonefacet <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
          })
          
        }else{
          
          str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
          str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
          
          show("cplot")
          output$cplot <- renderPlot({
            plotBothContinuousNoStr(Variable_1,Variable_2)
          })
          
          show("cplotonecolor")
          output$cplotonecolor <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var_1)
          })
          
          
          show("cplotonefacet")
          output$cplotonefacet <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var_1)
          })
          
          show("cplotonecolor2")
          output$cplotonecolor2 <- renderPlot({
            plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var_2)
          })
          
          
          show("cplotonefacet2")
          output$cplotonefacet2 <- renderPlot({
            plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var_2)
          })
          
          show("cplottwostr")
          output$cplottwostr <- renderPlot({
            plotBothContinuoustwostr(Variable_1,Variable_2,str_var_1,str_var_2)
          })
          
          show("cplottwostralt")
          output$cplottwostralt <- renderPlot({
            plotBothContinuoustwostralt(Variable_1,Variable_2,str_var_1,str_var_2)
          })
          
          show("cplottwostrfacet")
          output$cplottwostrfacet <- renderPlot({
            plotBothContinuoustwostrfacet(Variable_1,Variable_2,str_var_1,str_var_2)
          })
          
        }
        
        
      }else if(Variable_1_Type == "categorical" && Variable_2_Type == "categorical"){
        
        if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          show("caplotnostr")
          output$caplotnostr <- renderPlot({
            plotBothCategoricalNoStr(Variable_1,Variable_2)
          })
          
          show("caplotmosaic")
          output$caplotmosaic <- renderPlot({
            plotBothCategoricalMosaic(Variable_1,Variable_2)
          })
          
          show("caplotcount")
          output$caplotcount <- renderPlot({
            plotBothCategoricalCount(Variable_1,Variable_2)
          })
          
          show("caplotjitter")
          output$caplotjitter <- renderPlot({
            plotBothCategoricalJitter(Variable_1,Variable_2)
          })
          
        }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
          
          show("caplotnostr")
          output$caplotnostr <- renderPlot({
            plotBothCategoricalNoStr(Variable_1,Variable_2)
          })
          
          show("caplotmosaic")
          output$caplotmosaic <- renderPlot({
            plotBothCategoricalMosaic(Variable_1,Variable_2)
          })
          
          show("caplotcount")
          output$caplotcount <- renderPlot({
            plotBothCategoricalCount(Variable_1,Variable_2)
          })
          
          show("caplotjitter")
          output$caplotjitter <- renderPlot({
            plotBothCategoricalJitter(Variable_1,Variable_2)
          })
          
          show("caplotonestr")
          output$caplotonestr <- renderPlot({
            plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var)
          })
          
          
        }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
          
          str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
          
          show("caplotnostr")
          output$caplotnostr <- renderPlot({
            plotBothCategoricalNoStr(Variable_1,Variable_2)
          })
          
          show("caplotmosaic")
          output$caplotmosaic <- renderPlot({
            plotBothCategoricalMosaic(Variable_1,Variable_2)
          })
          
          show("caplotcount")
          output$caplotcount <- renderPlot({
            plotBothCategoricalCount(Variable_1,Variable_2)
          })
          
          show("caplotjitter")
          output$caplotjitter <- renderPlot({
            plotBothCategoricalJitter(Variable_1,Variable_2)
          })
          
          show("caplotonestr")
          output$caplotonestr <- renderPlot({
            plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var)
          })
          
        }else{
          
          str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
          str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
          
          show("caplotnostr")
          output$caplotnostr <- renderPlot({
            plotBothCategoricalNoStr(Variable_1,Variable_2)
          })
          
          show("caplotmosaic")
          output$caplotmosaic <- renderPlot({
            plotBothCategoricalMosaic(Variable_1,Variable_2)
          })
          
          show("caplotcount")
          output$caplotcount <- renderPlot({
            plotBothCategoricalCount(Variable_1,Variable_2)
          })
          
          show("caplotjitter")
          output$caplotjitter <- renderPlot({
            plotBothCategoricalJitter(Variable_1,Variable_2)
          })
          
          show("caplotonestr")
          output$caplotonestr <- renderPlot({
            plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var_1)
          })
          
          show("caplotonestr2")
          output$caplotonestr2 <- renderPlot({
            plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var_2)
          })
          
          show("caplottwostr")
          output$caplottwostr <- renderPlot({
            plotBothCategoricalTwoStrfacet(Variable_1,Variable_2,str_var_1,str_var_2)
          })
          
        }
        
        
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

