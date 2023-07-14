#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load the required packages and scripts
library(shiny)
library(shinyjs)
source("global.R")





# Define server logic required to obtain plots between two variables
shinyServer(function(input, output,session) {
  

  
  hide("cat_visual_choice")
  hide("plot_height")
  hide("plot_width")
  
  #The function to clear all the outputs from the page
  clearLayout <- function(){
    
    output$cplot.ui <- renderUI({
      NULL
    })
    
    output$cplot <- renderPlot({
      NULL
    })
    
    hide("cplot.ui")
    
    output$cplotonecolor.ui <- renderUI({
      NULL
    })
    
    output$cplotonecolor <- renderPlot({
      NULL
    })
    
    hide("cplotonecolor.ui")
    
    output$cplotonefacet.ui <- renderUI({
      NULL
    })
    
    output$cplotonefacet <- renderPlot({
      NULL
    })
    
    hide("cplotonefacet.ui")
    
    output$cplotonecolor2.ui <- renderUI({
      NULL
    })
    
    output$cplotonecolor2 <- renderPlot({
      NULL
    })
    
    hide("cplotonecolor2.ui")
    
    output$cplotonefacet2.ui <- renderUI({
      NULL
    })
    
    output$cplotonefacet2 <- renderPlot({
      NULL
    })
    
    hide("cplotonefacet2.ui")
    
    output$tvmessage <- renderUI({
      NULL
    })
    
    hide("tvmessage")
    
    output$cplottwostr.ui <- renderUI({
      NULL
    })
    
    output$cplottwostr <- renderPlot({
      NULL
    })
    
    hide("cplottwostr.ui")
    
    output$cplottwostralt.ui <- renderUI({
      NULL
    })
    
    output$cplottwostralt <- renderPlot({
      NULL
    })
    
    hide("cplottwostralt.ui")
    
    output$cplottwostrfacet.ui <- renderUI({
      NULL
    })
    
    output$cplottwostrfacet <- renderPlot({
      NULL
    })
    
    hide("cplottwostrfacet.ui")
    
    output$caplotNoStr.ui <- renderUI({
      NULL
    })
    
    output$caplotNoStr <- renderPlot({
      NULL
    })
    
    hide("caplotnostr.ui")
    
    output$caplotonestr.ui <- renderUI({
      NULL
    })
    
    output$caplotonestr <- renderPlot({
      NULL
    })
    
    hide("caplotonestr.ui")
    
    output$caplotonestr2.ui <- renderUI({
      NULL
    })
    
    output$caplotonestr2 <- renderPlot({
      NULL
    })
    
    hide("caplotonestr2.ui")
    
    output$caplottwostr.ui <- renderUI({
      NULL
    })
    
    output$caplottwostr <- renderPlot({
      NULL
    })
    
    hide("caplottwostr.ui")
  
    output$caplotmosaic.ui <- renderUI({
      NULL
    })
    
    output$caplotmosaic <- renderPlot({
      NULL
    })
    
    hide("caplotmosaic.ui")
    
    output$caplotcount.ui <- renderUI({
      NULL
    })
    
    output$caplotcount <- renderPlot({
      NULL
    })
    
    hide("caplotcount.ui")
    
    output$caplotjitter.ui <- renderUI({
      NULL
    })
    
    output$caplotjitter <- renderPlot({
      NULL
    })
    
    hide("caplotjitter.ui")
    
    output$ccaplotnostrcol.ui <- renderUI({
      NULL
    })
    
    output$ccaplotnostrcol <- renderPlot({
      NULL
    })
    
    hide("ccaplotnostrcol.ui")
    
    
    output$ccaplotnostrbox.ui <- renderUI({
      NULL
    })
    
    output$ccaplotnostrbox <- renderPlot({
      NULL
    })
    
    hide("ccaplotnostrbox.ui")
    
    
    output$ccaplotnostrviolin.ui <- renderUI({
      NULL
    })
    
    output$ccaplotnostrviolin <- renderPlot({
      NULL
    })
    
    hide("ccaplotnostrviolin.ui")
    
    
    output$ccaplotnostrdot.ui <- renderUI({
      NULL
    })
    
    output$ccaplotnostrdot <- renderPlot({
      NULL
    })
    
    hide("ccaplotnostrdot.ui")
    
    
    output$ccaplotonestrbox.ui <- renderUI({
      NULL
    })
    
    output$ccaplotonestrbox <- renderPlot({
      NULL
    })
    
    hide("ccaplotonestrbox.ui")
    
    
    output$ccaplotonestrbox2.ui <- renderUI({
      NULL
    })
    
    output$ccaplotonestrbox2 <- renderPlot({
      NULL
    })
    
    hide("ccaplotonestrbox2.ui")
    
    
    output$ccaplottwostrbox.ui <- renderUI({
      NULL
    })
    
    output$ccaplottwostrbox <- renderPlot({
      NULL
    })
    
    hide("ccaplottwostrbox.ui")
    
    
    output$ccaplottwostrbox2.ui <- renderUI({
      NULL
    })
    
    output$ccaplottwostrbox2 <- renderPlot({
      NULL
    })
    
    hide("ccaplottwostrbox2.ui")
    
    
    output$caplotcountonestr.ui <- renderUI({
      NULL
    })
    
    output$caplotcountonestr <- renderPlot({
      NULL
    })
    
    hide("caplotcountonestr.ui")
    
    output$caplotcountonestr2.ui <- renderUI({
      NULL
    })
    
    output$caplotcountonestr2 <- renderPlot({
      NULL
    })
    
    hide("caplotcountonestr2.ui")
    
  }
  
  
  clearLayout()
  
  #The function to process the selected information when the user clicks on submit button
  observeEvent(input$submitchoice, {
    

    #Check if the user did not select two variables 
    if(input$variable_1 == "Select an option" || input$variable_2 == "Select an option"){
      
      clearLayout()
      show("tvmessage")
      output$tvmessage <- renderUI({
        tags$h4("Please select two variables to get a correlation")
      })
    }else{
      
      clearLayout()
      Variable_1 <- variable_info$variables[as.numeric(input$variable_1)]
      Variable_2 <- variable_info$variables[as.numeric(input$variable_2)]
      var_1_group <- gsub(" ","",paste(strsplit(Variable_1,"_")[[1]][1],"_group"))
      var_2_group <- gsub(" ","",paste(strsplit(Variable_2,"_")[[1]][1],"_group"))
      if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2!= "Select an option"){
        str_var_1 <- "NA"
        str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
      }else if (input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
        str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
        str_var_2 <- "NA"
      }else if (input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
        str_var_1 <- "NA"
        str_var_2 <- "NA"
      }else{
        str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
        str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
      }
      
      
      if(Variable_1 == Variable_2 || 
         Variable_1 == str_var_1 || 
         Variable_1 == str_var_2 ||
         Variable_2 == str_var_1 || 
         Variable_2 == str_var_2){
        
        hide("showDots")
        hide("xScale")
        hide("yScale")
        hide("cI")
        hide("cat_visual_choice")
        clearLayout()
        show("tvmessage")
        output$tvmessage <- renderUI({
          tags$h4("Please select different variables for correlation and stratification")
        })
        
      }else if(Variable_1 == var_2_group || Variable_2 == var_1_group){
        
        hide("showDots")
        hide("xScale")
        hide("yScale")
        hide("cI")
        hide("cat_visual_choice")
        clearLayout()
        show("tvmessage")
        output$tvmessage <- renderUI({
          tags$h4("Please ensure that variable 1 is not similar to variable 2")
        })
        
      }else if( var_1_group == str_var_1 || 
                var_1_group == str_var_2 || 
                var_2_group == str_var_1 || 
                var_2_group == str_var_2){
        
        hide("showDots")
        hide("xScale")
        hide("yScale")
        hide("cI")
        hide("cat_visual_choice")
        clearLayout()
        show("tvmessage")
        output$tvmessage <- renderUI({
          tags$h4("Please select the stratification variables that are not similar to the variables")
        })
        
      }else if(str_var_1!="NA" && str_var_2!="NA" && str_var_1 == str_var_2) {
        
        hide("showDots")
        hide("xScale")
        hide("yScale")
        hide("cI")
        hide("cat_visual_choice")
        clearLayout()
        show("tvmessage")
        output$tvmessage <- renderUI({
          tags$h4("Please select different stratification variables")
        })
        
      }else{
        
        #Retrieve the variable types of the selected variables
        clearLayout()
        Variable_1 <- variable_info$variables[as.numeric(input$variable_1)]
        Variable_1_Type <- (variable_info[variable_info$variables == Variable_1, ])$varType
        Variable_2 <- variable_info$variables[as.numeric(input$variable_2)]
        Variable_2_Type <- (variable_info[variable_info$variables == Variable_2, ])$varType
        
        #If else block to check the variable types of both the variables and then generate appropriate plots
        if(Variable_1_Type == "continuous" && Variable_2_Type == "continuous"){
          
          show("showDots")
          show("xScale")
          show("yScale")
          show("cI")
          hide("cat_visual_choice")
          show("plot_height")
          show("plot_width")
          #if else block to check how many stratifications are selected and then call appropriate functions to generate plots
          if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            show("cplot.ui")
            output$cplot.ui <- renderUI({
              plotOutput("cplot", height = input$plot_height,width = input$plot_width)
            })
            output$cplot <- renderPlot({
              plotBothContinuousNoStr(Variable_1,Variable_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            
            
          }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            
            show("cplot.ui")
            output$cplot.ui <- renderUI({
              plotOutput("cplot", height = input$plot_height,width = input$plot_width)
            })
            output$cplot <- renderPlot({
              plotBothContinuousNoStr(Variable_1,Variable_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            show("cplotonecolor.ui")
            output$cplotonecolor.ui <- renderUI({
              plotOutput("cplotonecolor", height = input$plot_height,width = input$plot_width)
            })
            output$cplotonecolor <- renderPlot({
              plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            
            # show("cplotonefacet")
            # output$cplotonefacet <- renderPlot({
            #   plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
            # })
            
            
          }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            show("cplot.ui")
            output$cplot.ui <- renderUI({
              plotOutput("cplot", height = input$plot_height,width = input$plot_width)
            })
            output$cplot <- renderPlot({
              plotBothContinuousNoStr(Variable_1,Variable_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            show("cplotonecolor.ui")
            output$cplotonecolor.ui <- renderUI({
              plotOutput("cplotonecolor", height = input$plot_height,width = input$plot_width)
            })
            output$cplotonecolor <- renderPlot({
              plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            
            # show("cplotonefacet")
            # output$cplotonefacet <- renderPlot({
            #   plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var)
            # })
            
          }else{
            
            str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            show("cplot.ui")
            output$cplot.ui <- renderUI({
              plotOutput("cplot", height = input$plot_height,width = input$plot_width)
            })
            output$cplot <- renderPlot({
              plotBothContinuousNoStr(Variable_1,Variable_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            show("cplotonecolor.ui")
            output$cplotonecolor.ui <- renderUI({
              plotOutput("cplotonecolor", height = input$plot_height,width = input$plot_width)
            })
            output$cplotonecolor <- renderPlot({
              plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var_1,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            
            # show("cplotonefacet")
            # output$cplotonefacet <- renderPlot({
            #   plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var_1)
            # })
            
            show("cplotonecolor2.ui")
            output$cplotonecolor2.ui <- renderUI({
              plotOutput("cplotonecolor2", height = input$plot_height,width = input$plot_width)
            })
            output$cplotonecolor2 <- renderPlot({
              plotBothContinuousOneStrColor(Variable_1,Variable_2,str_var_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            
            # show("cplotonefacet2")
            # output$cplotonefacet2 <- renderPlot({
            #   plotBothContinuousOneStrFacet(Variable_1,Variable_2,str_var_2)
            # })
            
            show("cplottwostr.ui")
            output$cplottwostr.ui <- renderUI({
              plotOutput("cplottwostr", height = input$plot_height,width = input$plot_width)
            })
            output$cplottwostr <- renderPlot({
              plotBothContinuoustwostr(Variable_1,Variable_2,str_var_1,str_var_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            show("cplottwostralt.ui")
            output$cplottwostralt.ui <- renderUI({
              plotOutput("cplottwostralt", height = input$plot_height,width = input$plot_width)
            })
            output$cplottwostralt <- renderPlot({
              plotBothContinuoustwostralt(Variable_1,Variable_2,str_var_1,str_var_2,input$showDots,input$xScale,input$yScale,input$cI)
            })
            
            # show("cplottwostrfacet")
            # output$cplottwostrfacet <- renderPlot({
            #   plotBothContinuoustwostrfacet(Variable_1,Variable_2,str_var_1,str_var_2)
            # })
            
          }
          
          
        }else if(Variable_1_Type == "categorical" && Variable_2_Type == "categorical"){
          
          hide("showDots")
          hide("xScale")
          hide("yScale")
          hide("cI")
          show("cat_visual_choice")
          show("plot_height")
          show("plot_width")
          #if else block to check how many stratifications are selected and then call appropriate functions to generate plots
          if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            show("caplotnostr.ui")
            output$caplotnostr.ui <- renderUI({
              plotOutput("caplotnostr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotnostr <- renderPlot({
              plotBothCategoricalNoStr(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotmosaic")
            # output$caplotmosaic <- renderPlot({
            #   plotBothCategoricalMosaic(Variable_1,Variable_2)
            # })
            
            show("caplotcount.ui")
            output$caplotcount.ui <- renderUI({
              plotOutput("caplotcount", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcount <- renderPlot({
              plotBothCategoricalCount(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotjitter")
            # output$caplotjitter <- renderPlot({
            #   plotBothCategoricalJitter(Variable_1,Variable_2)
            # })
            
          }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            
            show("caplotnostr.ui")
            output$caplotnostr.ui <- renderUI({
              plotOutput("caplotnostr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotnostr <- renderPlot({
              plotBothCategoricalNoStr(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotmosaic")
            # output$caplotmosaic <- renderPlot({
            #   plotBothCategoricalMosaic(Variable_1,Variable_2)
            # })
            
            show("caplotcount.ui")
            output$caplotcount.ui <- renderUI({
              plotOutput("caplotcount", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcount <- renderPlot({
              plotBothCategoricalCount(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotjitter")
            # output$caplotjitter <- renderPlot({
            #   plotBothCategoricalJitter(Variable_1,Variable_2)
            # })
            
            show("caplotcountonestr.ui")
            output$caplotcountonestr.ui <- renderUI({
              plotOutput("caplotcountonestr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcountonestr <- renderPlot({
              plotBothCategoricalCountOneStr(Variable_1,Variable_2,str_var,input$cat_visual_choice)
            })
            
            show("caplotonestr.ui")
            output$caplotonestr.ui <- renderUI({
              plotOutput("caplotonestr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotonestr <- renderPlot({
              plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var,input$cat_visual_choice)
            })
            
            
          }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            show("caplotnostr.ui")
            output$caplotnostr.ui <- renderUI({
              plotOutput("caplotnostr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotnostr <- renderPlot({
              plotBothCategoricalNoStr(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotmosaic")
            # output$caplotmosaic <- renderPlot({
            #   plotBothCategoricalMosaic(Variable_1,Variable_2)
            # })
            
            show("caplotcount.ui")
            output$caplotcount.ui <- renderUI({
              plotOutput("caplotcount", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcount <- renderPlot({
              plotBothCategoricalCount(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            show("caplotcountonestr.ui")
            output$caplotcountonestr.ui <- renderUI({
              plotOutput("caplotcountonestr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcountonestr <- renderPlot({
              plotBothCategoricalCountOneStr(Variable_1,Variable_2,str_var,input$cat_visual_choice)
            })
            
            # show("caplotjitter")
            # output$caplotjitter <- renderPlot({
            #   plotBothCategoricalJitter(Variable_1,Variable_2)
            # })
            
            show("caplotonestr.ui")
            output$caplotonestr.ui <- renderUI({
              plotOutput("caplotonestr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotonestr <- renderPlot({
              plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var,input$cat_visual_choice)
            })
            
          }else{
            
            str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            show("caplotnostr.ui")
            output$caplotnostr.ui <- renderUI({
              plotOutput("caplotnostr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotnostr <- renderPlot({
              plotBothCategoricalNoStr(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            # show("caplotmosaic")
            # output$caplotmosaic <- renderPlot({
            #   plotBothCategoricalMosaic(Variable_1,Variable_2)
            # })
            
            show("caplotcount.ui")
            output$caplotcount.ui <- renderUI({
              plotOutput("caplotcount", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcount <- renderPlot({
              plotBothCategoricalCount(Variable_1,Variable_2,input$cat_visual_choice)
            })
            
            show("caplotcountonestr.ui")
            output$caplotcountonestr.ui <- renderUI({
              plotOutput("caplotcountonestr", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcountonestr <- renderPlot({
              plotBothCategoricalCountOneStr(Variable_1,Variable_2,str_var_1,input$cat_visual_choice)
            })
            
            show("caplotcountonestr2.ui")
            output$caplotcountonestr2.ui <- renderUI({
              plotOutput("caplotcountonestr2", height = input$plot_height,width = input$plot_width)
            })
            output$caplotcountonestr2 <- renderPlot({
              plotBothCategoricalCountOneStr(Variable_1,Variable_2,str_var_2,input$cat_visual_choice)
            })
            
            # show("caplotjitter")
            # output$caplotjitter <- renderPlot({
            #   plotBothCategoricalJitter(Variable_1,Variable_2)
            # })
            
            show("caplotonestr.ui")
            output$caplotonestr.ui <- renderUI({
              plotOutput("caplotonestr", height = input$plot_height, width = input$plot_width)
            })
            output$caplotonestr <- renderPlot({
              plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var_1,input$cat_visual_choice)
            })
            
            show("caplotonestr2.ui")
            output$caplotonestr2.ui <- renderUI({
              plotOutput("caplotonestr2", height = input$plot_height, width = input$plot_width)
            })
            output$caplotonestr2 <- renderPlot({
              plotBothCategoricalOneStrfacet(Variable_1,Variable_2,str_var_2,input$cat_visual_choice)
            })
            
            # show("caplottwostr")
            # output$caplottwostr <- renderPlot({
            #   plotBothCategoricalTwoStrfacet(Variable_1,Variable_2,str_var_1,str_var_2)
            # })
            
          }
          
          
        }else{
          
          hide("showDots")
          hide("xScale")
          hide("yScale")
          hide("cI")
          hide("cat_visual_choice")
          show("plot_height")
          show("plot_width")
          #if else block to check how many stratifications are selected and then call appropriate functions to generate plots
          if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            # show("ccaplotnostrcol")
            # output$ccaplotnostrcol <- renderPlot({
            #   plotConCategoricalNoStrCol(Variable_1,Variable_2)
            # })
            
            
            show("ccaplotnostrbox")
            output$ccaplotnostrbox.ui <- renderUI({
              plotOutput("ccaplotnostrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotnostrbox <- renderPlot({
              plotConCategoricalNoStrBoxAndViolin(Variable_1,Variable_2)
            })
            
            
            # show("ccaplotnostrviolin")
            # output$ccaplotnostrviolin <- renderPlot({
            #   plotConCategoricalNoStrViolin(Variable_1,Variable_2)
            # })
            
            # show("ccaplotnostrdot")
            # output$ccaplotnostrdot <- renderPlot({
            #   plotConCategoricalNoStrDot(Variable_1,Variable_2)
            # })
            
          }else if(input$stratification_variable_1 != "Select an option" && input$stratification_variable_2 == "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            
            # show("ccaplotnostrcol")
            # output$ccaplotnostrcol <- renderPlot({
            #   plotConCategoricalNoStrCol(Variable_1,Variable_2)
            # })
            
            
            show("ccaplotnostrbox.ui")
            output$ccaplotnostrbox.ui <- renderUI({
              plotOutput("ccaplotnostrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotnostrbox <- renderPlot({
              plotConCategoricalNoStrBoxAndViolin(Variable_1,Variable_2)
            })
            
            
            # show("ccaplotnostrviolin")
            # output$ccaplotnostrviolin <- renderPlot({
            #   plotConCategoricalNoStrViolin(Variable_1,Variable_2)
            # })
            
            # show("ccaplotnostrdot")
            # output$ccaplotnostrdot <- renderPlot({
            #   plotConCategoricalNoStrDot(Variable_1,Variable_2)
            # })
            
            show("ccaplotonestrbox.ui")
            output$ccaplotonestrbox.ui <- renderUI({
              plotOutput("ccaplotonestrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotonestrbox <- renderPlot({
              plotConCategoricalOneStrBoxAndViolin(Variable_1,Variable_2,str_var)
            })
            
          }else if(input$stratification_variable_1 == "Select an option" && input$stratification_variable_2 != "Select an option"){
            
            str_var <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            # show("ccaplotnostrcol")
            # output$ccaplotnostrcol <- renderPlot({
            #   plotConCategoricalNoStrCol(Variable_1,Variable_2)
            # })
            
            
            show("ccaplotnostrbox.ui")
            output$ccaplotnostrbox.ui <- renderUI({
              plotOutput("ccaplotnostrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotnostrbox <- renderPlot({
              plotConCategoricalNoStrBoxAndViolin(Variable_1,Variable_2)
            })
            
            
            # show("ccaplotnostrviolin")
            # output$ccaplotnostrviolin <- renderPlot({
            #   plotConCategoricalNoStrViolin(Variable_1,Variable_2)
            # })
            
            # show("ccaplotnostrdot")
            # output$ccaplotnostrdot <- renderPlot({
            #   plotConCategoricalNoStrDot(Variable_1,Variable_2)
            # })
            
            show("ccaplotonestrbox.ui")
            output$ccaplotonestrbox.ui <- renderUI({
              plotOutput("ccaplotonestrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotonestrbox <- renderPlot({
              plotConCategoricalOneStrBoxAndViolin(Variable_1,Variable_2,str_var)
            })
            
          }else{
            
            str_var_1 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_1)]]
            str_var_2 <- variable_info$variables[variable_info$varShow == categoricalVariables[as.numeric(input$stratification_variable_2)]]
            
            # show("ccaplotnostrcol")
            # output$ccaplotnostrcol <- renderPlot({
            #   plotConCategoricalNoStrCol(Variable_1,Variable_2)
            # })
            
            
            show("ccaplotnostrbox.ui")
            output$ccaplotnostrbox.ui <- renderUI({
              plotOutput("ccaplotnostrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotnostrbox <- renderPlot({
              plotConCategoricalNoStrBoxAndViolin(Variable_1,Variable_2)
            })
            
            
            # show("ccaplotnostrviolin")
            # output$ccaplotnostrviolin <- renderPlot({
            #   plotConCategoricalNoStrViolin(Variable_1,Variable_2)
            # })
            
            # show("ccaplotnostrdot")
            # output$ccaplotnostrdot <- renderPlot({
            #   plotConCategoricalNoStrDot(Variable_1,Variable_2)
            # })
            
            show("ccaplotonestrbox.ui")
            output$ccaplotonestrbox.ui <- renderUI({
              plotOutput("ccaplotonestrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotonestrbox <- renderPlot({
              plotConCategoricalOneStrBoxAndViolin(Variable_1,Variable_2,str_var_1)
            })
            
            show("ccaplotonestrbox2.ui")
            output$ccaplotonestrbox2.ui <- renderUI({
              plotOutput("ccaplotonestrbox2", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplotonestrbox2 <- renderPlot({
              plotConCategoricalOneStrBoxAndViolin(Variable_1,Variable_2,str_var_2)
            })
            
            show("ccaplottwostrbox.ui")
            output$ccaplottwostrbox.ui <- renderUI({
              plotOutput("ccaplottwostrbox", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplottwostrbox <- renderPlot({
              plotConCategoricalTwoStrBoxAndViolin(Variable_1,Variable_2,str_var_1,str_var_2)
            })
            
            
            
            show("ccaplottwostrbox2.ui")
            output$ccaplottwostrbox2.ui <- renderUI({
              plotOutput("ccaplottwostrbox2", height = input$plot_height, width = input$plot_width)
            })
            output$ccaplottwostrbox2 <- renderPlot({
              plotConCategoricalTwoStrBoxAndViolinAlt(Variable_1,Variable_2,str_var_1,str_var_2)
            })
            
          }
          
        }
        
      }
      
    }
    
    
    
    
    
  })
  
})

