#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load the required packages and scripts
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(markdown)
#source("global.R")

# Define UI for application
fluidPage(
  
  tagList(
    
    useShinyjs(),
    
    navbarPage(
      
      
      
      #Introduction about the application
      "dbRUSPClinical",
      
      #Theme for the application
      theme = shinytheme("slate"),
      
      ############ about ###########
      tabPanel(
        "About",
        includeMarkdown("content/about.md")
      ),
      
      #The page to generate plots between two variables
      ############ Correlation ###########
      tabPanel(
        "Correlation",
        id = "correlation",
        value = "correlation_choice",
        sidebarLayout(
          
          #sidebar panel allows you to select the variables for correlation
          sidebarPanel(
            h2("Select parameters for correlation"),
            hr(),
            tags$div(
              title = "Select first variable",
              selectInput(
                "variable_1",
                label = h4("Variable 1"),
                choices = c("Select an option",makeList(variable_info$varShow)),
                multiple = FALSE,
                selected = 5
              )
            ),
            tags$div(
              title = "Select second variable",
              selectInput(
                "variable_2",
                label = h4("Variable 2"),
                choices = c("Select an option",makeList(variable_info$varShow)),
                multiple = FALSE,
                selected = 3
              )
            ),
            tags$div(
              title = "Select first stratification variable",
              selectInput(
                "stratification_variable_1",
                label = h4("Stratification Variable 1"),
                choices = c("Select an option",makeList(categoricalVariables)),
                multiple = FALSE
              )
            ),
            tags$div(
              title = "Select second stratification variable",
              selectInput(
                "stratification_variable_2",
                label = h4("Stratification Variable 2"),
                choices = c("Select an option",makeList(categoricalVariables)),
                multiple = FALSE
              )
            ),
            hr(),
            radioButtons(
              inputId = "cat_visual_choice",
              label = "Select one visualization:",
              choices = c("Sample size", "Sample proportion"),
              selected = "Sample size"
            ),
            hr(),
            checkboxInput(inputId = "showDots", label = "Show Dots"),
            hr(),
            checkboxInput(inputId = "xScale", label = "Log scale for X axis"),
            hr(),
            checkboxInput(inputId = "yScale", label = "Log scale for Y axis"),
            hr(),
            checkboxInput(inputId = "cI", label = "Confidence Interval"),
            hr(),
            actionButton("submitchoice","Submit"),
            hr(),
            sliderInput(
              "plot_height",
              "Adjust Plot Height:",
              min = 400,
              max = 1200,
              value = 400
            ),
            hr(),
            sliderInput(
              "plot_width",
              "Adjust Plot Width:",
              min = 1250,
              max = 2600,
              value = 1250
            )
          ),
          
          #Main panel to display all the messages and plots.
          mainPanel(
            
            uiOutput(outputId = "tvmessage"),
            uiOutput("cplot.ui"),
            
            conditionalPanel(
              condition = "output.cplotonecolor.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("cplotonecolor.ui"),
            
            # conditionalPanel(
            #   condition = "output.cplotonefacet !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "cplotonefacet"),
            
            conditionalPanel(
              condition = "output.cplotonecolor2.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("cplotonecolor2.ui"),
            
            # conditionalPanel(
            #   condition = "output.cplotonefacet2 !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "cplotonefacet2"),
            
            conditionalPanel(
              condition = "output.cplottwostr.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("cplottwostr.ui"),
            
            conditionalPanel(
              condition = "output.cplottwostralt.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("cplottwostralt.ui"),
            
            # conditionalPanel(
            #   condition = "output.cplottwostrfacet !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "cplottwostrfacet"),
            
            conditionalPanel(
              condition = "output.caplotnostr.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("caplotnostr.ui"),
            
            conditionalPanel(
              condition = "output.caplotcount.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("caplotcount.ui"),
            
            conditionalPanel(
              condition = "output.caplotcountonestr.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("caplotcountonestr.ui"),
            
            # conditionalPanel(
            #   condition = "output.caplotjitter !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "caplotjitter"),
            
            # conditionalPanel(
            #   condition = "output.caplotmosaic !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "caplotmosaic"),
            
            conditionalPanel(
              condition = "output.caplotonestr.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("caplotonestr.ui"),
            
            
            conditionalPanel(
              condition = "output.caplotcountonestr2.ui !== null",
              tags$div(style = "height: 20px;")
            ),
            
            uiOutput("caplotcountonestr2.ui"),
            
            
            conditionalPanel(
              condition = "output.caplotonestr2.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("caplotonestr2.ui"),
            
            
            # conditionalPanel(
            #   condition = "output.caplottwostr !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "caplottwostr"),
            
            # conditionalPanel(
            #   condition = "output.ccaplotnostrcol !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "ccaplotnostrcol"),
            
            conditionalPanel(
              condition = "output.ccaplotnostrbox.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("ccaplotnostrbox.ui"),
            
            # conditionalPanel(
            #   condition = "output.ccaplotnostrviolin !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "ccaplotnostrviolin"),
            
            # conditionalPanel(
            #   condition = "output.ccaplotnostrdot !== null",
            #   tags$div(style = "height: 20px;")  
            # ),
            # 
            # plotOutput(outputId = "ccaplotnostrdot"),
            
            conditionalPanel(
              condition = "output.ccaplotonestrbox.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("ccaplotonestrbox.ui"),
            
            conditionalPanel(
              condition = "output.ccaplotonestrbox2.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("ccaplotonestrbox2.ui"),
            
            conditionalPanel(
              condition = "output.ccaplottwostrbox.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("ccaplottwostrbox.ui"),
            
            conditionalPanel(
              condition = "output.ccaplottwostrbox2.ui !== null",
              tags$div(style = "height: 20px;")  
            ),
            
            uiOutput("ccaplottwostrbox2.ui")
          )
          
        ))
    )
    
  )
)