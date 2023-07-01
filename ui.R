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
source("global.R")

# Define UI for application
shinyUI(
  
  navbarPage(
  
    #Introduction about the application
    "dbRUSPClinical",
    
    theme = shinytheme("slate"),
    
    useShinyjs(),

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
          selected = 3
        )
      ),
      tags$div(
        title = "Select second variable",
        selectInput(
          "variable_2",
          label = h4("Variable 2"),
          choices = c("Select an option",makeList(variable_info$varShow)),
          multiple = FALSE,
          selected = 5
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
      checkboxInput(inputId = "showDots", label = "Show Dots"),
      hr(),
      checkboxInput(inputId = "xScale", label = "Log scale for X axis"),
      hr(),
      checkboxInput(inputId = "yScale", label = "Log scale for y axis"),
      hr(),
      actionButton("submitchoice","Submit")
    ),
    
    #Main panel to display all the messages and plots.
    mainPanel(
      uiOutput(outputId = "tvmessage"),
      plotOutput(outputId = "cplot"),
      br(),
      plotOutput(outputId = "cplotonecolor"),
      br(),
      plotOutput(outputId = "cplotonefacet"),
      br(),
      plotOutput(outputId = "cplotonecolor2"),
      br(),
      plotOutput(outputId = "cplotonefacet2"),
      br(),
      plotOutput(outputId = "cplottwostr"),
      br(),
      plotOutput(outputId = "cplottwostralt"),
      br(),
      plotOutput(outputId = "cplottwostrfacet"),
      br(),
      plotOutput(outputId = "caplotnostr"),
      br(),
      plotOutput(outputId = "caplotcount"),
      br(),
      plotOutput(outputId = "caplotjitter"),
      br(),
      plotOutput(outputId = "caplotmosaic"),
      br(),
      plotOutput(outputId = "caplotonestr"),
      br(),
      plotOutput(outputId = "caplotonestr2"),
      br(),
      plotOutput(outputId = "caplottwostr"),
      br(),
      plotOutput(outputId = "ccaplotnostrcol"),
      br(),
      plotOutput(outputId = "ccaplotnostrbox"),
      br(),
      plotOutput(outputId = "ccaplotnostrviolin"),
      br(),
      plotOutput(outputId = "ccaplotnostrdot"),
      br(),
      plotOutput(outputId = "ccaplotonestrbox"),
      br(),
      plotOutput(outputId = "ccaplotonestrbox2"),
      br(),
      plotOutput(outputId = "ccaplottwostrbox")
      )
    
    ))
))
