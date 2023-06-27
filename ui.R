#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(markdown)
source("global.R")

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(
  
    "dbRUSPClinical",
    
    theme = shinytheme("slate"),
    
    useShinyjs(),

    ############ about ###########
    tabPanel(
      "About",
      includeMarkdown("content/about.md")
    ),
    
    
    ############ Correlation ###########
    tabPanel(
      "Correlation",
      id = "correlation",
      value = "correlation_choice",
      sidebarLayout(
        sidebarPanel(
      h2("Select parameters for correlation"),
      hr(),
      tags$div(
        title = "Select first variable",
        selectInput(
          "variable_1",
          label = h4("Variable 1"),
          choices = c("Select an option",makeList(variable_info$varShow)),
          multiple = FALSE
        )
      ),
      tags$div(
        title = "Select second variable",
        selectInput(
          "variable_2",
          label = h4("Variable 2"),
          choices = c("Select an option",makeList(variable_info$varShow)),
          multiple = FALSE
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
      actionButton("submitchoice","Submit")
    ),
    
    mainPanel(
      uiOutput(outputId = "tvmessage"),
      plotOutput(outputId = "cplot"),
      plotOutput(outputId = "cplotonecolor"),
      plotOutput(outputId = "cplotonefacet"),
      plotOutput(outputId = "cplotonecolor2"),
      plotOutput(outputId = "cplotonefacet2"),
      plotOutput(outputId = "cplottwostr"),
      plotOutput(outputId = "cplottwostralt"),
      plotOutput(outputId = "cplottwostrfacet"),
      plotOutput(outputId = "caplotnostr"),
      plotOutput(outputId = "caplotcount"),
      plotOutput(outputId = "caplotjitter"),
      plotOutput(outputId = "caplotmosaic"),
      plotOutput(outputId = "caplotonestr"),
      plotOutput(outputId = "caplotonestr2"),
      plotOutput(outputId = "caplottwostr"),
      plotOutput(outputId = "ccaplotnostrcol"),
      plotOutput(outputId = "ccaplotnostrbox"),
      plotOutput(outputId = "ccaplotnostrviolin"),
      plotOutput(outputId = "ccaplotnostrdot"),
      plotOutput(outputId = "ccaplotonestrbox"),
      plotOutput(outputId = "ccaplotonestrbox2"),
      plotOutput(outputId = "ccaplottwostrbox")
      )
    
    ))
))
