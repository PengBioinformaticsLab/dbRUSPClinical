#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(markdown)
source("global.R")

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  
    "dbRUSPClinical",
    
    theme = shinytheme("slate"),

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
      hr(),
      plotOutput(outputId = "cplot"),
      hr(),
      plotOutput(outputId = "cplotonecolor"),
      hr(),
      plotOutput(outputId = "cplotonefacet")
      )
    
    ))
))
