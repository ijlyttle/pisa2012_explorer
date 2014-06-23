
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("PISA Student Viewer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "subject",
        label = "Subject", 
        choices = var_names_subject
      ),
      
      selectInput(
        "country",
        label = "Countries",
        choices = levels(student2012$CNT),
        selected = c("United States of America", "Peru", "China-Shanghai"),
        multiple = TRUE
      ),
      
      selectInput(
        "factor_outer",
        label = "Outer factor",
        choices = var_names_factor,
        selected = "ST28Q01" # books at home
      ),
      
      selectInput(
        "factor_inner",
        label = "Inner factor",
        choices = var_names_factor,
        selected = "ST27Q02" # televisions
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("gg_score"),
      plotOutput("gg_count")
    )
    
  )
))
