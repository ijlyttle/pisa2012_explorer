
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
  fluidRow(

    column(
      width = 3,  
      plotOutput("gg_count")     
    ),
    
    column(
      width = 3,  
      plotOutput("gg_score")     
    ),
    
    column(
      width = 4,
      fluidRow(
        column(
          width = 6, 
          selectInput(
            "subject",
            label = "Subject", 
            choices = var_names_subject
          )
        ),
        column(
          width = 6,
          selectInput(
            "country",
            label = "Countries",
            choices = levels(student2012$CNT),
            selected = c("United States of America", "Peru", "China-Shanghai"),
            multiple = TRUE
          )
        )
        
      ),
      fluidRow(
        column(
          width = 6, 
          selectInput(
            "factor_outer",
            label = "Outer factor",
            choices = var_names_factor,
            selected = "ST28Q01" # books at home
          )
        ),
        column(
          width = 6,
          selectInput(
            "factor_inner",
            label = "Inner factor",
            choices = var_names_factor,
            selected = "ST27Q02" # televisions
          )
        )
      )
        
    )

  )
  
))
