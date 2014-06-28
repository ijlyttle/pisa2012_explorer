
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown) 

input_subject <-
  selectInput(
    "subject",
    label = "Subject", 
    choices = var_names_subject,
    width = "100%"
  )

input_group <- 
  selectInput(
    "group",
    label = helpText(
      "Group countries by correlation of",
      strong("score"), "and", sep = " "),
    choices = c(`Inner factor` = "factor_inner", 
                `Outer factor` = "factor_outer"),
    width = "100%"
  )

input_factor_inner <-
  selectInput(
    "factor_inner",
    label = "Inner factor",
    choices = var_names_factor,
    selected = "tv_mod", # televisions - modified
    width = "100%"
  )  

input_factor_outer <-
  selectInput(
    "factor_outer",
    label = "Outer factor",
    choices = var_names_factor,
    selected = "ST26Q03", # study place
    width = "100%"
  )

shinyUI(fluidPage(
  
  fluidRow(

    column(
      width = 3,          
      h3("PISA 2012 Explorer"),
      plotOutput("gg_map", height = "250px"),
      br(),
      tabsetPanel(
        type = "pills",
        tabPanel(
          title = tags$small("Input"),
          input_subject,
          input_factor_outer,
          input_factor_inner,
          input_group,
          br(),
          br(),
          br()
        ),
        tabPanel(
          title = tags$small("How-to"),
          includeMarkdown(file.path("markdown", "how_to.md"))
        ),
        tabPanel(
          title = tags$small("Example"),
          includeMarkdown(file.path("markdown", "example.md"))
        ),
        tabPanel(
          title = tags$small("Details"),
          includeMarkdown(file.path("markdown", "details.md"))
        ),
        tabPanel(
          title = tags$small("About"),
          includeMarkdown(file.path("markdown", "about.md"))
        )
      )
    ),
    
    column(
      width = 3,  
      plotOutput("gg_count")     
    ),
    
    column(
      width = 3,  
      plotOutput("gg_score")     
    ),
    
    column(
      width = 3,
      plotOutput("gg_group")
    )

  )
  
))
