
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

input_subject <-
  selectInput(
    "subject",
    label = "Subject", 
    choices = var_names_subject
  )

input_group <- 
  selectInput(
    "group",
    label = "Group countries by",
    choices = c(`Inner factor` = "factor_inner", 
                `Outer factor` = "factor_outer")
  )

input_factor_inner <-
  selectInput(
    "factor_inner",
    label = "Inner factor",
    choices = var_names_factor,
    selected = "ST27Q02" # televisions
  )  

input_factor_outer <-
  selectInput(
    "factor_outer",
    label = "Outer factor",
    choices = var_names_factor,
    selected = "ST28Q01" # books at home
  )

shinyUI(fluidPage(
  
  fluidRow(

    column(
      width = 3,
      input_subject,
      input_factor_inner,
      input_factor_outer,
      input_group,
      plotOutput("gg_map")
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
