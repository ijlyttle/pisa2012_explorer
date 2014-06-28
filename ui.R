
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
      hr(),
      tabsetPanel(
        tabPanel(
          title = "Input",
          input_subject,
          input_factor_outer,
          input_factor_inner,
          input_group
        ),
        tabPanel(
          title = "Information",
          p("This app was built as part of an entry to the",
            a("@useR! 2014 Data Visualization Contest,",
              href = "http://www.oecd.org/pisa/pisaproducts/datavisualizationcontest.htm"),
            "based on OECD's Programme for International Student Assessment (PISA) 2012."),
          p("In exploring the data, you have a choice among different estimates of",
            "student proficiency in the", strong("subjects"), "of math, reading, or science."),
          p("You may also choose to examine the correlation of proficiency with a",
            "variety of factors concerning the student's home, focusing on posessions.",
            "Two such factors are chosen: an", strong("outer factor,"), 
            "shown using the white violin-plots, and an",
            strong("inner factor,"), "shown using the blue-colored box-plots."),
          p("One of these factors is chosen for the", strong("correlation"),
            "used to group the countries."),
          p("discuss technique for modeling, significance, also modifying the TV question")
        ),
        tabPanel(
          title = "Example",
          includeMarkdown(file.path("example.md"))
        ),
        tabPanel(
          title = "About",
          p("This web application was created in", a("shiny,", href = "http://shiny.rstudio.com"), 
            "an open-source framework to bring interactivity to analyses built using",
            a("R.", href = "http://www.r-project.org/")),
          p("The authors are:"),
          tags$ul(
            tags$li("Ian Lyttle, Schneider Electric"),
            tags$li("Alex Shum, Iowa State University and Schneider Electric"),
            tags$li("Dianne Cook, Iowa State University"),
            tags$li("Luke Fostvedt, Iowa State University") 
          ),
          p("Source code is available at", 
            a("GitHub.", href = "https://github.com/ijlyttle/pisa_explorer")),
          p("This work is licensed under a",
            a("Creative Commons Attribution-ShareAlike 4.0 International License.", 
              href = "http://creativecommons.org/licenses/by-sa/4.0/"))
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
