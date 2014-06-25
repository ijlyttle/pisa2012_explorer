
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# given the value of a vector, get its name
val_get_name <- function(x, lst){
  names(lst[lst == x])[[1]]
}

shinyServer(function(input, output) {

  # deal only with data in selected countries
  rct_data <- reactive({
    df <- 
      student2012 %>%
      filter(CNT %in% input$country) 
  })
  
  # change the labels according to the variable selections
  rct_labels <- reactive({
    list(
      outer = input$factor_outer %>% val_get_name(var_names_factor),
      score = input$subject %>% val_get_name(var_names_subject),
      inner = input$factor_inner %>% val_get_name(var_names_factor)
    )
  })
  
  rct_countries <- reactive({
    
    str_factor <-
      switch(
        input$group,
        factor_outer = input$factor_outer,
        factor_inner = input$factor_inner
      )
    
    # get the just the data we need
    student_score_factor <-
      student2012 %>%
      select(
        country = CNT, 
        score = get(input$subject), 
        fctr = get(str_factor)
      ) %>%
      mutate(
        score = as.numeric(score),
        fctr = ordered(fctr)
      )   
    
    # create groups
    student_model <- 
      student_score_factor %>%
      group_by(country) %>%
      do(model = lm(score ~ fctr, data = .)) %>%
      mutate(
        linear_coef = get_linear_coef(model),
        pval = get_pval(model),
        corr = ordered(
          get_cluster(linear_coef, pval, 0.05), 
          levels = c("significant negative",
                     "not significant",
                     "significant positive")
        )
      ) %>%
      select(-model)
    
    # determine median score, join groups
    student_summary <- 
      student_score_factor %>%
      group_by(country) %>%
      summarize(median = median(score), count = n()) %>%
      left_join(student_model, by = "country")
    
    # reorder the countries by median score, for display
    student_summary <-
      student_summary %>%
      mutate(country = reorder(country, median))  
    
    # return student_summary
    
  })
  
  # plot showing the counts
  output$gg_count <- renderPlot({
    ggplot(data = rct_data()) +
      geom_rect(
        aes(fill = CNT),
        xmin = -Inf, xmax = Inf, 
        ymin = -Inf, ymax = Inf,
        alpha = 0.5,
        data = data.frame(CNT = unique(rct_data()$CNT))
      ) + 
      geom_bar(
        aes_string(x = input$factor_outer, group = input$factor_inner),
        position = "dodge", 
        fill = "white", 
        alpha = 1,
        color = "black"
      ) +   
      geom_bar(
        aes_string(x = input$factor_outer, alpha = input$factor_inner),
        position = "dodge", 
        fill = "blue"
      ) +   
      facet_grid(CNT ~ ., scales = "free_y") + 
      scale_x_discrete(name = rct_labels()[["outer"]]) +  
      scale_y_continuous(name = "number of students") +
      scale_alpha_discrete(
        na.value = 0, 
        name = rct_labels()[["inner"]],
        guide = guide_legend(title.position = "top")) +  
      scale_fill_brewer(type = "seq", palette = "BuGn", guide = FALSE) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  }, height = 750)
  
  # plot showing the scores
  output$gg_score <- renderPlot({
    ggplot(data = rct_data()) + 
      geom_rect(
        aes(fill = CNT),
        xmin = -Inf, xmax = Inf, 
        ymin = -Inf, ymax = Inf,
        alpha = 0.5,
        data = data.frame(CNT = unique(rct_data()$CNT))
      ) + 
      geom_violin(
        aes_string(x = input$factor_outer, y = input$subject),
        scale = "width"
      ) + 
      geom_boxplot(
        aes_string(x = input$factor_outer, y = input$subject, alpha = input$factor_inner),
        fill = "blue"
      ) + 
      facet_grid(CNT ~ .) + 
      scale_x_discrete(name = rct_labels()[["outer"]]) +  
      scale_y_continuous(limits = c(0, 1000), name = rct_labels()[["score"]]) +
      scale_alpha_discrete(
        na.value = 0, 
        name = rct_labels()[["inner"]],
        guide = guide_legend(title.position = "top")) +  
      scale_fill_brewer(type = "seq", palette = "BuGn", guide = FALSE) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  }, height = 750)
  
  # plot showing country groups
  output$gg_group <- renderPlot({
    
    ggplot(
      aes(x = median, y = country, size = count),
      data = rct_countries()
    ) +
      geom_point() +
      scale_size_area(guide = guide_legend(title.position = "top")) +
      facet_grid(corr ~ ., scales = "free", space = "free") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )   
  }, height = 750)
  
})
