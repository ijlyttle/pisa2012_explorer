
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(stringr)
library(rworldmap)

# taken from Di's work
extractPolygons <- function(shapes) {
 
  library(plyr)
  
  dframe <- ldply(1:length(shapes@polygons), function(i) {
    ob <- shapes@polygons[[i]]@Polygons
    dframe <- ldply(1:length(ob), function(j) {
      x <- ob[[j]]
      co <- x@coords
      data.frame(co, order=1:nrow(co), group=j)
    })
    dframe$region <- i
    dframe$name <- shapes@polygons[[i]]@ID
    dframe$area <- shapes@polygons[[i]]@area
    dframe
  })
  # construct a group variable from both group and polygon:
  dframe$group <- interaction(dframe$region, dframe$group)
  
  detach("package:plyr")
  
  dframe
}

world <- getMap(resolution = "low")
world.polys <- extractPolygons(world)

# To get a blank background on map
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

# params
plot_height <- 825

# given the value of a vector, get its name
val_get_name <- function(x, lst){
  names(lst[lst == x])[[1]]
}

get_linear_coef <- function(model){
  coef(summary(model))[2, 1]
}

get_pval <- function(model){
  coef(summary(model))[2, 4]
}

get_cluster <- function(linear_coef, pval, sig){
  
  cluster <- 
    ifelse (
      pval > sig,
      "not significant",
      ifelse (
        linear_coef > 0,
        "positive",
        "negative"
      )
    ) 
  
  cluster
}

shinyServer(function(input, output) {
  
  rct_data_new <- reactive({
    
    df <-
      student2012 %>%
      select(
        country = CNT,
        score = get(input$subject),
        factor_outer = get(input$factor_outer),
        factor_inner = get(input$factor_inner)
      ) %>%
      mutate(
        score = as.numeric(score),
        factor_outer = ordered(factor_outer),
        factor_inner = ordered(factor_inner),        
      )  
    
    df
    
  })
  
  rct_countries <- reactive({
    
    str_formula <- str_join("score ~", input$group, sep = " ")
    
    # create groups
    student_model <- 
      rct_data_new()  %>%
      group_by(country) %>%
      do(model = lm(str_formula, data = .)) %>%
      mutate(
        linear_coef = get_linear_coef(model),
        pval = get_pval(model),
        group_corr = ordered(
          get_cluster(linear_coef, pval, 0.05), 
          levels = c("negative",
                     "not significant",
                     "positive")
        )
      ) %>%
      select(-model)
    
    # determine median score, join groups
    student_summary <- 
      rct_data_new() %>%
      group_by(country) %>%
      summarize(median = median(score), count = n()) %>%
      left_join(student_model, by = "country")
    
    # reorder the countries by median score, for display
    student_summary <-
      student_summary %>%
      mutate(country = reorder(country, median))  
    
    # return student_summary
    student_summary
    
  })
  
  rct_data_group <- reactive({
    
    df_group <- rct_countries() %>% select(country, group_corr)
    
    df <- 
      rct_data_new() %>%
      left_join(df_group, by = "country")
    
  })
  
  # change the labels according to the variable selections
  rct_labels <- reactive({
    list(
      outer = input$factor_outer %>% val_get_name(var_names_factor),
      score = input$subject %>% val_get_name(var_names_subject),
      inner = input$factor_inner %>% val_get_name(var_names_factor)
    )
  })
  

  
  observe({
    print(summary(rct_data_map()))
  })
  
  # plot showing the counts
  output$gg_count <- renderPlot({
    ggplot(data = rct_data_group()) +
      geom_rect(
        aes(fill = group_corr),
        xmin = -Inf, xmax = Inf, 
        ymin = -Inf, ymax = Inf,
        alpha = 0.5,
        data = data.frame(group_corr = unique(rct_data_group()$group_corr))
      ) + 
      geom_bar(
        aes(x = factor_outer, group = factor_inner),
        position = "dodge", 
        fill = "white", 
        alpha = 1,
        color = "black"
      ) +   
      geom_bar(
        aes(x = factor_outer, alpha = factor_inner),
        position = "dodge", 
        fill = "blue"
      ) +   
      facet_grid(group_corr ~ ., scales = "free_y") + 
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
  }, height = plot_height)
  
  # plot showing the scores
  output$gg_score <- renderPlot({
    ggplot(data = rct_data_group()) + 
      geom_rect(
        aes(fill = group_corr),
        xmin = -Inf, xmax = Inf, 
        ymin = -Inf, ymax = Inf,
        alpha = 0.5,
        data = data.frame(group_corr = unique(rct_data_group()$group_corr))
      ) + 
      geom_violin(
        aes(x = factor_outer, y = score),
        scale = "width"
      ) + 
      geom_boxplot(
        aes(x = factor_outer, y = score, alpha = factor_inner),
        fill = "blue",
        outlier.shape = NA
      ) + 
      facet_grid(group_corr ~ .) + 
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
  }, height = plot_height)
  
  # plot showing country groups
  output$gg_group <- renderPlot({
    
    ggplot() +
      geom_rect(
        aes(fill = group_corr),
        xmin = -Inf, xmax = Inf, 
        ymin = -Inf, ymax = Inf,
        alpha = 0.5,
        data = data.frame(group_corr = unique(rct_countries()$group_corr))
      ) + 
      geom_point(      
        aes(x = median, y = country, size = count),
        alpha = 0.75,
        data = rct_countries()
      ) +
      facet_grid(group_corr ~ ., scales = "free", space = "free") +
      scale_size_area(guide = guide_legend(title.position = "top")) +
      scale_fill_brewer(
        type = "seq", 
        palette = "BuGn", 
        guide = guide_legend(
          title = "Correlation with factor",
          title.position = "top"
        )
      ) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )   
  }, height = plot_height)
  
  rct_data_map <- reactive({
    
    map_data <- 
      rct_data_group() %>%
      mutate(name = country) %>%
      select(name, group_corr) %>%
      left_join(world.polys, by = "name")
    
  })
  
  output$gg_map <- reactive({
    
#    ggplot(data=world.polys) + 
#      geom_path(aes(x=X1, y=X2, order=order, group=group), colour=I("grey70")) + 
#      geom_polygon(data=rct_data_map(), aes(x=X1, y=X2, order=order, group=group, fill=group_corr)) +
#      new_theme_empty + 
#      theme(legend.position="none")
    
  })
  
})
