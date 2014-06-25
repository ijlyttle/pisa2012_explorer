library(dplyr)
library(magrittr)
library(stringr)
library(biglm)

# the thought is that we should have only those things that are of interest to 
# both ui.R and server.R

# we are going to read only the student tables
load(file.path("data", "student2012.rda"))
load(file.path("data", "student2012dict.rda"))

# let's coerce some variables into factors
fn_make_factor <- function(vec){  
  vec <- factor(vec, levels = sort(unique(vec)))
}

# hacky - fix when time
student2012$ST01Q01 <- fn_make_factor(student2012$ST01Q01)
student2012$ST02Q01 <- fn_make_factor(student2012$ST02Q01)
student2012$ST06Q01 <- fn_make_factor(student2012$ST06Q01)
student2012$ST21Q01 <- fn_make_factor(student2012$ST21Q01)
student2012$ST115Q01 <- fn_make_factor(student2012$ST115Q01)

# switch order in named vector for dictionary
var_names <- names(student2012dict)
names(var_names) <- 
  student2012dict %>%
  str_replace_all("\x92", "'") # gets rid of funny characters"

var_names_subject <- 
  var_names[c(
    seq(501, 505), # math
    seq(541, 545), # reading
    seq(546, 550)  # science
  )]

var_type <- 
  lapply(student2012, class) 

var_factor <- 
  var_type[var_type == "factor"] %>%
  names 

var_names_factor <- var_names[var_names %in% var_factor] 

# we want to rename and combine countries

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
       "significant positive",
       "significant negative"
      )
    ) 
      
  cluster
}

# we want to group by country, then look at making a linear model WRT Television
str_subject <- "PV1MATH"
str_factor_inner <- "ST27Q02"

student_score_factor <-
  student2012 %>%
  select(
    country = CNT, 
    score = get(str_subject), 
    factor_inner = get(str_factor_inner)
  ) %>%
  mutate(
    score = as.numeric(score),
    factor_inner = ordered(factor_inner)
  )   
  
student_model <- 
  student_score_factor %>%
  group_by(country) %>%
  do(model = lm(score ~ factor_inner, data = .)) %>%
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

student_summary <- 
  student_score_factor %>%
  group_by(country) %>%
  summarize(median = median(score), count = n()) %>%
  left_join(student_model, by = "country")

student_summary <-
  student_summary %>%
  mutate(country = reorder(country, median))

ggplot(
  aes(x = median, y = country),
  data = student_summary
) +
  geom_point() +
  facet_grid(corr ~ ., scales = "free")

# student_results <- 
#   student_models %>%
#   summarise()

