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

# we want to group by country, then look at making a linear model WRT Television
student_models <-
  student2012 %>%
  select(CNT, PV1MATH, ST27Q02) %>%
  mutate(PV1MATH = as.numeric(PV1MATH)) %>%
  group_by(CNT) %>%
  do(model = lm(PV1MATH ~ ST27Q02, data = .))

# student_results <- 
#   student_models %>%
#   summarise()

