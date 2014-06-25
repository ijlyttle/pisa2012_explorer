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

# new variable for presence of TV at home
student2012$tv_at_home <- 
  ifelse(
    student2012$ST27Q02 == "None",
    "No",
    "Yes"
  )
student2012dict["tv_at_home"] <- "At Home - Television"

# adjust names for mapping purpose, and others
fn_sub_name <- function(names, name_old, name_new){
  names[names == name_old] <- name_new
  
  names
} 

country_names <- as.character(student2012$CNT)

country_names <- fn_sub_name(country_names, "Serbia", "Republic of Serbia")
country_names <- fn_sub_name(country_names, "Korea", "South Korea")
country_names <- fn_sub_name(country_names, "Chinese Taipei", "Taiwan")
country_names <- fn_sub_name(country_names, "Slovak Republic", "Slovakia")
country_names <- fn_sub_name(country_names, "Russian Federation", "Russia")
country_names <- fn_sub_name(country_names, "Perm(Russian Federation)", "Russia")
country_names <- fn_sub_name(country_names, "Hong Kong-China", "Hong Kong S.A.R.")
country_names <- fn_sub_name(country_names, "China-Shanghai", "China")
country_names <- fn_sub_name(country_names, "China-Macau", "China")
country_names <- fn_sub_name(country_names, "Connecticut (USA)", "United States of America")
country_names <- fn_sub_name(country_names, "Florida (USA)", "United States of America")
country_names <- fn_sub_name(country_names, "Massachusetts (USA)", "United States of America")

country_names <- as.factor(country_names)

student2012$CNT <- country_names

# hacky - fix when time
student2012$ST01Q01 <- fn_make_factor(student2012$ST01Q01)
student2012$ST02Q01 <- fn_make_factor(student2012$ST02Q01)
student2012$ST06Q01 <- fn_make_factor(student2012$ST06Q01)
student2012$ST21Q01 <- fn_make_factor(student2012$ST21Q01)
student2012$ST115Q01 <- fn_make_factor(student2012$ST115Q01)
student2012$tv_at_home <- fn_make_factor(student2012$tv_at_home)

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





