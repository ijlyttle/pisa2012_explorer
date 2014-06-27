library(dplyr)
library(magrittr)
library(stringr)
library(biglm)

# the thought is that we should have only those things that are of interest to 
# both ui.R and server.R

# we are going to read only the student tables
load(file.path("data", "student2012_reduced.rda"))
load(file.path("data", "student2012dict_reduced.rda"))

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

# switch order in named vector for dictionary
var_names <- names(student2012dict)
names(var_names) <- 
  student2012dict %>%
  str_replace_all("\x92", "'") # gets rid of funny characters"

var_names_subject <- 
  var_names[str_detect(var_names, "PV")]

var_type <- 
  lapply(student2012 %>% select(-CNT), class) 

var_factor <- 
  var_type[var_type == "factor"] %>%
  names 

var_names_factor <- var_names[var_names %in% var_factor] 





