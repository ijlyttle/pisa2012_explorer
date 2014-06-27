library(dplyr)
library(magrittr)
library(stringr)

# the thought is that we should have only those things that are of interest to 
# both ui.R and server.R

# we are going to read only the student tables
load(file.path("data", "student2012_reduced.rda"))
load(file.path("data", "student2012dict_reduced.rda"))

# Order some factors

# ordering this is fraught with peril - we'll count X-chromosomes
student2012$ST04Q01 <- ordered(student2012$ST04Q01, levels = c("Male", "Female"))

student2012$ST26Q01 <- ordered(student2012$ST26Q01, levels = c("No", "Yes"))
student2012$ST26Q02 <- ordered(student2012$ST26Q02, levels = c("No", "Yes"))
student2012$ST26Q03 <- ordered(student2012$ST26Q03, levels = c("No", "Yes"))
student2012$ST26Q04 <- ordered(student2012$ST26Q04, levels = c("No", "Yes"))
student2012$ST26Q05 <- ordered(student2012$ST26Q05, levels = c("No", "Yes"))
student2012$ST26Q06 <- ordered(student2012$ST26Q06, levels = c("No", "Yes"))
student2012$ST26Q07 <- ordered(student2012$ST26Q07, levels = c("No", "Yes"))
student2012$ST26Q08 <- ordered(student2012$ST26Q08, levels = c("No", "Yes"))
student2012$ST26Q09 <- ordered(student2012$ST26Q09, levels = c("No", "Yes"))
student2012$ST26Q10 <- ordered(student2012$ST26Q10, levels = c("No", "Yes"))
student2012$ST26Q11 <- ordered(student2012$ST26Q11, levels = c("No", "Yes"))
student2012$ST26Q12 <- ordered(student2012$ST26Q12, levels = c("No", "Yes"))
student2012$ST26Q13 <- ordered(student2012$ST26Q13, levels = c("No", "Yes"))
student2012$ST26Q14 <- ordered(student2012$ST26Q14, levels = c("No", "Yes"))

student2012$ST27Q01 <- ordered(student2012$ST27Q01)
student2012$ST27Q02 <- ordered(student2012$ST27Q02)
student2012$ST27Q03 <- ordered(student2012$ST27Q03)
student2012$ST27Q04 <- ordered(student2012$ST27Q04)
student2012$ST27Q05 <- ordered(student2012$ST27Q05)

student2012$ST28Q01 <- ordered(student2012$ST28Q01)



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

var_is_factor <- 
  vapply(student2012, is.factor, TRUE) 

var_names_factor <- var_names[var_is_factor] 

var_names_factor <- 
  var_names_factor[!var_names_factor %in% c("CNT", "ST26Q15", "ST26Q16", "ST26Q17")]





