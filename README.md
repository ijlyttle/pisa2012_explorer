# pisa2012_explorer

this is a shiny app used to explore relationships within the PISA 2012 dataset

to get this to run, you will have to add `student2012.rda` and `student2012dict.rda` to the `data` directory. These are found at [http://beta.icm.edu.pl/PISAcontest/data/]

here is a list of R packages you will need (all from CRAN):

* `shiny`, `ggplot2`, `stringr`, `rworldmap`, `plyr`, `dplyr`, `magrittr` 

to reduce the number of variables, you will have to run (just once) the code in the file `reduce_student.R`

to run the app, from the directory of the app, and type `runApp()` at the R prompt


