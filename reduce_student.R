library(dplyr)

load(file.path("data", "student2012.rda"))
load(file.path("data", "student2012dict.rda"))

df_dict <- data.frame(
  name = names(student2012dict),
  desc = student2012dict,
  row.names = NULL
)

student2012 <-
  student2012 %>%
  select(
    CNT,            # country
    ST04Q01,        # gender
    seq(44, 60),    # binary possessions
    seq(61, 66) ,   # ordered possessions
    seq(501, 505),  # math scores
    seq(541, 550)   # reading & science scores
  )

student2012dict <-
  student2012dict[names(student2012dict) %in% names(student2012)]

save(student2012, file = file.path("data", "student2012_reduced.rda"))
save(student2012dict, file = file.path("data", "student2012dict_reduced.rda"))