The dataset used by this app is taken from the `student2012.rda` file found at the contest [website](http://beta.icm.edu.pl/PISAcontest/data/). This app does not access the full dataset. Instead, approximately 25 of the factors concerning student possessions are kept (as well as gender).   

Each of the factors is turned into an ordered factor. When a correlation is made, the linear term of the (`lm`) model is examined for sign and p-value. For the purposes of grouping, those correlations with p-values greater than 0.05 are deemed not significant.

The factor **How many - televisions (mod)** is a modified version of the factor **How many - televisions**, the change being that the categories for "Zero" and "One" are combined into "One or less". 