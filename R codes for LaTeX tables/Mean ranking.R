# R codes for generating tables in LaTeX

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("R codes for LaTeX tables/Packages.R") # Any error? Check `Packages.R` script for instructions.


# Table from Excel for binary class data

mean_rank_binary <- read_excel("Table of rank of models/Table of mean ranks of learning methods.xlsx", sheet = 1, range = "T6:AI19")

# LaTex table

kable(mean_rank_binary, booktabs = TRUE, caption = "Rank of the mean score of method M on binary data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(12, bold = TRUE) %>%
  row_spec(13, bold = TRUE)

# Table from Excel for multi class data

mean_rank_multiclass <- read_excel("Table of rank of models/Table of mean ranks of learning methods.xlsx", sheet = 2, range = "T6:AI17")

# LaTex table

kable(mean_rank_multiclass, booktabs = TRUE, caption = "Rank of the mean score of method M on multi class data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(10, bold = TRUE) %>%
  row_spec(11, bold = TRUE)

# Table from Excel for regression data

mean_rank_regression <- read_excel('Table of rank of models/Table of mean ranks of learning methods.xlsx', sheet = 3, range = 'T6:AI30')


# LaTex table

kable(mean_rank_regression, booktabs = TRUE, caption = "Rank of the mean score of method M on regression data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position", "repeat_header")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(23, bold = TRUE) %>%
  row_spec(24, bold = TRUE)
