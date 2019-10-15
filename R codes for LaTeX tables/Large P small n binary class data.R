# R codes for generating tables in LaTeX

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("R codes for LaTeX tables/Packages.R") # Any error? Check `Packages.R` script for instructions.

# Table from Excel for binary class data

mean_rank_binary <- read_excel("Table of rank of models/Table of ranks of large P small n binary class.xlsx", sheet = 1, range = "T5:AI12")


# LaTex table

kable(mean_rank_binary, booktabs = TRUE, caption = "Rank of the mean score of method M on binary data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(6, bold = TRUE) %>%
  row_spec(7, bold = TRUE)

# Table from Excel for multi class data

median_rank_binary <- read_excel("Table of rank of models/Table of ranks of large P small n binary class.xlsx", sheet = 2, range = "T5:AI12")


# LaTex table

kable(median_rank_binary, booktabs = TRUE, caption = "Rank of the mean score of method M on multi class data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(6, bold = TRUE) %>%
  row_spec(7, bold = TRUE)

# Table from Excel for multi class data

sd_rank_binary <- read_excel("Table of rank of models/Table of ranks of large P small n binary class.xlsx", sheet = 3, range = "T5:AI12")


# LaTex table

kable(sd_rank_binary, booktabs=TRUE, caption = 'Rank of the mean score of method M on regression data S') %>% kable_styling(latex_options = c('striped', 'scale_down', 'HOLD_position')) %>% 
  row_spec(0, bold = TRUE) %>%
  row_spec(6, bold = TRUE) %>% 
  row_spec(7, bold = TRUE)
