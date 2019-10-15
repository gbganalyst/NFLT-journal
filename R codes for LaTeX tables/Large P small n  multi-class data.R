# R codes for generating tables in LaTeX

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("R codes for LaTeX tables/Packages.R") # Any error? Check `Packages.R` script for instructions.

# Table from Excel for multi class data

mean_rank_multi=read_excel('Table of rank of models/Table of ranks of large P small n Multi class .xlsx', sheet = 1, range = 'T5:AI11')

# LaTex table

kable(mean_rank_multi, booktabs = TRUE, caption = "Rank of the mean score of method M on binary data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(5, bold = TRUE) %>%
  row_spec(6, bold = TRUE)

# Table from Excel for multi class data

median_rank_multiclass <- read_excel('Table of rank of models/Table of ranks of large P small n Multi class .xlsx', sheet = 2, range = 'T5:AI11')

# LaTex table

kable(median_rank_multiclass, booktabs = TRUE, caption = "Rank of the median score of method M on multi class data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(5, bold = TRUE) %>%
  row_spec(6, bold = TRUE)

# Table from Excel for multi class data

sd_rank_multi <- read_excel('Table of rank of models/Table of ranks of large P small n Multi class .xlsx', sheet = 3, range = 'T5:AI11')


# LaTex table

kable(sd_rank_multi, booktabs = TRUE, caption = "Rank of the SD score of method M on multiclass data S") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position", "repeat_header")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(5, bold = TRUE) %>%
  row_spec(6, bold = TRUE)
