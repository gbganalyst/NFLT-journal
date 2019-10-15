# R codes for generating tables in LaTeX

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("R codes for LaTeX tables/Packages.R") # Any error? Check `Packages.R` script for instructions.

# Table from Excel for binary class data

binary_data <- read_excel("Appendix/Table of n and p.xlsx", sheet = 1, range = "B4:G20")

# LaTex table

kable(binary_data, booktabs = TRUE, caption = "Structure of binary datasets") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE)

# Table from Excel for multi class data

multi_data <- read_excel("Appendix/Table of n and p.xlsx", sheet = 2, range = "B4:G17")

# LaTex table

kable(multi_data, booktabs = TRUE, caption = "Structure of multi-class datasets") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE)


# Table from Excel for Regression data

regression_data <- read_excel("Appendix/Table of n and p.xlsx", sheet = 3, range = "B4:f26")

# LaTex table

kable(regression_data, booktabs = T, caption = "Structure of regression datasets") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE)
