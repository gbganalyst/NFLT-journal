
# R Packages ----------------------------------------

# This program will download from the internet and install the latest version of the packages below if they are not already installed in your R environment. It is necessary to have internet connection to download those packages.


# If for any reason this program fails to run, please make sure that the packages are installed.

data_wrangling_packages <- c("tidyverse", "readxl", "openxlsx")

table_formating <- c("knitr", "kableExtra")

machine_learning_packages <- c("caret", "MASS", "car", "kernlab", "rpart", "randomForest", "class", "ada", "rda", "e1071", "nnet", "ipred", "dbarts", "klaR", "glmnet", "earth", "Matrix", "foba", "mboost", "bst", "relaxo", "monomvn", "elasticnet", "Metrics")

if (!require(install.load)) {
  install.packages("install.load")
}

# loading packages

install.load::install_load(c(data_wrangling_packages, table_formating, machine_learning_packages))
