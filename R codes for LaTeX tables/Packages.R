# R Packages ----------------------------------------

# This program will download from the internet and install the latest version of the packages below if they are not already installed in your R environment. It is necessary to have internet connection to download those packages.


# If for any reason this program fails to run, please make sure that the packages are installed.

if (!require("xfun")) {
  install.packages("xfun")
}

# loading packages

xfun::pkg_attach(c("knitr", "kableExtra", "readxl"), install = TRUE)

# Define global option for LaTex using:
options(knitr.table.format = "latex")
