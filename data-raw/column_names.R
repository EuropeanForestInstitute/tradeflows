# Prepare the column_names reference table for use
# within the package
column_names <- read.csv("inst/config/column_names.csv", as.is=TRUE)
devtools::use_data(column_names, overwrite = TRUE)

column_description <- read.csv("data-raw/column_description.csv", as.is=TRUE)
devtools::use_data(column_description, overwrite = TRUE)
