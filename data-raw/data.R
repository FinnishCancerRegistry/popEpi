

library("data.table")
library("devtools")

# Finnish meanpop ---------------------------------------------------------

source("data-raw/meanpop_fi.R", encoding = "UTF-8")
meanpop_fi <- fread("data-raw/meanpop_fi.csv")

devtools::use_data(meanpop_fi, internal = FALSE, overwrite = TRUE)

