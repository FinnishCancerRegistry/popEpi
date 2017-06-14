
survmean_test_data_01 <- fread("data-raw/pkg_survival_to_115_yrs.csv")

devtools::use_data(survmean_test_data_01, internal = TRUE, overwrite = TRUE)
