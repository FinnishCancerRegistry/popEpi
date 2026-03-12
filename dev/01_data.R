stopifnot(
  length(system2("git", "diff", stdout = TRUE, stderr = TRUE)) == 0
)

# I don't think this needs to be updated every time.
# e <- new.env()
# source("data-raw/meanpop_fi.R", encoding = "UTF-8", local = e)
# meanpop_fi <- e$mp
# usethis::use_data(meanpop_fi, internal = FALSE, overwrite = TRUE)

e <- new.env()
source("data-raw/surv_estimate_objs.R", encoding = "UTF-8", local = e)
surv_estimate_expr_table__ <- e$surv_estimate_expr_table__
surv_estimate_expr_list__ <- e$surv_estimate_expr_list__

e <- new.env()
source("data-raw/lexis_split_column_objs.R", encoding = "UTF-8", local = e)
lexis_split_column_expr_table__ <- e$lexis_split_column_expr_table__
lexis_split_column_expr_list__ <- e$lexis_split_column_expr_list__

e <- new.env()
source("data-raw/lexis_aggre_objs.R", encoding = "UTF-8", local = e)
lexis_aggre_expr_table__ <- e$lexis_aggre_expr_table__
lexis_aggre_expr_list__ <- e$lexis_aggre_expr_list__

usethis::use_data(
  surv_estimate_expr_table__,
  surv_estimate_expr_list__,
  lexis_split_column_expr_table__,
  lexis_split_column_expr_list__,
  lexis_aggre_expr_table__,
  lexis_aggre_expr_list__,
  internal = TRUE, overwrite = TRUE
)
system2("git", c("add", "--all"))
system2("git", c("commit", "-m", "\"feat: run dev/01_data.r\""))
