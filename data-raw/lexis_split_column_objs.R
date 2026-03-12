lexis_split_column_expr_table__ <- data.table::fread(
  "data-raw/lexis_split_column_expr_table__.csv",
  encoding = "UTF-8"
)
lexis_split_column_expr_list__ <- lapply(
  lexis_split_column_expr_table__[["expr"]],
  function(expr_string) {
    parse(text = expr_string)[[1]]
  }
)
names(lexis_split_column_expr_list__) <-
  lexis_split_column_expr_table__[["name"]]
