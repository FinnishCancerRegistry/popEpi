lexis_aggre_expr_table__ <- data.table::fread(
  "data-raw/lexis_aggre_expr_table__.csv",
  encoding = "UTF-8"
)
lexis_aggre_expr_list__ <- lapply(
  lexis_aggre_expr_table__[["expr"]],
  function(expr_string) {
    expr_string <- gsub("\\r", "", expr_string)
    parse(text = expr_string)[[1]]
  }
)
names(lexis_aggre_expr_list__) <- lexis_aggre_expr_table__[["name"]]
