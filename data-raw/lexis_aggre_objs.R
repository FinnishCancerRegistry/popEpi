lexis_aggre_expr_table__ <- data.table::fread(
  "data-raw/lexis_aggre_expr_table__.csv",
  encoding = "UTF-8"
)
lexis_aggre_expr_list__ <- lapply(
  lexis_aggre_expr_table__[["expr"]],
  function(expr_string) {
    expr_lines <- strsplit(expr_string, "\r\n")
    parse(text = expr_lines)[[1]]
  }
)
names(lexis_aggre_expr_list__) <- lexis_aggre_expr_table__[["name"]]
