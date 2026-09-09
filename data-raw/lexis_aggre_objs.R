lexis_aggre_expr_table__ <- data.table::fread(
  "data-raw/lexis_aggre_expr_table__.csv",
  encoding = "UTF-8"
)
iw_info <- " The `iw` in the expression stands for the individual weight. "
lexis_aggre_expr_table__ <- rbind(
  data.table::copy(lexis_aggre_expr_table__)[
    j = "info" := paste0(.SD[["info"]], iw_info)
  ],
  data.table::copy(lexis_aggre_expr_table__)[
    j = "name" := sub("_bw", "", .SD[["name"]])
  ][
    j = "expr" := sub("\\s*[*]\\s*iw", "", .SD[["expr"]])
  ][]
)
data.table::setkeyv(lexis_aggre_expr_table__, names(lexis_aggre_expr_table__))
lexis_aggre_expr_list__ <- lapply(
  lexis_aggre_expr_table__[["expr"]],
  function(expr_string) {
    expr_string <- gsub("\\r", "", expr_string)
    parse(text = expr_string)[[1]]
  }
)
names(lexis_aggre_expr_list__) <- lexis_aggre_expr_table__[["name"]]
