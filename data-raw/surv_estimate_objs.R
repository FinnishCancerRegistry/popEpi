devtools::load_all()

surv_estimate_expr_table__ <- data.table::fread(
  "data-raw/surv_estimate_expr_table__.csv",
  encoding = "UTF-8"
)
bw_obj_nms <- names(lexis_aggre_expr_list__)[grepl(
  "_bw$",
  names(lexis_aggre_expr_list__)
)]
bw_obj_nms <- c(
  bw_obj_nms,
  paste0(surv_estimate_expr_table__[["name"]], "_bw")
)
is_xy <- grepl("[x, y]", bw_obj_nms, fixed = TRUE)
# we want to replace first e.g. `h_lt_[x, y]` before `h_lt`
bw_obj_nms <- c(bw_obj_nms[is_xy], bw_obj_nms[!is_xy])
names(bw_obj_nms) <- sub("_bw$", "", bw_obj_nms)
bw_info <- paste0(
  " The suffix `_bw` implies that the estimates are based on Brenner weighted ",
  "statistics such as `t_at_risk_bw`."
)
surv_estimate_expr_table__ <- rbind(
  surv_estimate_expr_table__,
  data.table::copy(surv_estimate_expr_table__)[
    j = "name" := paste0(.SD[["name"]], "_bw")
  ][
    j = "info" := paste0(.SD[["info"]], bw_info)
  ][
    j = c("est", "se") := lapply(.SD, function(col) {
      for (re in names(bw_obj_nms)) {
        col <- gsub(
          sprintf("(?!<\\w)\\Q%s\\E(?!\\w)", re),
          bw_obj_nms[re],
          col,
          perl = TRUE
        )
      }
      col
    }),
    .SDcols = c("est", "se")
  ][]
)

make_surv_estimate_expr_list__ <- function(surv_estimate_expr_list) {
  utility_expr_list <- list()
  for (estimator_nm in names(surv_estimate_expr_list)) {
    for (elem_nm in names(surv_estimate_expr_list[[estimator_nm]])) {
      # e.g. expr = quote(S_lt)
      expr <- surv_estimate_expr_list[[estimator_nm]][[elem_nm]]
      # e.g. expr_expr = quote(substitute(S_lt, utility_expr_list))
      expr_expr <- substitute(
        substitute(expr, utility_expr_list),
        list(expr = expr)
      )
      # e.g. expr = quote(cumprod(1 - n_events / n_at_risk_eff))
      expr <- eval(expr_expr)
      utility_elem_nm <- switch(
        elem_nm,
        est = estimator_nm,
        paste0(estimator_nm, "_", elem_nm)
      )
      utility_expr_list[[utility_elem_nm]] <- expr
      surv_estimate_expr_list[[estimator_nm]][[elem_nm]] <- expr
    }
  }
  return(surv_estimate_expr_list)
}
surv_estimate_expr_list__ <- lapply(
  seq_len(nrow(surv_estimate_expr_table__)),
  function(i) {
    dt <- surv_estimate_expr_table__
    nms <- c("est", "se")
    expr_list <- lapply(nms, function(nm) {
      expr_string <- dt[[nm]][[i]]
      expr_string <- gsub("\\r", "", expr_string)
      parse(text = expr_string)[[1]]
    })
    names(expr_list) <- nms
    return(expr_list)
  }
)
names(surv_estimate_expr_list__) <- surv_estimate_expr_table__[["name"]]
surv_estimate_expr_list__ <- make_surv_estimate_expr_list__(
  surv_estimate_expr_list__
)
