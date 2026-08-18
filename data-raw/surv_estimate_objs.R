devtools::load_all()

surv_estimate_expr_table__ <- data.table::fread(
  "data-raw/surv_estimate_expr_table__.csv",
  encoding = "UTF-8"
)

make_surv_estimate_expr_list__ <- function(surv_estimate_expr_list) {
  utility_expr_list <- list(
    h_ch = quote(
      n_events / t_at_risk
    ),
    H_ch = quote(
      cumsum(delta_t * h_ch)
    ),
    S_lt_cond = quote(
      1 - (n_events / n_at_risk_eff)
    ),
    S_ch_cond = quote(
      exp(-delta_t * h_ch)
    ),
    S_lt_lag1 = quote(
      c(
        1.00,
        cumprod(S_lt_cond)[-length(n_events)]
      )
    ),
    S_ch_lag1 = quote(
      c(
        1.00,
        exp(-H_ch)[-length(delta_t)]
      )
    )
  )
  for (utility_expr_nm in names(utility_expr_list)) {
    # e.g. expr = quote(exp(-cumsum(delta_t * h_ch)))
    expr <- utility_expr_list[[utility_expr_nm]]
    # e.g. expr_expr = quote(substitute(exp(-cumsum(delta_t * h_ch)), utility_expr_list))
    expr_expr <- substitute(
      substitute(expr, utility_expr_list),
      list(expr = expr)
    )
    # e.g. expr = quote(exp(-cumsum(delta_t * n_events / t_at_risk)))
    expr <- eval(expr_expr)
    utility_expr_list[[utility_expr_nm]] <- expr
  }
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
