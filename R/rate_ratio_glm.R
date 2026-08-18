rate_ratio_glm__ <- function(
  d1,
  y1,
  d2,
  y2
) {
  stopifnot(
    inherits(d1, c("integer", "numeric")),
    inherits(d2, c("integer", "numeric")),
    length(d1) == length(d2),
    d1 %% 1 == 0,
    d2 %% 1 == 0,
    length(y1) == length(d1),
    length(y2) == length(d2),
    inherits(y1, c("integer", "numeric")),
    inherits(y2, c("integer", "numeric"))
  )
  dt <- data.table::setDT(list(
    d = rep(NA_real_, 2),
    log_y = rep(NA_real_, 2),
    grp = factor(c("b", "a"))
  ))
  out <- data.table::setDT(list(
    rr = rep(NA_real_, length(d1)),
    rr_se = rep(NA_real_, length(d1)),
    rr_lo = rep(NA_real_, length(d1)),
    rr_hi = rep(NA_real_, length(d1))
  ))
  lapply(
    seq_along(d1),
    function(i) {
      data.table::set(
        x = dt,
        j = c("d", "log_y"),
        value = list(
          c(d1[i], d2[i]),
          log(c(y1[i], y2[i]))
        )
      )
      fit <- stats::glm(
        formula = d ~ grp,
        offset = dt[["log_y"]],
        data = dt,
        family = "poisson"
      )
      est <- exp(coef(fit)[2L])
      # Var(theta) ~ (theta ^ 2) * Var(ln(theta))
      var <- stats::vcov(fit)["grpb", "grpb"]
      var <- (est^2) * var
      se <- sqrt(var)
      ci <- suppressMessages(exp(confint(fit, parm = "grpb")))
      data.table::set(
        x = out,
        i = i,
        j = c("rr", "rr_se", "rr_lo", "rr_hi"),
        value = list(
          est,
          se,
          ci[1],
          ci[2]
        )
      )
      NULL
    }
  )
  return(out)
}
