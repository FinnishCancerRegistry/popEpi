




#' @md
#' @title Survival Objects
#' @description
#' Wrapper for [survival::Surv].
#' @param time see  [survival::Surv]
#' @param time2 see  [survival::Surv]
#' @param event see  [survival::Surv]
#' @param type see  [survival::Surv]
#' @param origin see [survival::Surv]
#' @section Surv in survival vs. in popEpi:
#' `popEpi::Surv` is a wrapper for [survival::Surv].
#' Therefore you don't need to to do `library("survival")` when using `Surv` 
#' with e.g.
#' \code{\link{survtab}}. Remember that if you do `library("survival")` after
#' `library("popEpi")`, the `Surv` from \pkg{survival} is used instead of
#' from \pkg{popEpi} (`R` throws a warning about this) when an expression
#' such as `Surv(my_times, my_events)` is evaluated. You can avoid such
#' conflicts by writing e.g. `popEpi::Surv(my_times, my_events)` instead.
#' However, `popEpi::Surv` is designed in such a way that this should not
#' become a problem and you should be able to use the two interchangeably.
#' @export
#' @family main functions
#' @family survtab functions
#' @family survmean functions
#' @importFrom survival Surv
#' 
Surv <- function(
  time, 
  time2, 
  event, 
  type = c("right", "left", "interval", "counting", "interval2", "mstate"), 
  origin = 0
) {
  
  pf <- parent.frame(1)
  arg_nms <- names(formals(Surv))
  test_env <- environment()
  is_missing <- vapply(arg_nms, function(arg_nm) {
    eval(substitute(
      missing(OBJ),
      list(OBJ = parse(text = arg_nm)[[1]])
    ), envir = test_env)
  }, logical(1))
  pass_arg_nms <- arg_nms[!is_missing]
  pass_arg_nms <- intersect(pass_arg_nms, names(formals(survival::Surv)))
  
  pass_arg_list <- mget(pass_arg_nms)
  eval_env <- as.environment(pass_arg_list)
  parent.env(eval_env) <- pf
  expr_args <- lapply(pass_arg_nms, function(stri) {
    parse(text = stri)[[1]]
  })
  names(expr_args) <- pass_arg_nms
  surv_expr <- as.call(c(
    quote(survival::Surv),
    expr_args
  ))
  eval(surv_expr, envir = eval_env)
}




