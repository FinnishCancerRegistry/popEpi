




#' @md
#' @title Survival Objects
#' @description
#' Create survival objects as in  \pkg{survival}::\code{\link[survival]{Surv}}.
#' @param time see  \pkg{survival}::\code{\link[survival]{Surv}}
#' @param time2 see  \pkg{survival}::\code{\link[survival]{Surv}}
#' @param event see  \pkg{survival}::\code{\link[survival]{Surv}}
#' @param type see  \pkg{survival}::\code{\link[survival]{Surv}}
#' @param origin see  \pkg{survival}::\code{\link[survival]{Surv}}
#' @param id argument to be added to \code{\link[survival]{Surv}} in 
#' \pkg{survival} 3.0, included here to ensure a smooth transition;
#' if you have \pkg{survival} `< 3.0` installed, this argument is ignored
#' and a warning is thrown if it is used;
#' else the values are passed to \pkg{survival}::\code{\link[survival]{Surv}}
#' @section Surv in survival vs. in popEpi:
#' This function is a wrapper for  \pkg{survival}::\code{\link[survival]{Surv}}.
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
  origin = 0, 
  id
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
  
  # "id" to be / was added in survival 3.0
  if (!"id" %in% names(formals(survival::Surv))) {
    if (!missing(id)) {
      warning("argument \"id\" was ignored; it is only passed ",
              "to survival::Surv if it has that argument (it didn't), so see ",
              "?survival::Surv (and ?popEpi::Surv)")
    }
    pass_arg_nms <- setdiff(pass_arg_nms, "id")
  }
  
  pass_arg_list <- mget(pass_arg_nms)
  eval_env <- new.env(parent = pf)
  surv_expr <- as.call(c(
    quote(survival::Surv),
    pass_arg_list
  ))
  eval(surv_expr, envir = eval_env)
}




