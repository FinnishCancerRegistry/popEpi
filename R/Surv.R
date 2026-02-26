#' @description
#' `popEpi::Surv` simply calls `[survival::Surv]`. This wrapper was written
#' simply to avoid doing e.g. `library(survival)` when `Surv` used in a
#' formula evaluated by `popEpi`.
#' @title Survival Objects
#' @param time see  [survival::Surv]
#' @param time2 see  [survival::Surv]
#' @param event see  [survival::Surv]
#' @param type see  [survival::Surv]
#' @param origin see [survival::Surv]
#' @export
#' @family main functions
#' @family survtab functions
#' @family survmean functions
#' @return
#' See `[survival::Surv]`.
#'
Surv <- function(
  time,
  time2,
  event,
  type = c("right", "left", "interval", "counting", "interval2"),
  origin = 0
) {
  call_env <- parent.frame(1L)
  surv_call <- match.call()
  surv_call[[1]] <- quote(survival::Surv)
  out <- eval(surv_call, call_env)
  return(out)
}
