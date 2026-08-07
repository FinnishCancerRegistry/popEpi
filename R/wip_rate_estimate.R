assert_is_arg_t_at_risk_dt <- function(t_at_risk_dt, dt = NULL) {
  stopifnot(
    data.table::is.data.table(t_at_risk_dt),
    "t_at_risk" %in% names(t_at_risk_dt),
    !duplicated(t_at_risk_dt, by = setdiff(names(t_at_risk_dt), "t_at_risk"))
  )
  if (!is.null(dt)) {
    stopifnot(
      setdiff(names(t_at_risk_dt), "t_at_risk") %in% names(dt)
    )
  }
  return(invisible(NULL))
}

#' @title Rate Estimates
#' @description
#' Produce estimates of rates, optionally directly adjusted.
#' @eval codedoc::pkg_doc_fun("popEpi::rate_estimate")
#' @examples
#' # popEpi::rate_estimate
#'
#' # this data.table::setDTthreads call is included here only to
#' # conform to the CRAN submission requirement to only use at most 2
#' # threads. you do not need to set this to use popEpi.
#' # however some long calculations may benefit from using more threads.
#' data.table::setDTthreads(2L)
#'
#' dt <- data.table::CJ(
#'   cancer_type = c("brain", "lung"),
#'   sex = 0:1,
#'   year = 2001:2002,
#'   age_group = 1:18
#' )
#' dt[j = "n" := rpois(n = .N, lambda = 100)]
#'
#' tar_dt <- data.table::copy(dt)
#' tar_dt[j = "n" := n * runif(n = .N, min = 10.0, max = 100.0)]
#' data.table::setnames(tar_dt, "n", "t_at_risk")
#'
#' rdt_1 <- popEpi:::rate_estimate(
#'   dt = dt,
#'   stratum_col_nms = c("cancer_type", "sex", "year", "age_group"),
#'   count_col_nms = "n",
#'   t_at_risk_dt = tar_dt,
#'   weight_dt = NULL
#' )
#' stopifnot(
#'   c("cancer_type", "sex", "year", "age_group") %in% names(rdt_1),
#'   "t_at_risk" %in% names(rdt_1)
#' )
#'
#' # world standard population, 2000 edition and 1966 edition, in 5-year age groups
#' wdt_world <- data.table::data.table(
#'   age_group = 1:18,
#'   weight_world_1966 = c(
#'     0.12,
#'     0.1,
#'     0.09,
#'     0.09,
#'     0.08,
#'     0.08,
#'     0.06,
#'     0.06,
#'     0.06,
#'     0.06,
#'     0.05,
#'     0.04,
#'     0.04,
#'     0.03,
#'     0.02,
#'     0.01,
#'     0.005,
#'     0.005
#'   ),
#'   weight_world_2000 = c(
#'     0.088569,
#'     0.08687,
#'     0.08597,
#'     0.08467,
#'     0.082171,
#'     0.079272,
#'     0.076073,
#'     0.071475,
#'     0.065877,
#'     0.060379,
#'     0.053681,
#'     0.045484,
#'     0.037187,
#'     0.02959,
#'     0.022092,
#'     0.015195,
#'     0.009097,
#'     0.006348
#'   )
#' )
#'
#' # one weight column only
#' rdt_2 <- popEpi:::rate_estimate(
#'   dt = dt,
#'   stratum_col_nms = c("cancer_type", "sex", "year", "age_group"),
#'   count_col_nms = "n",
#'   t_at_risk_dt = tar_dt,
#'   weight_dt = data.table::data.table(
#'     age_group = wdt_world[["age_group"]],
#'     weight = wdt_world[["weight_world_2000"]]
#'   )
#' )
#' stopifnot(
#'   c("cancer_type", "sex", "year") %in% names(rdt_2),
#'   !"age_group" %in% names(rdt_2),
#'   "t_at_risk" %in% names(rdt_2)
#' )
#'
#' # multiple weight columns
#' rdt_3 <- popEpi:::rate_estimate(
#'   dt = dt,
#'   stratum_col_nms = c("cancer_type", "sex", "year", "age_group"),
#'   count_col_nms = "n",
#'   t_at_risk_dt = tar_dt,
#'   weight_dt = wdt_world
#' )
#' stopifnot(
#'   c("cancer_type", "sex", "year") %in% names(rdt_3),
#'   !"age_group" %in% names(rdt_3),
#'   "t_at_risk" %in% names(rdt_3)
#' )
#'
rate_estimate <- function(
  dt,
  stratum_col_nms,
  count_col_nms,
  t_at_risk_dt,
  weight_dt = NULL,
  conf_lvls = 0.95,
  conf_methods = "identity"
) {
  #' @param dt `[data.frame, data.table]` (no default)
  #'
  #' A `data.frame` or `data.table` containing columns of counts
  #' (`count_col_nms`) by stratifying columns `stratum_col_nms`.
  #' This object must contain every possible stratum combination of your
  #' stratifying columns. For instance every age group must be included in `dt`
  #' even if their count column values are zero. This is necessary for the
  #' correct merging of `t_at_risk` and direct adjusting.
  stopifnot(
    data.table::is.data.table(dt),
    #' @param stratum_col_nms `[character]` (no default)
    #'
    #' Names of stratifying columns in `dt`. E.g.
    #' `c("sex", "age_group", "year")`.
    stratum_col_nms %in% names(dt),
    !duplicated(dt, by = stratum_col_nms),
    #' @param count_col_nms `[character]` (no default)
    #'
    #' Names of count columns in `dt`. E.g.
    #' `c("n_cancer", "n_dead")`.
    count_col_nms %in% names(dt)
  )
  for (count_col_nm in count_col_nms) {
    eval(substitute(
      stopifnot(
        !is.na(dt[[count_col_nm]]),
        dt[[count_col_nm]] >= 0
      ),
      list(count_col_nm = count_col_nm)
    ))
  }
  rm(count_col_nm)
  # if (
  #   data.table::uniqueN(dt, by = stratum_col_nms) !=
  #     prod(vapply(
  #       stratum_col_nms,
  #       function(stratum_col_nm) {
  #         data.table::uniqueN(dt[[stratum_col_nm]])
  #       },
  #       integer(1L)
  #     ))
  # ) {
  #   stop("Argument `dt` does not contain every combination of its stratifying columns")
  # }
  #' @param t_at_risk_dt `[data.table]` (no default)
  #'
  #' `data.table` object containing exactly one value column `"t_at_risk"` and
  #' one or more stratifying columns.
  assert_is_arg_t_at_risk_dt(t_at_risk_dt, dt = dt)
  #' @param weight_dt `[NULL, data.table]` (default `NULL`)
  #'
  #' Optional `data.table` of direct adjusting weights passed to
  #' `[directadjusting::directly_adjusted_estimates]`. The stratifying columns
  #' of this table must be found also in `dt` and will be used for adjusting.
  #' E.g. `data.table::data.table(age_group = 1:18, weight = some_values)`.
  assert_is_arg_weight_dt(
    weight_dt,
    dt = dt,
    allowed = c("NULL", "data.table")
  )
  adjust_col_nms <- names(weight_dt)[!grepl("^weight", names(weight_dt))]
  dt <- data.table::setDT(as.list(dt)[intersect(
    c(
      stratum_col_nms,
      names(t_at_risk_dt),
      count_col_nms
    ),
    names(dt)
  )])

  # adding t_at_risk -----------------------------------------------------------
  # @codedoc_comment_block popEpi::rate_estimate
  # `popEpi::rate_estimate` performs the following steps:
  #
  # - Add column `t_at_risk` from `t_at_risk_dt` to (copy of) `dt`.
  # @codedoc_comment_block popEpi::rate_estimate
  dt_join_assign(
    x = dt,
    i = t_at_risk_dt,
    on = setdiff(names(t_at_risk_dt), "t_at_risk"),
    x_col_nms = "t_at_risk"
  )

  # rates ----------------------------------------------------------------------
  # @codedoc_comment_block popEpi::rate_estimate
  # - The name of the rate columns will usually be
  #   `paste0("r_", count_col_nms)`, e.g. `r_my_count_column`. But for those
  #   count columns that start with `n_`, this will be replaced,
  #   e.g. `n_cancer` -> `r_cancer`. Also, `n` -> `r`.
  #   The standard error column names simply have
  #   the suffix `_se` at the end, e.g. `r_cancer_se`.
  # @codedoc_comment_block popEpi::rate_estimate
  rate_col_nms <- data.table::fcase(
    grepl("^n_", count_col_nms) , sub("^n_", "r_", count_col_nms) ,
    count_col_nms == "n"        , "r"                             ,
    default = paste0("r_", count_col_nms)
  )
  var_col_nms <- paste0(rate_col_nms, "_var")
  names(var_col_nms) <- names(rate_col_nms) <- count_col_nms
  lapply(count_col_nms, function(count_col_nm) {
    # @codedoc_comment_block popEpi::rate_estimate
    # - Compute a rate for each `count_col_nms` element as
    #   `dt[[count_col_nm]] / dt[["t_at_risk"]]`.
    # - Compute the variance of the rate as the rate divided by
    #   `dt[["t_at_risk"]]`.
    #   The formula is `n / (t_at_risk ^ 2) = r / t_at_risk`.
    # @codedoc_comment_block popEpi::rate_estimate
    data.table::set(
      x = dt,
      j = rate_col_nms[count_col_nm],
      value = dt[[count_col_nm]] / dt[["t_at_risk"]]
    )
    data.table::set(
      x = dt,
      j = var_col_nms[count_col_nm],
      value = dt[[rate_col_nms[count_col_nm]]] / dt[["t_at_risk"]]
    )
  })

  # adjusting ------------------------------------------------------------------
  output_stratum_col_nms <- setdiff(
    stratum_col_nms,
    adjust_col_nms
  )
  out <- dt[
    j = lapply(.SD, sum),
    .SDcols = c(count_col_nms, "t_at_risk"),
    keyby = eval(output_stratum_col_nms)
  ]
  # @codedoc_comment_block popEpi::rate_estimate
  # - Call `[directadjusting::directly_adjusted_estimates]`. Note that direct
  #   adjusting is not performed if `weight_dt = NULL`.
  # @codedoc_comment_block popEpi::rate_estimate
  adt <- directadjusting::directly_adjusted_estimates(
    stats_dt = dt,
    stat_col_nms = rate_col_nms,
    var_col_nms = var_col_nms,
    stratum_col_nms = output_stratum_col_nms,
    adjust_col_nms = adjust_col_nms,
    weights = weight_dt,
    #' @param conf_lvls
    #' Passed to `[directadjusting::directly_adjusted_estimates]`.
    conf_lvls = conf_lvls,
    #' @param conf_methods
    #' Passed to `[directadjusting::directly_adjusted_estimates]`.
    conf_methods = conf_methods
  )
  add_value_col_nms <- setdiff(names(adt), output_stratum_col_nms)
  dt_join_assign(
    x = out,
    i = adt,
    on = output_stratum_col_nms,
    x_col_nms = add_value_col_nms
  )
  # @codedoc_comment_block popEpi::rate_estimate
  # - Turn the variance columns produced by
  #   `[directadjusting::directly_adjusted_estimates]` into standard error
  #   columns using `sqrt`.
  # @codedoc_comment_block popEpi::rate_estimate
  meta <- attr(adt, "directly_adjusted_estimates_meta")
  data.table::set(
    x = out,
    j = meta[["meta_dt"]][["var_col_nm_w"]],
    value = lapply(meta[["meta_dt"]][["var_col_nm_w"]], function(vcn) {
      sqrt(out[[vcn]])
    })
  )
  meta[["meta_dt"]][["se_col_nm_w"]] <- sub(
    "_var$",
    "_se",
    meta[["meta_dt"]][["var_col_nm_w"]]
  )
  data.table::setnames(
    x = out,
    old = meta[["meta_dt"]][["var_col_nm_w"]],
    new = meta[["meta_dt"]][["se_col_nm_w"]]
  )
  data.table::setcolorder(
    out,
    c(output_stratum_col_nms, count_col_nms, "t_at_risk")
  )
  data.table::setkeyv(out, output_stratum_col_nms)
  # @codedoc_comment_block popEpi::rate_estimate
  # - Add the attribute `directly_adjusted_estimates_meta` from the output of
  #   the `[directadjusting::directly_adjusted_estimates]` into the output of
  #   this function.
  # - Return a `data.table` with columns of strata, counts, rates, and
  #   `t_at_risk`.
  # @codedoc_comment_block popEpi::rate_estimate
  #' @return
  #' Returns a `data.table` with columns of strata, counts, rates, and
  #' `t_at_risk`.
  data.table::setattr(
    out,
    "directly_adjusted_estimates_meta",
    meta
  )
  return(out[])
}
