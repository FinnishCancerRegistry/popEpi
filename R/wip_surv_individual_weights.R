#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_individual_weights",
#'   "surv_functions"
#' )
surv_individual_weights <- function(
  dt,
  standard_weight_dt,
  observed_weight_dt = NULL
) {
  # @codedoc_comment_block surv_arg_dt
  # - `surv_individual_weights`: A `data.frame` / `data.table`.
  # @codedoc_comment_block surv_arg_dt
  assert_is_arg_dt(dt = dt, lexis = FALSE)
  # @codedoc_comment_block popEpi::surv_individual_weights::standard_weight_dt
  # @param standard_weight_dt `[data.table]` (no default)
  #
  # Table of standardisation weights, e.g. ICSS weights.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weights
  # @codedoc_comment_block popEpi::surv_individual_weights::standard_weight_dt
  assert_is_arg_weights(standard_weight_dt, allowed = "data.table")
  # @codedoc_comment_block popEpi::surv_individual_weights
  # Produce a vector of weights, one weight for each row in `dt`.
  # These weights have been called individual weights, Brenner weights,
  # (Brenner et al 2004, https://doi.org/10.1016/j.ejca.2004.07.007),
  # pre-weights, and maybe even others. A beloved child has many names.
  #
  # The idea of these weights is to weigh the individual contribution of each
  # person to the summary statistics from which the survival function estimates
  # themselves are produced. They pronounce the influence of under-represented
  # data and reduce the influence of over-represented data just as the more
  # conventional computation of weighted averages survival function estimates
  # does. However, in this individual weighting approach even a single event
  # can be included in the summary statistics as e.g. 0.80 or 1.20 events
  # for over and under-represented data respectively.
  #
  # This function makes producing these weights easier. It performs the
  # following steps:
  # @codedoc_comment_block popEpi::surv_individual_weights
  stratum_col_nms <- setdiff(names(standard_weight_dt), "weight")
  # @codedoc_comment_block popEpi::surv_individual_weights::observed_weight_dt
  # @param observed_weight_dt `[NULL, data.table]` (default `NULL`)
  #
  # Table of weights in your dataset.
  #
  # - `NULL`: This is computed based on `dt`.
  # - `data.table`: Must be a valid table of weights.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weights
  # @codedoc_comment_block popEpi::surv_individual_weights::observed_weight_dt
  assert_is_arg_weights(observed_weight_dt, allowed = c("NULL", "data.table"))
  if (is.null(observed_weight_dt)) {
    # @codedoc_comment_block popEpi::surv_individual_weights
    # - If `is.null(observed_weight_dt)`, `observed_weight_dt` is computed
    #   by `surv_individual_weights` by simply counting the number of cases
    #   in each stratum in `standard_weight_dt`.
    # @codedoc_comment_block popEpi::surv_individual_weights
    if (!data.table::is.data.table(dt)) {
      dt <- data.table::setDT(lapply(stratum_col_nms, function(col_nm) {
        dt[[col_nm]]
      }))
      data.table::setnames(dt, stratum_col_nms)
    }
    observed_weight_dt <- dt[
      i = standard_weight_dt,
      on = stratum_col_nms,
      #' @importFrom data.table .N
      j = list(weight = .N),
      #' @importFrom data.table .EACHI
      keyby = .EACHI
    ]
  }
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Merge `standard_weight_dt` and `observed_weight_dt` into one table.
  #   Scale the standard weights and the observed weights to sum into one
  #   (they are in separate columns). E.g. one stratum has
  #   `weight_standard = 0.5` and `weight_observed = 0.4`.
  # @codedoc_comment_block popEpi::surv_individual_weights
  weight_dt <- merge(
    x = standard_weight_dt,
    y = observed_weight_dt,
    by = stratum_col_nms,
    suffixes = c("_standard", "_observed")
  )
  data.table::set(
    x = weight_dt,
    j = "weight_standard",
    value = weight_dt[["weight_standard"]] / sum(weight_dt[["weight_standard"]])
  )
  data.table::set(
    x = weight_dt,
    j = "weight_observed",
    value = weight_dt[["weight_observed"]] / sum(weight_dt[["weight_observed"]])
  )
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Compute the individual weights as the standard weights divided by the
  #   observed weights. E.g. one stratum has
  #   `weight_brenner = 0.5 / 0.4 = 1.2`.
  # @codedoc_comment_block popEpi::surv_individual_weights
  data.table::set(
    x = weight_dt,
    j = "weight_brenner",
    value = weight_dt[["weight_standard"]] / weight_dt[["weight_observed"]]
  )
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Using left-join, produce a vector of length `nrow(dt)` where each row in
  #   `dt` gets an individual weight based on its stratum.
  # @codedoc_comment_block popEpi::surv_individual_weights
  out <- weight_dt[
    i = dt,
    on = stratum_col_nms,
    j = .SD,
    .SDcols = "weight_brenner"
  ]
  # @codedoc_comment_block popEpi::surv_individual_weights
  # @codedoc_insert_comment_block return(popEpi::surv_individual_weights)
  # @codedoc_comment_block popEpi::surv_individual_weights
  # @codedoc_comment_block return(popEpi::surv_individual_weights)
  # A vector of weights is returned.
  # @codedoc_comment_block return(popEpi::surv_individual_weights)
  return(out[["weight_brenner"]])
}
