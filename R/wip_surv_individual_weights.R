#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_individual_weights",
#'   "surv_functions"
#' )
#' @examples
#'

#' # popEpi::surv_individual_weights
#' make_column_icss_ag <- function(age) {
#'   cut(
#'     age,
#'     breaks = c(seq(15, 85, 5), Inf),
#'     right = FALSE
#'   )
#' }
#' 
#' make_standard_weight_dt <- function() {
#'   wdt <- popEpi::ICSS[
#'     j = list(
#'       weight = as.double(sum(.SD[["ICSS1"]]))
#'     ),
#'     keyby = list(
#'       icss_ag = make_column_icss_ag(popEpi::ICSS[["age"]])
#'     )
#'   ][]
#'   return(wdt[])
#' }
#' 
#' lexis <- Epi::Lexis(
#'   entry = list(ts_fut = c(0.0, 0.0)),
#'   exit = list(ts_fut = c(1.5, 2.5)),
#'   entry.status = 0L,
#'   exit.status = 0L
#' )
#' lexis[["icss_ag"]] <- make_column_icss_ag(c(50.5, 60.5))
#' wdt <- make_standard_weight_dt()
#' 
#' # this is what happens when at least one weighting stratum is empty
#' warn_env <- new.env()
#' warn_env[["w"]] <- NULL
#' iw_bad <- suppressWarnings(withCallingHandlers(popEpi::surv_individual_weights(
#'   df = lexis,
#'   standard_weight_dt = wdt
#' ), warning = function(w) warn_env[["w"]] <- w))
#' stopifnot(
#'   inherits(warn_env[["w"]], "warning"),
#'   iw_bad >= 0
#' )
#' 
#' # here is an easy way to handle this problem. note that the weights are
#' # always larger when we need to aggregate strata --- because the standard
#' # weight increases but the observed weight remains the same. the same
#' # observations serve "double duty" now for multiple original strata.
#' iw <- popEpi::surv_individual_weights(
#'   df = lexis,
#'   standard_weight_dt = wdt,
#'   collapse_stratum_col_nms = "icss_ag"
#' )
#' stopifnot(
#'   length(iw) == nrow(lexis),
#'   iw >= 0.0,
#'   iw >= iw_bad
#' )
surv_individual_weights <- function(
  df,
  standard_weight_dt,
  observed_weight_dt = NULL,
  collapse_stratum_col_nms = NULL
) {
  #' @param df `[data.frame, data.table]` (no default)
  #'
  #' Dataset containing stratum columns also found in `standard_weight_dt`.
  stopifnot(
    is.data.frame(df),
    is.null(collapse_stratum_col_nms) || (
      all(collapse_stratum_col_nms %in% names(df)) &&
        all(collapse_stratum_col_nms %in% names(standard_weight_dt))
    )
  )
  # @codedoc_comment_block popEpi::surv_individual_weights::standard_weight_dt
  # @param standard_weight_dt `[data.table]` (no default)
  #
  # Table of standardisation weights, e.g. ICSS weights.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weight_dt
  # @codedoc_comment_block popEpi::surv_individual_weights::standard_weight_dt
  assert_is_arg_weight_dt(standard_weight_dt, df)

  stratum_col_nms <- setdiff(names(standard_weight_dt), "weight")
  dt <- data.table::setDT(as.list(df)[stratum_col_nms])
  # @codedoc_comment_block popEpi::surv_individual_weights
  # Produce a vector of weights, one weight for each row in `df`.
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
  # @codedoc_comment_block popEpi::surv_individual_weights::observed_weight_dt
  # @param observed_weight_dt `[NULL, data.table]` (default `NULL`)
  #
  # Table of weights in your dataset.
  #
  # - `NULL`: Weights are computed using `df`. See **Details**.
  # - `data.table`: Must be a valid table of weights.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weight_dt
  # @codedoc_comment_block popEpi::surv_individual_weights::observed_weight_dt
  assert_is_arg_weight_dt(observed_weight_dt, dt)
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
  # - At this point the weight table might look like this:
  #
  # |ag       | weight_standard| weight_observed|
  # |:--------|---------------:|---------------:|
  # |(0,15]   |         0.00000|       0.0000000|
  # |(15,20]  |         0.00164|       0.0000000|
  # |(20,25]  |         0.00215|       0.0004110|
  # |(25,30]  |         0.00416|       0.0030037|
  # |(30,35]  |         0.00842|       0.0092639|
  # |(35,40]  |         0.01874|       0.0174213|
  # |(40,45]  |         0.03489|       0.0317440|
  # |(45,50]  |         0.04906|       0.0641520|
  # |(50,55]  |         0.07094|       0.0962755|
  # |(55,60]  |         0.09641|       0.1094916|
  # |(60,65]  |         0.13359|       0.1531554|
  # |(65,70]  |         0.14234|       0.1582459|
  # |(70,75]  |         0.14766|       0.1212217|
  # |(75,80]  |         0.14131|       0.0954218|
  # |(80,85]  |         0.09347|       0.0718983|
  # |(85,Inf] |         0.05522|       0.0682939|
  #
  # @codedoc_comment_block popEpi::surv_individual_weights
  weight_dt <- merge(
    x = standard_weight_dt,
    y = observed_weight_dt,
    by = stratum_col_nms,
    suffixes = c("_standard", "_observed")
  )
  weight_dt[
    i = is.na(weight_dt[["weight_observed"]]),
    #' @importFrom data.table :=
    j = "weight_observed" := 0L
  ]
  data.table::set(
    x = weight_dt,
    j = c("weight_standard", "weight_observed"),
    value = lapply(c("weight_standard", "weight_observed"), function(col_nm) {
      weight_dt[[col_nm]] / sum(weight_dt[[col_nm]])
    })
  )
  bad_row_nos <- which(
    weight_dt[["weight_observed"]] == 0 & weight_dt[["weight_standard"]] != 0
  )
  if (length(bad_row_nos) > 0) {
    #' @param collapse_stratum_col_nms `[NULL, character]` (default `NULL`)
    #'
    #' Optional names of stratum column names in both `df` and
    #' `standard_weight_dt`. If this argument is supplied, strata defined by
    #' these columns are attempted to be combined with their neighbours in the
    #' event of a zero in the corresponding `observed_weight_dt$weight` value.
    #' For instance, with `collapse_stratum_col_nms = "agegroup"` and the
    #' first age group having an observed weight of zero, we combine the first
    #' age group with the second for the purpose of calculating the individual
    #' weight. See **Details**.
    #'
    #' - `NULL`: No automatic combination of strata is performed.
    #' - `character`: Combine neighbouring strata defined by these column names.
    #'   E.g. `"agegroup"`.
    if (length(collapse_stratum_col_nms) == 0) {
    # @codedoc_comment_block popEpi::surv_individual_weights
    # - If the weight table contains rows where the observed weight is zero and
    #   the standard weight is not, the individual weights will not work
    #   properly.
    #   + If `is.null(collapse_stratum_col_nms)`, a warning is thrown.
    # @codedoc_comment_block popEpi::surv_individual_weights
      warning(
        "Individual weights will be wrong to some extent because there ",
        "are one or more strata where the observed weight is zero but ",
        "standard ",
        "weight is not zero. This typically occurs when e.g. there are no ",
        "patients of a certain age group. To correct the error ",
        "arising from this, aggregate neighbouring strata where possible, ",
        "e.g. use larger age groups. You may use the argument ",
        "`collapse_stratum_col_nms` to make ",
        "`popEpi::surv_individual_weights` do that for you."
      )
    } else {
    # @codedoc_comment_block popEpi::surv_individual_weights
    #   + Else we combine strata identified by `collapse_stratum_col_nms`
    #     to ensure there are zero problematic rows in the weight table.
    # - At this point the weight table looks like e.g. (`weight_standard` and
    #   `weight_observed` are the same for the 2nd and 3rd rows because they
    #   were combined --- but we keep the original strata because we need them
    #   for the join later)
    #
    # |ag       | weight_standard| weight_observed|
    # |:--------|---------------:|---------------:|
    # |(0,15]   |         0.00000|       0.0000000|
    # |(15,20]  |         0.00379|       0.0004110|
    # |(20,25]  |         0.00379|       0.0004110|
    # |(25,30]  |         0.00416|       0.0030037|
    # |(30,35]  |         0.00842|       0.0092639|
    # |(35,40]  |         0.01874|       0.0174213|
    # |(40,45]  |         0.03489|       0.0317440|
    # |(45,50]  |         0.04906|       0.0641520|
    # |(50,55]  |         0.07094|       0.0962755|
    # |(55,60]  |         0.09641|       0.1094916|
    # |(60,65]  |         0.13359|       0.1531554|
    # |(65,70]  |         0.14234|       0.1582459|
    # |(70,75]  |         0.14766|       0.1212217|
    # |(75,80]  |         0.14131|       0.0954218|
    # |(80,85]  |         0.09347|       0.0718983|
    # |(85,Inf] |         0.05522|       0.0682939|
    # @codedoc_comment_block popEpi::surv_individual_weights
      noncollapse_stratum_col_nms <- setdiff(
        stratum_col_nms,
        collapse_stratum_col_nms
      )
      weight_dt[
        #' @importFrom data.table := .SD
        j = c("weight_standard", "weight_observed") := {
          sub_dt <- data.table::setDT(as.list(.SD)[
            c(collapse_stratum_col_nms,
              "weight_standard", "weight_observed")
          ])
          sub_dt[
            #' @importFrom data.table := .GRP
            j = "collapse_stratum_start" := .GRP,
            by = eval(collapse_stratum_col_nms)
          ]
          data.table::set(
            x = sub_dt,
            j = "collapse_stratum_stop",
            value = sub_dt[["collapse_stratum_start"]] + 1L
          )
          data.table::set(
            x = sub_dt,
            j = collapse_stratum_col_nms,
            value = NULL
          )
          collapsed_dt <- surv_collapse_1d__(
            dt = sub_dt,
            ts_col_nm = "collapse_stratum",
            value_col_nms = c("weight_standard", "weight_observed"),
            test_expr = quote(!(weight_observed == 0 & weight_standard != 0))
          )
          data.table::set(
            x = sub_dt,
            j = "__collapsed_stratum_id__",
            value = cut(
              x = sub_dt[["collapse_stratum_start"]],
              breaks = union(
                collapsed_dt[["collapse_stratum_start"]],
                collapsed_dt[["collapse_stratum_stop"]]
              ),
              right = FALSE,
              labels = FALSE
            )
          )
          collapsed_dt[
            i = sub_dt[["__collapsed_stratum_id__"]],
            #' @importFrom data.table .SD
            j = .SD,
            .SDcols = c("weight_standard", "weight_observed")
          ]
        },
        by = eval(noncollapse_stratum_col_nms)
      ]
    }
  }
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Compute the individual weights as the standard weights divided by the
  #   observed weights. E.g. one stratum has
  #   `weight_brenner = 0.5 / 0.4 = 1.2`. If either the standard or observed
  #   weight is zero then set the Brenner weight to zero.
  # - At this point the table looks like e.g. (note: `weight_brenner` was made
  #   larger by our combining the strata in rows 2 and 3. In fact it will
  #   always be larger when we combine strata because `weight_standard` is
  #   summed and becomes greater but `weight_observed` does not become any
  #   greater. The observations here serve "double duty" and represent all the
  #   strata in the combination.):
  #
  # |ag       | weight_standard| weight_observed| weight_brenner|
  # |:--------|---------------:|---------------:|--------------:|
  # |(0,15]   |         0.00000|       0.0000000|      0.0000000|
  # |(15,20]  |         0.00379|       0.0004110|      9.2214110|
  # |(20,25]  |         0.00379|       0.0004110|      9.2214110|
  # |(25,30]  |         0.00416|       0.0030037|      1.3849735|
  # |(30,35]  |         0.00842|       0.0092639|      0.9089002|
  # |(35,40]  |         0.01874|       0.0174213|      1.0756964|
  # |(40,45]  |         0.03489|       0.0317440|      1.0991045|
  # |(45,50]  |         0.04906|       0.0641520|      0.7647460|
  # |(50,55]  |         0.07094|       0.0962755|      0.7368441|
  # |(55,60]  |         0.09641|       0.1094916|      0.8805243|
  # |(60,65]  |         0.13359|       0.1531554|      0.8722511|
  # |(65,70]  |         0.14234|       0.1582459|      0.8994864|
  # |(70,75]  |         0.14766|       0.1212217|      1.2180987|
  # |(75,80]  |         0.14131|       0.0954218|      1.4808988|
  # |(80,85]  |         0.09347|       0.0718983|      1.3000304|
  # |(85,Inf] |         0.05522|       0.0682939|      0.8085640|
  #
  # @codedoc_comment_block popEpi::surv_individual_weights
  data.table::set(
    x = weight_dt,
    j = "weight_brenner",
    value = data.table::fifelse(
      weight_dt[["weight_observed"]] == 0L,
      0.0,
      weight_dt[["weight_standard"]] / weight_dt[["weight_observed"]]
    )
  )
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Using left-join, produce a vector of length `nrow(df)` where each row in
  #   `df` gets an individual weight based on its stratum.
  # @codedoc_comment_block popEpi::surv_individual_weights
  out <- weight_dt[
    i = dt,
    on = stratum_col_nms,
    j = .SD,
    .SDcols = "weight_brenner"
  ]
  # @codedoc_comment_block popEpi::surv_individual_weights
  # - Return a vector of weights.
  # @codedoc_comment_block popEpi::surv_individual_weights
  # @codedoc_comment_block return(popEpi::surv_individual_weights)
  # Returns a vector of weights.
  # @codedoc_comment_block return(popEpi::surv_individual_weights)
  return(out[["weight_brenner"]])
}
