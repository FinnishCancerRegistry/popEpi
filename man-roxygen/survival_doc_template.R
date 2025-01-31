#' @title Estimate Survival Time Functions
#' @description This function estimates survival time functions: survival,
#' relative/net survival, and crude/absolute risk functions (CIF).
#'
#' @param adjust can be used as an alternative to passing variables to
#' argument `formula` within a call to `adjust()`; e.g.
#' `adjust = "agegr"`. [Flexible input][flexible_argument].
#'
#' @param weights typically a list of weights or a `character` string
#' specifying an age group standardization scheme; see
#' the [dedicated help page][direct_standardization]
#' and examples. NOTE: `weights = "internal"` is based on the counts
#' of persons in follow-up at the start of follow-up (typically T = 0)
#'
#' @param surv.type one of `'surv.obs'`,
#' `'surv.cause'`, `'surv.rel'`,
#' `'cif.obs'` or `'cif.rel'`;
#' defines what kind of survival time function(s) is/are estimated; see Details
#'
#' @param surv.method either `'lifetable'` or `'hazard'`; determines
#' the method of calculating survival time functions, where the former computes
#' ratios such as `p = d/(n - n.cens)`
#' and the latter utilizes subject-times
#' (typically person-years) for hazard estimates such as `d/pyrs`
#' which are used to compute survival time function estimates.
#' The former method requires argument `n.cens` and the latter
#' argument `pyrs` to be supplied.
#'
#' @param relsurv.method  either `'e2'` or `'pp'`;
#' defines whether to compute relative survival using the
#' EdererII method or using Pohar-Perme weighting;
#' ignored if `surv.type != "surv.rel"`
#'
#' @param subset a logical condition; e.g. `subset = sex == 1`;
#' subsets the data before computations
#'
#' @param conf.level confidence level used in confidence intervals;
#' e.g. `0.95` for 95 percent confidence intervals
#'
#' @param conf.type character string; must be one of `"plain"`,
#' `"log-log"` and `"log"`;
#' defines the transformation used on the survival time
#' function to yield confidence
#' intervals via the delta method
#'
#' @param verbose logical; if `TRUE`, the function is chatty and
#'  returns some messages and timings along the process
#'
#' @section Basics:
#'
#' This function computes interval-based estimates of survival time functions,
#' where the intervals are set by the user. For product-limit-based
#' estimation see packages \pkg{survival} and \pkg{relsurv}.
#'
#' if `surv.type = 'surv.obs'`, only 'raw' observed survival
#' is estimated over the chosen time intervals. With
#' `surv.type = 'surv.rel'`, also relative survival estimates
#' are supplied in addition to observed survival figures.
#'
#' `surv.type = 'cif.obs'` requests cumulative incidence functions (CIF)
#' to be estimated.
#' CIFs are estimated for each competing risk based
#' on a survival-interval-specific proportional hazards
#' assumption as described by Chiang (1968).
#' With `surv.type = 'cif.rel'`, a CIF is estimated with using
#' excess cases as the ''cause-specific'' cases. Finally, with
#' `surv.type = 'surv.cause'`, cause-specific survivals are
#' estimated separately for each separate type of event.
#'
#' In hazard-based estimation (`surv.method = "hazard"`) survival
#' time functions are transformations of the estimated corresponding hazard
#' in the intervals. The hazard itself is estimated using counts of events
#' (or excess events) and total subject-time in the interval. Life table
#' `surv.method = "lifetable"` estimates are constructed as transformations
#' of probabilities computed using counts of events and counts of subjects
#' at risk.
#'
#'
#' The vignette \href{../doc/survtab_examples.html}{survtab_examples}
#' has some practical examples.
#'
#' @section Relative survival:
#'
#' When `surv.type = 'surv.rel'`, the user can choose
#' `relsurv.method = 'pp'`, whereupon Pohar-Perme weighting is used.
#' By default `relsurv.method = 'e2'`, i.e. the Ederer II method
#' is used to estimate relative survival.
#'
#' @section Adjusted estimates:
#'
#' Adjusted estimates in this context mean computing estimates separately
#' by the levels of adjusting variables and returning weighted averages
#' of the estimates. For example, computing estimates separately by
#' age groups and returning a weighted average estimate (age-adjusted estimate).
#'
#' Adjusting requires specification of both the adjusting variables and
#' the weights for all the levels of the adjusting variables. The former can be
#' accomplished by using `adjust()` with the argument `formula`,
#' or by supplying variables directly to argument `adjust`. E.g. the
#' following are all equivalent:
#'
#' `formula = fot ~ sex + adjust(agegr) + adjust(area)`
#'
#' `formula = fot ~ sex + adjust(agegr, area)`
#'
#' `formula  = fot ~ sex, adjust = c("agegr", "area")`
#'
#' `formula  = fot ~ sex, adjust = list(agegr, area)`
#'
#' The adjusting variables must match with the variable names in the
#' argument `weights`;
#' see the [dedicated help page][direct_standardization].
#' Typically weights are supplied as a `list` or
#' a `data.frame`. The former can be done by e.g.
#'
#' `weights = list(agegr = VEC1, area = VEC2)`,
#'
#' where `VEC1` and `VEC2` are vectors of weights (which do not
#' have to add up to one). See
#' \href{../doc/survtab_examples.html}{survtab_examples}
#' for an example of using a `data.frame` to pass weights.
#'
#'
#' @section Period analysis and other data selection schemes:
#'
#' To calculate e.g. period analysis (delayed entry) estimates,
#' limit the data when/before supplying to this function.See
#' \href{../doc/survtab_examples.html}{survtab_examples}.
#'
#' @return
#' Returns a table of life time function values and other
#' information with survival intervals as rows.
#' Returns some of the following estimates of survival time functions:
#'
#' \itemize{
#'  \item `surv.obs` - observed (raw, overall) survival
#'  \item `surv.obs.K` - observed cause-specific survival for cause K
#'  \item `CIF_k` - cumulative incidence function for cause `k`
#'  \item `CIF.rel` - cumulative incidence function using excess cases
#'  \item `r.e2` -  relative survival, EdererII
#'  \item `r.pp` -  relative survival, Pohar-Perme weighted
#' }
#' The suffix `.as` implies adjusted estimates, and `.lo` and
#' `.hi` imply lower and upper confidence limits, respectively.
#' The prefix `SE.` stands for standard error.
#'
#'
#' @seealso
#' `[splitMulti]`, `[lexpand]`,
#' `[ICSS]`, `[sire]`
#' \href{../doc/survtab_examples.html}{The survtab_examples vignette}
#'
#' @references
#'
#' Perme, Maja Pohar, Janez Stare, and Jacques Esteve.
#' "On estimation in relative survival." Biometrics 68.1 (2012): 113-120.
#' \doi{10.1111/j.1541-0420.2011.01640.x}
#'
#' Hakulinen, Timo, Karri Seppa, and Paul C. Lambert.
#' "Choosing the relative survival method for cancer survival estimation."
#' European Journal of Cancer 47.14 (2011): 2202-2210.
#' \doi{10.1016/j.ejca.2011.03.011}
#'
#' Seppa, Karri, Timo Hakulinen, and Arun Pokhrel.
#' "Choosing the net survival method for cancer survival estimation."
#' European Journal of Cancer (2013).
#' \doi{10.1016/j.ejca.2013.09.019}
#'
#' CHIANG, Chin Long. Introduction to stochastic processes in biostatistics.
#' 1968. ISBN-14: 978-0471155003
#'
#' Seppa K., Dyba T. and Hakulinen T.: Cancer Survival,
#' Reference Module in Biomedical Sciences. Elsevier. 08-Jan-2015.
#' \doi{10.1016/B978-0-12-801238-3.02745-8}
#'
#' @family main functions
#' @family survtab functions

