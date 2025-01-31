


#' @title Flexible Variable Usage in \pkg{popEpi} Functions
#' @author Joonas Miettinen
#' @name flexible_argument
#' @description Certain arguments in \pkg{popEpi} can be passed in multiple
#' ways. This document shows the usage and a pitfall in the
#' usage of such flexible arguments.
#'
#' @details
#'
#' Flexible arguments in \pkg{popEpi} are used to pass variables existing
#' in your data or in the environment where the function is used
#' (for everyday users this is the global environment - in simple terms,
#' where your data is / your work space). The flexible arguments
#' are modelled after the `by` argument in `data.tables` -
#' see `?data.table`. There are many ways to supply the same information
#' to certain functions in \pkg{popEpi}, but the possible ways listed below
#' may be limited in some of them to only allow for using only a part of them.
#'
#' @section Everyday usage:
#'
#' Most commonly you may pass
#' variable names as character strings, e.g.
#'
#' `FUN(arg = c("V1", "V2"), data = x)`
#'
#' which may be stored in advance:
#'
#' `vars <- c("V1", "V2")`
#'
#' `FUN(arg = vars, data = x)`
#'
#' where `x` contains those variables. You may also supply variable
#' names as symbols:
#'
#' `FUN(arg = V1, data = x)`
#'
#' Or as a list of symbols (similarly to as in `[aggregate]`):
#'
#' `FUN(arg = list(V1, V2), data = x)`
#'
#' Or as a list of expressions:
#'
#' `FUN(arg = list(V1 + 1, factor(V2)), data = x)`
#'
#' A formula without a left-hand-side specified is sometimes allowed as well:
#'
#' `FUN(arg = ~ I(V1 + 1) + factor(V2), data = x)`
#'
#' Using a symbol or a list of symbols/expressions typically
#' causes the function to look for the variable(s)
#' first in the supplied data (if any) and then where the function was called.
#' For everyday users this means you might define e.g.
#'
#' `V3 <- factor(letters)`
#'
#' and do e.g.
#'
#' `FUN(arg = list(V1 + 1, factor(V2), V3), data = x)`
#'
#' provided `V1` and `V2` exist in `x` or in the function calling
#' environment.
#'
#' @section A pitfall:
#'
#' There is one way to use flexible arguments incorrectly: By supplying
#' the name of a variable which exists both in the supplied data
#' and the calling environment, and intending the latter to be used. E.g.
#'
#' `vars <- c("V2")`
#'
#' `FUN(arg = V3, data = x)`
#'
#' where `x` has a column named `vars`. This causes the function to
#' use `x$vars` and NOT `x$V2`.
#'
#' @section Advanced:
#'
#' Function programmers are advised to pass character strings
#' whenever possible. To fool-proof against conflicts as described in the
#' section above, refer to the calling environment explicitly when
#' passing the variable containing the character strings:
#'
#' `TF <- environment() ## current env to refer to`
#'
#' `vars <- c("V1", "V2")`
#'
#' `FUN(arg = TF$vars, data = x)`
#'
#' Even if `x` has columns named `vars` and `TF`,
#' using `TF$vars` does not use those columns but only evaluates
#' `TF$vars`
#' in the calling environment. This is made possible by the fact
#' that data is always passed as a `data.frame`, within which evaluation
#' of expressions using the dollar operator is not possible. Therefore
#' it is safe to assume the data should not be used. However, lists of
#' expressions will not be checked for dollar use and will fail in conflict
#' situations:
#'
#' `TF <- environment() ## current env to refer to`
#'
#' `vars <- letters[1:5]`
#'
#' `x <- data.frame(vars = 1:5, TF = 5:1, V1 = 10:6)`
#'
#' `FUN(arg = list(TF$vars, V1), data = x)`
#'
#' On the other hand you may typically also pass quoted (`[quote]`)
#' or substituted `[substitute]` expressions etc., where
#' the `env$object` trick will work as well:
#'
#' `q <- quote(list(vars, V1))`
#'
#' `FUN(arg = TF$q, data = x)`
#'
#' This works even with
#'
#' `a <- 1:5`
#'
#' `V1 <- quote(TF$a)`
#'
#' `FUN(arg = TF$V1, data = x)`
#'
#' So no conflicts should occur.
#' @family popEpi argument evaluation docs
#' @examples
#'
#' data(sire)
#' ## prepare data for e.g. 5-year "period analysis" for 2008-2012
#' ## note: sire is a simulated cohort integrated into popEpi.
#' BL <- list(fot=seq(0, 5, by = 1/12))
#' x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = status %in% 1:2,
#'              breaks = BL)
#'
#' x <- aggre(x, by = fot)
#'
#' ## silly example of referring to pyrs data by fixed character string;
#' ## its possible that the real name wont be fixed in a real-life application.
#' pyrs <- "actual_pyrs"
#' TF <- environment()
#' x$actual_pyrs <- as.numeric(x$pyrs)
#' x$pyrs <- 1
#'
#' ## this works (uses actual_pyrs eventually)
#' st <- survtab_ag(fot ~ 1, data = x, surv.type = "surv.obs",
#'                  pyrs = TF$pyrs, d = from0to1,
#'                  surv.method = "hazard")
#' ## this would be wrong (sees expression 'pyrs' and uses that column,
#' ## which is not what is intended here)
#' st <- survtab_ag(fot ~ 1, data = x, surv.type = "surv.obs",
#'                  pyrs = pyrs, d = from0to1,
#'                  surv.method = "hazard")

NULL









