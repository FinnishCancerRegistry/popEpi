# 
# 
# 
# 
# 
# nse_check_env <- function(e, allowed.classes = NULL) {
#   allowed_classes <- c("list", "data.frame", "data.table", "environment")
#   if (!is.null(allowed.classes)) allowed_classes <- allowed.classes
#   if (!inherits(e, allowed_classes)) {
#     stop("Non-standard evaluation problem: passed environment argument ",
#          "did not have any of the following classes: ",
#          paste0("'", allowed_classes, "'", collapse = ", "), 
#          "; instead the class(es) ",
#          "were: ", paste0("'", class(e), "'", collapse = ", "), 
#          ". If you see this, you should complain to the package maintainer ",
#          "and try to supply your arguments in a different way.")
#   }
# }
# 
# 
# 
# 
# 
# nse_quote <- function(expr, substitution.env = NULL) {
#   
#   e <- substitute(expr)
#   e <- substitute(substitute(expr = EXPR), list(EXPR = e))
#   
#   if (missing(substitution.env)) substitution.env <- parent.frame(1L)
#   nse_check_env(substitution.env)
#   
#   e <- eval(e, env = substitution.env)
#   if (is.character(e)) e <- parse(text = e)[[1]]
#   
#   return(e)
# }
# 
# 
# 
# 
# nse_eval_formula_side <- function(f, side = "rhs", env, enc) {
#   stopifnot(
#     inherits(f, "formula"),
#     identical(side, "rhs") || identical(side, "lhs")
#   )
#   nse_check_env(env)
#   nse_check_env(enc, allowed.classes = "environment")
#   
#   side_pos <- switch(side, lhs = 1L, rhs = 2L)
#   side_string <- strsplit(x = deparse(f), split = "~")[[1]][side_pos]
#   side_string <- paste0(side_string, collapse = " ")
#   side_string <- gsub(side_string, pattern = "\\s{1,}", replacement = " ")
#   
#   protection_regex <- "((^)|(\\Q + \\E))(\\QI(.{0,])\\E)((\\Q + \\E)|($))"
#   
#   return(side_string)
# }
# 
# 
# 
# 
# nse_eval_quoted <- function(expr, env, enc) {
#   nse_check_env(env)
#   nse_check_env(enc, allowed.classes = "environment")
#   
#   tmp_env <- as.environment(env)
#   parent.env(tmp_env) <- enc
#   
#   eval(expr, envir = tmp_env)
# }
# 
# 
# 
# 
# 
# nse_quote_testfun <- tf <- function(x) {
#   df <- data.frame(a = 1:5, b = 5:1)
#   
#   e <- nse_quote(expr = x)
#   PF <- parent.frame(1L)
#   
#   nse_eval_quoted(e, env = df, enc = PF)
# }
# 
# 
# 
# 
# 
