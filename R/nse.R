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
# nse_is_string <- function(expr) {
#   if (inherits(expr, "character")) return(TRUE)
#   FALSE
# }
# nse_is_formula <- function(expr) {
#   if (inherits(expr, "formula")) return(TRUE)
#   identical(as.list(expr)[[1]], quote(`~`))
# }
# 
# 
# nse_quote <- function(expr, substitution.env, source.env) {
#   ## @param expr a function argument passed directly without quoting
#   ## @param substitution.env the environment of the function call ---
#   ## typically can be left missing (taken to be parent env of this function's
#   ## call)
#   if (missing(substitution.env)) substitution.env <- parent.frame(1L)
#   if (missing(source.env)) source.env <- parent.frame(2L)
#   nse_check_env(substitution.env)
#   nse_check_env(source.env)
# 
# 
#   ## suppose this is used within foo(x = something).
#   e <- substitute(expr)
#   stopifnot(inherits(e, "name"), deparse(e) %in% ls(substitution.env))
#   arg_nm <- deparse(e)
#   ## now e == quote(x)
# 
#   e <- substitute(substitute(EXPR), list(EXPR = e))
#   stopifnot(deparse(e) == paste0("substitute(", arg_nm, ")"))
#   ## now e == quote(substitute(x))
#   message(deparse(e))
#   e <- eval(e, envir = substitution.env)
#   message(deparse(e))
# 
#   # e <- substitute(substitute(EXPR), list(EXPR = e))
#   # ## now e == quote(substitute(something))
#   # message(deparse(e))
#   # 
#   # e <- eval(e, envir = as.list(source.env))
# 
#   return(e)
# }
# 
# 
# 
# 
# nse_eval_prep <- function(env, enc) {
#   nse_check_env(env)
#   nse_check_env(enc, allowed.classes = "environment")
#   tmp_env <- as.environment(env)
#   parent.env(tmp_env) <- enc
#   tmp_env
# }
# 
# 
# 
# 
# 
# nse_eval <- function(expr, env, enc) {
#   UseMethod("nse_eval")
# }
# 
# 
# 
# 
# 
# nse_eval.default <- function(expr, env, enc) {
#   stop("No method for object with class(es) ",
#        paste0("'", class(expr), "'", collapse = ", "))
# }
# 
# 
# 
# 
# 
# nse_eval.call <- function(expr, env, enc) {
#   tmp_env <- nse_eval_prep(env, enc)
#   eval(expr, envir = tmp_env)
# }
# 
# 
# 
# 
# 
# nse_eval.name <- function(expr, env, enc) {
#   tmp_env <- nse_eval_prep(env, enc)
#   eval(expr, envir = tmp_env)
# }
# 
# 
# 
# 
# 
# nse_eval.character <- function(expr, env, enc) {
#   tmp_env <- nse_eval_prep(env, enc)
# 
#   missing_vars <- setdiff(expr, ls(tmp_env))
#   if (length(missing_vars)) {
#     stop("Did not find following variables: ",
#          paste0("'", missing_vars, "'", collapse = ", "))
#   }
# 
#   e <- paste0("list(", paste0(expr, collapse = ", "), ")")
#   eval(parse(text = x), envir = tmp_env)
# }
# 
# 
# 
# 
# 
# nse_eval.formula <- function(expr, env, enc) {
#   tmp_env <- nse_eval_prep(env, enc)
# 
#   quoted_list <- attr(terms(expr), "variables")
#   return(quoted_list)
# }
# 
# 
# 
# nse_substituted_arglist <- function() {
#   l <- eval(quote(
#     as.list(match.call())[-1]
#   ), envir = parent.frame(1L))
#   l <- lapply(l, function(expr) {
#     e <- substitute(
#       substitute(expr = EXPR, env = as.list(environment())),
#       list(EXPR = expr)
#     )
#     str(e)
#     e <- eval(e, envir = parent.frame(3L))
#     str(e)
#     e
#   })
#   l
# }
# 
# 
# 
# 
# nse_arg_type <- function(expr, env) {
#   stopifnot(
#     inherits(expr, c("call", "name"))
#   )
#   nse_check_env(env)
#   
#   if (inherits(expr, "name")) {
#     obj_nm <- deparse(expr)
#     return(class(env[[obj_nm]]))
#   } else {
#     return("call")
#   }
#   
# }
# 
# 
# 
# nse_tf <- function(x, ...) {
#   # print(nse_substituted_arglist())
#   df <- data.frame(a = 1:5, b = 5:1)
# 
#   PF <- parent.frame(1L)
#   TF <- environment()
#   e <- nse_quote(expr = x, substitution.env = TF, source.env = PF)
#   message(nse_arg_type(e, env = PF))
#   return(e)
#   nse_eval(e, env = df, enc = PF)
# }
# 
# 
# 
# 
# 
