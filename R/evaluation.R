

is_expression <- function(e) {
  
  ifelse(any(c("call", "name") %in% class(e)), TRUE, FALSE)
  
}

is_list_expression <- function(e) {
  
  ifelse(is_expression(e) && deparse(as.list(e)[[1]]) == "list", TRUE, FALSE)
  
}

is_dollar_expression <- function(e) {
  
  ifelse(is_expression(e) && deparse(as.list(e)[[1]]) == "$", TRUE, FALSE)
  
}


is_variable <- function(x) {
  
  varModes <- c("numeric", "complex", "logical", "character", "raw")
  
  ifelse(mode(x) %in% varModes, TRUE, FALSE)
  
}


evalArg <- function(arg, env, enc, ...) {
  UseMethod("evalArg")
}


evalArg.default <- function(arg, env, enc) {
  if (is.list(arg)) {
    arg <- as.list(arg)
  } else if (is_variable(arg)) {
    l <- list(arg)
  }
  return(arg)
}

evalArg.name <- function(arg, env, enc) {
  d <- deparse(arg)
  out <- try(get(d, envir = env, inherits = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) {
    out <- try(get(d, envir = enc, inherits = FALSE), silent = TRUE)
  }
  if (inherits(out, "try-error")) {
    stop("Could not find object ", d, ".")
  }
  out <- list(out)
  names(out) <- d
  out
}

evalArg.call <- function(arg, env, enc) {
  
  out <- eval(arg, envir = env, enclos = enc)
  
  if (is.list(out)) {
    out <- as.list(out)
  } else if (is_variable(out)) {
    out <- list(out)
    names(out) <- paste0(deparse(arg), collapse = "")
  }
  out
}

evalArg.character <- function(arg, env, enc) {
  ## NOTE: enc unused
  se <- substitute(env)
  out <- lapply(arg, function(stri) {
    try({
      get(stri, envir = as.environment(env), inherits = FALSE)
    }, silent = TRUE)
  })
  
  notFound <- arg[sapply(out, inherits, "try-error")]
  
  if (length(notFound)) {
    if (length(notFound) > 5) notFound <- notFound[1:5]
    stop("Could not find object(s): ", 
         paste0(notFound, collapse = ", "), ".")
  }
  names(out) <- arg
  out
}

evalArg.formula <- function(arg, env, enc) {
  
  rhsl <- as.list(RHS2DT(arg, data = env, enclos = enc))
  
  rhsl
  
}


method_classes <- function(f) {
  
  stopifnot(is.character(f))
  e <- utils::methods(f)
  e <- unlist(lapply(e, as.character))
  e <- unlist(lapply(e, sub, pattern = paste0(f, "."), replacement = ""))
  setdiff(e, "default")
  
}


do_evalPopArg <- function(arg, env, enc) {
  eam <- method_classes("evalArg")
  
  ne <- list(env = env, enc = enc)
  de <- list(env = enc, enc = baseenv())
  
  r <- arg
  tick <- 1L
  while (any(class(r) %in% eam)) {
    
    envs <-  if (is_dollar_expression(r)) de else ne
    r <- evalArg(arg = r, env = envs$env, enc = envs$enc)
    
    tick <- tick + 1L
    if (tick == 100L) stop("No result after 100 evaluations")
  }
  
  r
}



argType <- function(arg) {
  
  tl <- list("NULL" = "NULL", character = "character",
             list = "call", formula = "formula", 
             expression = c("call", "name"))
  
  tl <- sapply(tl, function(ch) {
    t <- tryCatch(inherits(arg, ch), 
                  error = function(e) e,
                  warning = function(w) w)
    isTRUE(t)
  })
  if (!any(tl)) tl[names(tl) == "expression"] <- TRUE
  if (tl["list"]) tl["list"] <- substr(deparse(arg), 1, 5) == "list("
  names(tl)[tl & !duplicated(tl)]
  
}





evalPopArg2 <- function(data, arg, enclos, DT = TRUE, 
                        types = c("NULL","character", "list", "expression")) {
  
  allowed_types <- c("NULL", "character", "list", "expression", "formula")
  types <- match.arg(types, allowed_types, 
                     several.ok = TRUE)
  if (!argType(arg) %in% types) {
    stop("Supplied argument not allowed type. Current type: ",
         argType(arg), ". Allowed types: ",
         paste0(types, collapse = ", "))
  }
  
  l <- do_evalPopArg(arg = arg, env = data, enc = enclos)
  l
}




evalPopArg <- function(data, arg, n = 1L, DT = TRUE, enclos = NULL, recursive = TRUE, types = c("NULL","character", "list", "expression"), naming = c("DT", "model")) {
  ## arg: an unevaluated AND substitute()'d argument within a function, which may be
  ## * an expression
  ## * a list of expressions
  ## * a character vector of variable names (in a given data set)
  ## n: steps upstream as in parent.frame(n); 0L refers to calling environment
  ## of evalPopArg, 1L to calling environment of e.g. sir which uses evalPopArg, etc.
  ## hence n = 1L should be almost always the right way to go.
  ## ALTERNATIVELY supply an environment by hand via enclos.
  ## enclos will override n.
  ## recursive: if TRUE, evals arg as many times as it is of type language.
  ## output:
  ## * vector as a result of an expression
  ## * list as a result of a list
  ## * character vector of names
  ## OR with DT = TRUE, a data.table based on aforementioned results.
  ## intention: output to be used in by argument of data.table.
  ## a data.table output is directly usable in by.
  ## if column names cannot be easily found, BV1, BV2, ... are imputed
  ## for missing names (unrobustly: such names may already exist, resulting in duplicates)
  
  ## naming: DT style uses first element of all.names() where 
  ## a name has to be created; model style keeps the whole deparsed
  ## expression. Only applied when DT = TRUE
  naming <- match.arg(naming[1L], c("DT", "model"))
  
  ## types: allowed popArg types of arguments.
  types <- match.arg(types, c("NULL","character", "list", "expression", "formula"), several.ok = TRUE)
  
  if (!is.null(enclos) && !is.environment(enclos)) {
    stop("enclos must be NULL or an environment")
  }
  if (!is.environment(enclos)) enclos <- parent.frame(n + 1L)
  
  ## used data may change if expression uses dollar operator, hence
  ## arg should not be evaluated within data but only its surroundings.
  use_data <- data
  use_enc <- enclos
  dataNames <- names(data)
  
  if (uses_dollar(arg, data.names = dataNames)) {
    use_data <- enclos
    use_enc <- baseenv()
  }
  e <- eval(arg, envir = use_data, enclos = use_enc)
  if (is.language(e) && !inherits(e, "formula")) {
    if (!recursive) stop("arg is of type language after evaluating, and recursive = FALSE")
    
    tick <- 1L
    while (is.language(e) && !inherits(e, "formula") && tick < 100L) {
      arg <- e
      use_data <- data
      use_enc <- enclos
      if (uses_dollar(arg, data.names = dataNames)) {
        use_data <- enclos
        use_enc <- baseenv()
      }
      e <- eval(arg, envir = use_data, enclos = use_enc)
      tick <- tick + 1L
    }
    if (tick == 100L) stop("arg was of type language even after 100 evaluations. Something went wrong here...")
    
    
    
  } 
  argType <- "NULL"
  if (is.list(e)) argType <- "list" else 
    if (is.character(e)) argType <- "character" else 
      if (mode(e) == "numeric" || is.vector(e) || is.factor(e)) argType <- "expression" else 
        if (inherits(e, "formula")) argType <- "formula"
  
  if (!argType %in% types) stop("popArg type of evaluated arg not one of the allowed types (set via argument types). Detected type: '", argType, "'. Allowed types: ", paste0("'", types, "'", collapse = ", "))
  
  if (argType == "NULL") return(NULL)
  
  av <- all.vars(arg)
  if (argType == "character") av <- e
  
  ## byNames: names of columns resulting from aggre argument, by which
  ## pyrs and such are aggregated. same functionality
  ## as in results seen in e.g.DT[, .N, by = list(factor(x), y, z = w)] ## factor, y, z
  ## note: first object in ags with list or expression aggre is "list"
  byNames <- NULL
  
  if (is.character(e)) byNames <- e
  else if (argType == "list" && substr(paste0(deparse(arg)), 1, 5) == "list(") byNames <- sapply(arg[-1], function(x) all.names(x)[1]) 
  else if (argType == "expression") byNames <- all.names(arg)[1]
  
  badNames <- c("$", ":")
  
  byNames[byNames %in% badNames] <- paste0("BV", 1:length(byNames))[byNames %in% badNames]
  
  if (argType == "formula") {
    arg <- e
    use_data <- data
    use_enc <- enclos
    e <- RHS2DT(formula = e, data = use_data, enclos = use_enc)
    if (ncol(e) == 0L || nrow(e) == 0L) e <- data.table() ## e.g. y ~ 1
    
  } else if (is.character(e)) {
    all_names_present(data, e)
    if (DT) {
      ## note: e contains variable names in character strings,
      ## ergo fully named list & DT created
      l <- lapply(e, function(x) data[[x]])
      setattr(l, "names", e)
      setDT(l)
      e <- l; rm(l)
    }
  } else if (is.list(e)) {
    ## note: fully unnamed list has NULL names()
    ## partially named list has some "" names
    ne <- names(e)
    
    if (DT && any(sapply(e, is.null))) stop("at least one object in list arg is NULL; cannot form data.table with such list")
    
    if (is.null(ne)) ne <- rep("", length(e))
    
    
    wh_bad <- which(ne == "")
    if (length(wh_bad) > 0) {
      if (is.null(byNames)) {
        byNames <- paste0("BV", 1:length(e))
      }
      
      ne[wh_bad] <- byNames[wh_bad]
      setattr(e, "names", ne)
    }
    
    if (DT) {
      ## NOTE: used to be setDT, but length of different elements
      ## in list may differ, which as.data.table handles correctly
      e <- as.data.table(e)
    }
  } else if (mode(e) == "numeric" || is.vector(e) || is.factor(e)) {
    ## is e.g. a numeric vector or a factor
    if (DT) {
      e <- data.table(V1 = e)
      setnames(e, 1, byNames)
    }
  }
  
  ## NOTE: e may be of type language at this point if arg was double-quoted
  ## and recursive = FALSE
  
  if (DT) {
    setDT(e)
    setattr(e, "all.vars", av)
    setattr(e, "quoted.arg", arg)
    setattr(e, "arg.type", argType)
    if (naming == "model" && ncol(e) > 0L) setnames(e, 1:ncol(e), popArg2ModelNames(arg, type = argType))
  }
  e
}


popArgType <- function(arg, data = NULL, n = 1L, enclos = NULL, recursive = TRUE) {
  ## input: a substitute()'d expression / argument
  ## NOTE: recursive useful when arg might be quoted twice and want the eventual
  ## result; need to supply data for it though
  ## output: type of thingie that was substitute()'d
  ##  * list (of expressions)
  ##  * character string vector
  ##  * an expression (includes symbol)
  av <- all.vars(arg, unique = TRUE) ## all variables
  av <- setdiff(av, c("$", "T", "F"))
  an <- all.names(arg, unique = TRUE) ## all variables and functions
  af <- setdiff(an, av) ## all functions used
  
  a <- deparse(arg)
  a <- paste0(a, collapse = "") ## lists may somehow produce length > 1 here
  if (substr(a, 1, 5) == "list(") return("list")
  if (a == "NULL") return("NULL")
  ## detection of character arguments is not easy and should not be considered
  ## fool proof since user may pass e.g. a vector of character strings as a 
  ## symbol, which can only really be interpreted as an expression
  if (sum(grep('\\"', a)) && length(setdiff(af, "c")) == 0) return("character")
  
  if (is.data.frame(data)) {
    if (is.symbol(arg) && a %in% names(data)) return("expression")
    if (length(av) == 1L && av %in% names(data)) return("expression")
    e <- eval(arg, envir = data[1:min(nrow(data), 20L), ], 
              enclos = if (is.environment(enclos)) enclos else parent.frame(n + 1L))
    if (inherits(e, "formula")) return("formula")
    if (is.null(e)) return("NULL")
    if (is.list(e)) return("list")
    if (is.character(e) && all(e %in% names(data))) return("character")
    if (is.vector(e) || is.factor(e)) return("expression")
    
    if (recursive && is.language(e)) return(popArgType(e, data = data, n = n + 1L, enclos = enclos))
  }
  
  "expression"
  
}
popArg2ModelNames <- function(arg, type) {
  ## INTENTION: given a quoted/substituted expression,
  ## digs out the expression(s) creating a/multiple column(s)
  ## and returns the deparsed expression(s) to be used as names
  ## of columns the same way that models such as lm() display
  ## the names of expressions used within formula
  
  ## some exceptions
  if (is.data.frame(arg)) return(names(arg))
  if (is.character(arg)) return(arg)
  
  type <- match.arg(type[1L], c("NULL", "character", "list", "expression", "formula"))
  
  lang <- NULL
  lang <- try(is.language(arg) || inherits(arg, "formula"), silent = TRUE)
  
  
  if (inherits(lang, "try-error") || !lang) stop("arg must be a quoted or substituted expression or a formula. Error message: ", lang, ". type of arg: ", typeof(arg), ". Class: ", class(arg), ". Mode: ", mode(arg), ".")
  
  d <- oneWhitespace(paste0(deparse(arg)))
  
  if (type == "expression") return(d) else 
    if (type == "NULL") return(NULL) else 
      if (type == "character") return(eval(arg)) else 
        if (type == "list") {
          d <- substr(d, 6, nchar(d)-1L) ## removes "list(" and ")"
          d <- strsplit(d, ", ")
          return(unlist(d))
        } else if (type == "formula") {
          arg <- eval(arg)
          d <- names(RHS2list(arg))
          if (length(d) == 0L) return(NULL) ## e.g. y ~ 1
          return(d)
        }
  stop("could not determine deparsed-expression-names")
}





uses_dollar <- function(q, data.names) {
  ## INTENTION: determine whether q is an expressions that is evaluated
  ## outside a data.frame, i.e. one that uses the dollar operator.
  ## e.g. TF$V1 should not be evaluated in a data.frame even if it has
  ## the variables TF and V1 since it wont work and was not intended.
  if (!is.language(q) || inherits(q, "formula")) {
    return(FALSE)
  }
  
  d <- deparse(q)
  ## sometimes d is of length > 1 for some reason...
  d <- paste0(d, collapse = "")
  d <- oneWhitespace(d)
  
  if (substr(d, 1, 4) == "list") {
    ## lists are not allowed to work in this manner for now.
    return(FALSE)
  }
  
  if (!grepl(x = d, pattern = "\\$")) {
    ## does not use dollar operator.
    return(FALSE)
  }
  
  ## detect if word$word is used in d
  t <- regexec(pattern = "\\w+\\$\\w+", text = d)
  if (t != -1) {
    ## ok, used word$word
    ## is there are variable with that name in data.names?
    m <- unlist(regmatches(d, t))
    if (m %in% data.names) {
      return(FALSE)
    }
    ## if not, it should be evaluated outside the data.
    return(TRUE)
  } 
  
  return(FALSE)
}




`%.:%` <- function(x, y) {
  ## INTENTION: hacking formula calls using `:`
  ## which is apparently normally evaluated in C... (in model.matrix.default)
  ## USAGE: e.g. c(1,2) %.:% c(3,4) = c(3, 8)
  ## (instead of getting warning)
  if (length(x) > 1L && length(y) > 1L && is.numeric(x) && is.numeric(y)) {
    return(x*y)
  } else if (length(x) == 1L && length(y) == 1L && is.numeric(x) && is.numeric(y)) {
    return(x:y)
  }
  as.factor(x):as.factor(y)
  
}



RHS2list <- function(formula, handle.adjust=TRUE) {
  ## INTENTION: turns the right-hand side of a formula
  ## into a list of substituted expressions;
  ## each element in list is an expressions separated
  ## by a '+' in the formula. needs to be eval()'d,
  ## preferably using the appropriate data set.
  if (!inherits(formula, "formula")) stop("not a formula")
  
  ## no response
  formula <- formula[c(1, length(formula))]
  
  te <- terms(formula)
  tl <- attr(te, "term.labels")
  
  ## handle adjusting variables (e.g. adjust(V1, V2) -> c("V1", "V2"))
  adj <- tl[substr(tl, 1, 7) == "adjust("]
  if (length(adj) == 0L) adj <- NULL
  if (handle.adjust && !is.null(adj)) {
    tl <- setdiff(tl, adj)
    adj <- unlist(lapply(adj, function(stri) {
      e <- parse(text = stri)[[1]]
      e <- as.list(e)[-1]
      unlist(lapply(e, deparse))
    }))
    
    tl <- c(tl, adj)
  }
  
  ## to avoid e.g. c(1,2):c(3,4) NOT evaluating as c(3, 8)
  l <- lapply(tl, function(x) gsub(pattern = ":", x = x, replacement = "%.:%"))
  l <- lapply(l, function(x) parse(text = x)[[1L]])
  
  names(l) <- tl
  
  setattr(l, "adjust", adj)
  
  l
}

uses_colon <- function(e) {
  if (is.character(e)) e <- parse(text = e)[[1]]
  stopifnot(is.call(e) || is.name(e))
  stopifnot(!inherits(e, "formula"))
  
  l <- as.list(e)
  if (deparse(l[[1]]) %in% c(":", "%.:%") && length(l) == 3) {
    return(TRUE)
  }
  FALSE
}

replace_colon <- function(e) {
  stopifnot(is.call(e) || is.name(e))
  stopifnot(!inherits(e, "formula"))
  
  if (!uses_colon(e)) return(e)
  
  l <- as.list(e)
  l[[1]] <- parse(text = "popEpi:::`%.:%`")[[1]]
  as.call(l)
}

RHS2DT <- function(formula, data = data.frame(), enclos = parent.frame(1L)) {
  l <- RHS2list(formula)
  if (length(l) == 0L) return(data.table())
  adj <- attr(l, "adjust")
  
  ## foolproofing in case data contains column named e.g. "a:b" and that is 
  ## intended to be used
  dana <- names(data)
  dana <- gsub(x=dana, pattern=" %.:% ", replacement = ":")
  dana <- gsub(x=dana, pattern="%.:%", replacement = ":")
  
  
  ld <- lapply(l, deparse)
  ld <- lapply(ld, function(ch) {
    ch <- gsub(x=ch, pattern=" %.:% ", replacement = ":")
    ch <- gsub(x=ch, pattern="%.:%", replacement = ":")
    int <- if (ch %in% dana) which(dana %in% ch) else ch
    if (is.integer(int)) data[[names(data)[int]]] else NULL
  })
  ld[which(sapply(ld, is.null))] <- NULL
  l[names(ld)] <- ld
  
  ## foolproofs use of function %.:% by explicit referral, i.e. :::
  ## (this avoids scoping problem)
  l <- lapply(l, function(elem) {
    if (!is.call(elem)) return(elem)
    if (!uses_colon(elem)) return(elem)
    
    replace_colon(elem)
    
  })
  
  l <- lapply(l, function(elem) {
    eval(expr = elem, envir = data, enclos = enclos)
  })
  
  l <- as.data.table(l)
  setattr(l, "adjust", adj)
  l
}





Surv2DT <- function(Surv) {
  sa <- attributes(Surv)
  type <- sa$type
  
  dt <- as.data.table(lapply(sa$dimnames[[2]], function(col_nm) {
    Surv[, col_nm]
  }))
  setnames(dt, names(dt), sa$dimnames[[2]])
  
  statNA <- sum(is.na(dt$status))
  if (statNA) {
    stop("Some status indicators (", statNA  ," values in total) were NA. ",
         "Usual suspects: original status variable ",
         "has NA values, or you have numeric status variable with more than ",
         "two levels and you did not assign e.g. type = 'mstate' (e.g. ",
         "Surv(time = c(1,1,1), event = c(0,1,2), type = 'mstate') works).")
    
  }
  
  
  setattr(dt, "type", type)
  
  label_sources <- c("time2", "event")
  lapply(label_sources, function(lbl_src) {
    if (identical(sa$inputAttributes[[lbl_src]]$class, "factor")) {
      set(
        dt, j = "status", 
        value = factor(dt$status, labels = sa$inputAttributes[[lbl_src]]$levels)
      )
    }
    NULL
  })
  
  dt[]
}





evalPopFormula <- function(formula, data = data.frame(), enclos = parent.frame(2L), subset = NULL, Surv.response = TRUE) {
  
  ## INTENTION: given a formula object, returns a DT where each column
  ## is an evaluated expression from formula (separated by  + )
  
  fe <- environment(formula)
  
  either <- FALSE
  if (is.character(Surv.response)) {
    Surv.response <- match.arg(Surv.response, "either")
    Surv.response <- TRUE
    either <- TRUE
  } else if (!is.logical(Surv.response)) {
    stop("Surv.response must be either logical or 'either'")
  }
  
  ## subset if needed ----------------------------------------------------------
  if (!is.null(subset) && !is.logical(subset)) {
    stop("subset must be NULL or a logical vector and not an expression at ",
         "this point. If you see this, complain to the package maintainer.")
  }
  if (!is.null(subset)) {
    keepVars <- c(all.vars(formula), "lex.Xst")
    data <- subsetDTorDF(data, subset = subset, select = keepVars)
  }
  
  
  ## formula -------------------------------------------------------------------
  if (!inherits(formula, "formula")) {
    stop("formula is not of class 'formula'; supply it as e.g. y ~ x")
  }
  if (length(formula) < 3L) {
    stop("formula appears to be one-sided, which is not supported; ",
         "supply it as e.g. y ~ x")
  }
  
  ## response
  y <- eval(formula[[2L]], envir = data, enclos = enclos)
  if (inherits(y, "Surv") && !either && !Surv.response) {
    stop("Response is a result of using Surv(), which is not allowed in ",
         "this context.")
  }
  
  if (!inherits(y, "Surv") && !either && Surv.response) {
    stop("The response of the formula must be a Surv object; ",
         "see ?Surv (in package survival).")
  }
  
  if (inherits(y, "Surv")) {
    y <- Surv2DT(y)
    setcolsnull(y, keep = c("time", "start", "status"), colorder = TRUE)
    if (!any(c("time", "start") %in% names(y))) {
      stop("You must supply function Surv a value to the 'time' ",
           "argument. See ?Surv")
    }
    setnames(y, names(y), c("time", "status")[1:ncol(y)])
  } else {
    y <- data.table(y)
    setnames(y, 1, deparse(formula[[2L]]))
    if (either && inherits(data, "Lexis")) {
      ## we assume the unmodified lex.Xst to be a useful status variable.
      if (!"lex.Xst" %in% names(data)) {
        stop("Supplied a formula without using Surv(), and data was a Lexis ",
             "object, so assumed you intended to use 'lex.Xst' in data as the ",
             "status variable in this context, but that column was missing ",
             "from data.")
      }
      setnames(y, 1, "time")
      y[, "status"] <- data$lex.Xst
      
    }
  }
  
  
  ## RHS
  l <- RHS2DT(formula, data = data, enclos = enclos)
  adj <- attr(l, "adjust")
  
  ## combine
  l <- if (length(l) > 0L) cbind(y, l) else y
  
  setattr(l, "adjust.names", adj)
  setattr(l, "print.names", setdiff(names(l), c(adj, names(y))))
  setattr(l, "Surv.names", names(y))
  setattr(l, "formula", formula)
  
  l
}


evalRecursive <- function(arg, env, enc, max.n = 100L) {
  ## INTENTION: digs out actual evaluatable value and expression
  
  if (missing(env)) env <- environment()
  if (missing(enc)) enc <- parent.frame(1L)
  
  if (is.data.frame(env)) {
    na <- names(env)
    env <- env[1:(min(10L, nrow(env))), ]
    
    env <- data.frame(env)
    setattr(env, "names", na)
    
  }
  argSub <- arg
  
  tick <- 1L
  while (!inherits(arg, "try-error") && is.language(arg) && 
         !inherits(arg, "formula") && tick < max.n) {
    
    argSub <- arg
    arg <- try(eval(argSub, envir = env, enclos = enc), silent = TRUE)
    
    tick <- tick + 1L
  }
  
  if (tick == max.n) {
    stop("evaluated expression ", max.n, 
         " times and still could not find underlying expression")
  }
  if (!is.language(argSub)) argSub <- substitute(arg)
  list(arg = arg, argSub = argSub, all.vars = all.vars(argSub))
}


usePopFormula <- function(form = NULL, adjust = NULL, data = data.frame(), 
                          enclos, Surv.response = TRUE) {
  ## INTENTION: evaluates form and combines with adjust appropriately
  ## returns a list of the elements dug out from the formula and adjust 
  ## arguments.
  # formSub <- substitute(form)
  al <- evalRecursive(arg = form, env = data, enc = enclos)
  
  if (!inherits(al$arg, "formula")) stop("'form' is not a formula object")
  
  dt <- evalPopFormula(formula = al$arg, data = data, enclos = enclos, 
                       Surv.response = Surv.response)
  adNames <- attr(dt, "adjust.names")
  prNames <- attr(dt, "print.names")
  suNames <- attr(dt, "Surv.names")
  
  adjust <- evalPopArg(data, adjust, DT = TRUE, recursive = TRUE, 
                       enclos = new.env(), naming = "model",
                       types = c("NULL", "character", "list", "expression"))
  
  if (is.data.frame(adjust) && (nrow(adjust) == 0L || ncol(adjust) == 0L)) {
    stop("adjust evaluated to an empty data.frame")
  }
  if (!is.null(adjust) && ncol(adjust) > 0L && length(adNames) > 0L) {
    stop("Cannot both use argument 'adjust' AND use an adjust() term within ",
         "the formula argument. Please only use one.")
  }
  if (is.null(adjust) && length(adNames) > 0L) {
    adjust <- dt[, .SD, .SDcols = c(adNames)]
  }
  
  
  print <- NULL
  if (length(prNames > 0L)) print <- dt[, .SD, .SDcols = eval(prNames)]
  
  list(y = dt[, .SD, .SDcols = c(suNames)], 
       print = print, 
       adjust = adjust, formula = al$arg)
}





#' @title Adjust Estimates by Categorical Variables
#' @description This function is only intended to be used within a formula
#' when supplied to e.g. \code{\link{survtab_ag}} and should not be
#' used elsewhere. 
#' @param ... variables to adjust by, e.g. \code{adjust(factor(v1), v2, v3)}
#' @return Returns a list of promises of the variables supplied which can be
#' evaluated.
#' @examples 
#' 
#' y ~ x + adjust(z)
#' @export
adjust <- function(...) {
  
  call <- sys.call(1L)
  call <- as.list(call)[1L]

  if (deparse(call) %in% c("adjust", "list(adjust)")) {
    stop("Function adjust() only intended to be used within the formulas of ",
         "certain functions of package popEpi. See e.g. ?survtab_ag for usage.")
  }
  
  mc <- as.list(match.call())[-1L]
  if (is.list(mc) && length(mc) == 1) mc <- mc[[1]]
  mc
}





parse_adjust_formula <- function(f) {
  
  env <- environment(f)
  t <- attr(terms(f), "term.labels")
  
  l <- sapply(t, grepl, pattern = "adjust(", fixed = TRUE)
  
  a <- t[l]
  
  if (!any(l)) return(f)
  
  f <- deparse(f)
  f <- paste0(f, collapse = "")
  f <- oneWhitespace(f)
  
  for (k in seq_along(a)) {
    f <- sub(x = f, pattern = paste0(" + ", a[k]), replacement = "", fixed = TRUE)
  }
  
  a <- lapply(a, function(stri) {
    e <- parse(text = stri)[[1]]
    as.list(e)[-1]
  })
  
  a <- unlist(a, recursive = FALSE)
  
  a <- sapply(a, function(e) {
    
    e <- substitute(adjust(e), list(e = e))
    deparse(e)
    
  })
  
  f <- paste0(f, " + ", paste0(a, collapse = " + "))
  f <- eval(parse(text = f)[[1]])
  environment(f) <- env
  f
}





model_frame_robust <- function(formula, data, enc) {
  
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.environment(enc))
  
  pe <- function(x, ...) {
    eval(parse(text = x), ...)
  }
  
  fo <- formula
  
  te <- terms(fo)
  tl <- attr(te, "term.labels")
  
  av <- all.vars(fo)
  av <- intersect(av, names(data))
  
  ## non-interactions as they are
  l <- lapply(tl, function(stri) {
    if (stri %in% names(data)) return(data[[stri]])
    e <- try(pe(stri, envir = data, enclos = enc), silent = TRUE)
    if (inherits(e, "try-error")) e <- stri
    e
  })
  
  fa <- attr(te, "factors")
  
  whInter <- colnames(fa)[colSums(fa) > 1]
  
  whInter <- which(sapply(l, function(elem) {
    is.character(elem) && length(elem) == 1L && elem %in% whInter
  }))
  whVar <- setdiff(seq_along(l), whInter)
  
  if (sum(whInter)) {
    
    fa <- fa[, whInter, drop = FALSE]
    interList <- lapply(colnames(fa), function(stri) {
      
      e <- parse(text = stri)[[1]]
      e <- as.list(e)[-1]
      e <- sapply(e, deparse)
      
    })
    
    names(interList) <- colnames(fa)
    
    interVars <- unique(unlist(interList))
    
    
    interData <- lapply(interVars, function(stri) {
      if (stri %in% names(data)) return(data[[stri]])
      eval(parse(text = stri), envir = data, enclos = enc)
    })
    
    interData <- setDT(lapply(interData, as.factor))
    names(interData) <- interVars
    
    on <- copy(names(interData))
    tn <- paste0("V", seq_along(interData))
    names(tn) <- on
    names(interData) <- tn
    
    til <- lapply(interList, function(stri) {
      tn[stri]
    })
    names(til) <- sapply(til, paste0, collapse = ":")
    
    
    
    l[whInter] <- lapply(seq_along(til), function(i) {
      
      tmpExpr <- til[[i]]
      realExpr <- l[whInter][[i]]
      e <- try(pe(tmpExpr, envir = interData), silent = TRUE)
      
      if (inherits(e, "try-error")) {
        e <- pe(realExpr, envir = data, enclos = enc)
      }
      e
    })
    
  }
  
  names(l) <- tl
  l <- as.data.table(l)
  l
}





dt_robust_by <- function(e, by.var.nms) {
  stopifnot(
    length(e) == 1,
    is.character(e),
    grepl(x = e, pattern = "by\\s{0,}=\\s{0,}%%BY_VAR_NMS%%"),
    length(by.var.nms) == 0 || is.character(by.var.nms)
  )
  
  le <- paste0("list(", paste0("`", by.var.nms, "`", collapse = ", "), ")")
  if (!length(by.var.nms)) le <- "NULL"
  
  e <- gsub(x = e, pattern = "%%BY_VAR_NMS%%", fixed = TRUE, replacement = le)
  eval(parse(text = e), envir = parent.frame(1L))
  NULL
}









