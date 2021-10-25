
surv_dt <- function(...) {
  
  ddd <- list(...)
  
  # this works around "id" argument added in survival 3.0
  allowed_arg_nms <- union(names(formals(survival::Surv)), c("id", ""))
  bad_arg_nms <- setdiff(names(ddd), allowed_arg_nms)
  if (length(bad_arg_nms)) {
    stop("unrecognised argument name(s): ",
         deparse(bad_arg_nms), "; see ?survival::Surv for allowed arguments")
  }
  non_surv_arg_nms <- setdiff(names(ddd), c(names(formals(survival::Surv)), ""))
  non_surv_args <- ddd[non_surv_arg_nms]
  surv_args <- ddd[!names(ddd) %in% non_surv_arg_nms]
  surv_matrix <- do.call(survival::Surv, surv_args)
  
  surv_dt <- data.table::data.table(.__TMP = surv_matrix[, 1])
  data.table::setnames(surv_dt, ".__TMP", colnames(surv_matrix)[1])
  data.table::set(
    x = surv_dt,
    j = colnames(surv_matrix)[-1],
    value = lapply(2:ncol(surv_matrix), function(col_pos) {
      surv_matrix[, col_pos]
    })
  )
  if (length(non_surv_arg_nms) > 0) {
    data.table::set(
      x = surv_dt,
      j = non_surv_arg_nms,
      value = non_surv_args
    )
  }
  data.table::setattr(
    surv_dt, 
    "Surv", 
    list(call = match.call(),
         col_nms = data.table::copy(names(surv_dt)))
  )
  data.table::setattr(
    surv_dt, "class", union("surv_dt", class(surv_dt))
  )
  surv_dt[]
}

nse_expr_to_list_expr <- function(j) {
  UseMethod("nse_expr_to_list_expr")
}

nse_expr_to_list_expr.default <- function(j) {
  stop("No method defined for j of class(es) ", deparse(class(j)))
}

nse_expr_to_list_expr.character <- function(j) {
  parse(text = paste0("list(", paste0(j, collapse = ", "), ")"))[[1]]
}

nse_expr_to_list_expr.expression <- function(j) {
  nse_expr_to_list_expr(j = j[[1]])
}

nse_expr_to_list_expr.name <- function(j) {
  as.call(list(quote(list), j))
}

nse_expr_to_list_expr.call <- function(j) {
  j_elems <- as.list(j)
  is_two_name_expr <- length(j_elems) == 3 && 
    is.name(j_elems[[2]]) && inherits(j_elems[[3]], c("name", "character"))
  is_dollar_expr <- identical(j_elems[[1]], `$`) && is_two_name_expr
  is_double_bracket_expr <- identical(j_elems[[1]], `[[`) && is_two_name_expr
  is_extract_expr <- is_dollar_expr || is_double_bracket_expr
  if (is_extract_expr) {
    # data.table looks for OBJ outside of dt when it is written ..OBJ
    j_elems[2:3] <- lapply(j_elems[2:3], function(j_elem) {
      j_elem_string <- paste0(deparse(j_elem), collapse = "")
      if (is.name(j_elem) && substr(j_elem_string, 1, 2) != "..") {
        j_elem <- parse(text = paste0("..", j_elem_string))[[1]]
      }
      j_elem
    })
  }
  
  if (substr(deparse(j)[1], 1, 5) != "list(") {
    j <- as.call(c(list(quote(list)), j))
  }
  j
}

nse_expr_to_list_expr.formula <- function(j) {
  attr(terms(j), "variables")
}

nse_eval <- function(
  dt,
  j,
  enclos,
  ...
) {
  stopifnot(
    data.table::is.data.table(dt),
    inherits(j, c("call", "name", "expression", "formula", "character","NULL")),
    is.environment(enclos)
  )
  original_j <- j
  if (inherits(j, "expression")) {
    j <- j[[1]]
  } else if (inherits(j, "NULL")) {
    return(NULL)
  }
  
  # handle quoted expressions pointing to quoted expressions -------------------
  if (inherits(j, "name")) {
    test_dt <- dt[0L, ]
    test_env <- new.env(parent = enclos)
    test_env[["test_dt"]] <- test_dt
    prev_test <- test <- j
    max_tries <- 10L
    n_tries <- 0L
    while (inherits(test, "name") && n_tries < max_tries) {
      prev_test <- test
      test_expr <- substitute(
        test_dt[j = list(list(prev_test))], list(prev_test = prev_test)
      )
      test <- try(
        eval(test_expr, envir = test_env), 
        silent = TRUE
      )[[1]][[1]]
      n_tries <- n_tries + 1L
    }
    j <- prev_test
  }
  
  # standardise to list expression ---------------------------------------------
  list_j <- nse_expr_to_list_expr(j = j)
  
  # create and eval final expression -------------------------------------------
  ddd <- as.list(substitute(list(...)))
  if (length(ddd) > 1) {
    ddd <- ddd[-1]
  } else {
    ddd <- NULL
  }
  call_arg_list <- c(
    list(
      name = "[",
      x = quote(dt),
      j = substitute(list(list(list_j)), list(list_j = list_j))
    ),
    ddd
  )
  
  e <- do.call(call, call_arg_list, quote = TRUE)
  eval_env <- new.env(parent = enclos)
  eval_env[["dt"]] <- dt
  eval_result <- eval(e, envir = eval_env)
  eval_result <- eval_result[[1L]][[1L]]
  
  # compile sensible output data.table -----------------------------------------
  j_elems <- as.list(list_j)[-1]
  if (is.null(names(eval_result))) {
    data.table::setattr(eval_result, "names", rep("", length(eval_result)))
  }
  data.table::setattr(
    eval_result, 
    "names", 
    vapply(seq_along(eval_result), function(i) {
      name <- names(eval_result)[i]
      if (name == "") {
        name <- paste0(deparse(j_elems[[i]]), collapse = "")
      }
      name
    }, character(1))
  )
  
  if (data.table::is.data.table(eval_result[[1]])) {
    output <- eval_result[[1]]
  } else {
    output <- data.table::setDT(list(.___TMP = eval_result[[1]]))
    on.exit({output[, ".___TMP" := NULL]})
  }
  
  lapply(names(eval_result), function(col_nm) {
    if (inherits(eval_result[[col_nm]], "surv_dt")) {
      dt <- eval_result[[col_nm]]
      dt_col_nms <- attr(dt, "Surv")[["col_nms"]]
      data.table::set(
        x = output,
        j = dt_col_nms,
        value = dt
      )
    } else {
      data.table::set(
        x = output,
        j = col_nm,
        value = eval_result[[col_nm]]
      )
    }
    NULL
  })
    
  nse_eval_attrs <- as.environment(list(
    list_j = list_j,
    used_j = j,
    original_j = original_j
  ))
  if (inherits(j, "formula")) {
    j_elems <- as.list(j)
    has_lhs <- length(j_elems) == 3L
    if (has_lhs) {
      nse_eval_attrs[["formula_j_lhs"]] <- j_elems[[2]]
      nse_eval_attrs[["formula_j_rhs"]] <- j_elems[[3]]
    } else {
      nse_eval_attrs[["formula_j_lhs"]] <- NULL
      nse_eval_attrs[["formula_j_rhs"]] <- j_elems[[2]]
    }
  }
  lapply(eval_result, function(result_elem) {
    if (inherits(result_elem, "surv_dt")) {
      nse_eval_attrs[["Surv"]] <- attr(result_elem, "Surv")
    }
  })
  data.table::setattr(
    output,
    "nse_eval",
    as.list(nse_eval_attrs)
  )
  output
}




