# everything here based on github::finnishcancerregistry/stabli
# ref 378095c36670cb0fa7c0a1a96d0421c6922eb69e

level_space_list_to_level_space_data_table <- function(
  x
) {
  stopifnot(inherits(x, "list"))
  x[vapply(x, is.null, logical(1L))] <- NULL
  dt <- do.call(data.table::CJ, lapply(seq_along(x), function(i) {
    if (data.table::is.data.table(x[[i]])) {
      seq_len(nrow(x[[i]]))
    } else {
      seq_along(x[[i]])
    }
  }))
  pos_col_nms <- sprintf("_____x[[%i]]_____", seq_along(x))
  data.table::setnames(dt, names(dt), pos_col_nms)
  lapply(seq_along(x), function(i) {
    pos_col_nm <- pos_col_nms[i]
    x_i_is_dt <- data.table::is.data.table(x[[i]])
    x_i <- x[[i]]
    add_col_nms <- if (x_i_is_dt) names(x_i) else names(x)[i]
    pos_vec <- dt[[pos_col_nm]]
    data.table::set(
      x = dt,
      j = add_col_nms,
      value = if (x_i_is_dt) x_i[pos_vec, ] else x_i[pos_vec]
    )
    data.table::set(
      x = dt,
      j = pos_col_nm,
      value = NULL
    )
    NULL
  })
  data.table::setkeyv(dt, names(dt))
  return(dt[])
}

assert_is_arg_by <- function(by, dataset) {
  stopifnot(
    inherits(by, c("data.table", "character", "list", "NULL"))
  )
  if (is.null(by)) {
    return(NULL)
  } else if (is.character(by)) {
    stratum_col_nms <- by
  } else if (inherits(by, "data.table")) {
    stratum_col_nms <- names(by)
  } else if (inherits(by, "list")) {
    stratum_col_nms <- unique(unlist(lapply(seq_along(by), function(i) {
      eval(substitute(stopifnot(
        inherits(by[[i]], c("character", "data.table", "NULL"))
      ), list(i = i)))
      if (is.null(by)) {
        return(NULL)
      } else if (is.character(by)) {
        return(by)
      } else {
        return(names(by))
      }
    })))
  }
  bad_col_nms <- setdiff(stratum_col_nms, names(dataset))
  if (length(bad_col_nms) > 0) {
    stop("Column names in `by` not in the dataset: ", deparse1(bad_col_nms))
  }
  return(invisible(NULL))
}

handle_arg_by <- function(
  by,
  dataset
) {
  # @codedoc_comment_block popEpi:::handle_arg_by
  # @param aggre_by `[data.table, character, list, NULL]` (default `NULL`)
  #
  # - `NULL`: No stratification of output.
  # - `data.table`: Compute produce results for each stratum defined in this
  #   table, e.g. `data.table::data.table(sex = 1:2)`. You may even use this to
  #   take a subset at the same time by doing
  #   e.g. `data.table::data.table(sex = 1L)` even if your `dt` contains data
  #   for both sexes.
  # - `character`: Compute results for each stratum defined by these columns
  #   in `dt`. E.g. `"sex"`.
  # - `list`: Each element must be either a `data.table`, a `character` vector,
  #   or `NULL`. These are combined to yield a big `data.table`. E.g.
  #   `list("sex", data.table::data.table(ag = 1:18))` leading to the same as
  #   `data.table::CJ(sex = 1:2, ag = 1:18)`.
  # @codedoc_comment_block popEpi:::handle_arg_by
  assert_is_arg_by(by, dataset)
  if (is.character(by)) {
    stratum_col_nms <- by
    nondup <- !duplicated(dataset, by = stratum_col_nms)
    by <- dataset[
      i = (nondup),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = stratum_col_nms
    ]
    data.table::setkeyv(by, stratum_col_nms)
  } else if (inherits(by, "list")) {
    by <- level_space_list_to_level_space_data_table(by)
  }
  bad_col_nms <- setdiff(names(by), names(dataset))
  if (length(bad_col_nms) > 0) {
    stop("Column names in `by` not in the dataset: ", deparse1(bad_col_nms))
  }
  return(by[])
}

handle_arg_subset <- function(
  arg_subset_nm = "subset",
  dataset_nm = "dt",
  output_type = c("logical", "integer",  "as-is")[1],
  eval_env = NULL,
  calling_env = NULL
) {
  stopifnot(
    inherits(eval_env, c("environment", "NULL")),

    inherits(calling_env, c("environment", "NULL"))
  )
  if (is.null(eval_env)) {
    eval_env <- parent.frame(1L)
  }
  if (is.null(calling_env)) {
    calling_env <- parent.frame(2L)
  }
  stopifnot(
    is.character(arg_subset_nm),
    length(arg_subset_nm) == 1,
    arg_subset_nm %in% ls(envir = eval_env),

    is.character(dataset_nm),
    length(dataset_nm) == 1,
    dataset_nm %in% ls(envir = eval_env),

    is.character(output_type),
    length(output_type) == 1,
    output_type %in% c("logical", "integer",  "as-is")
  )
  subset_expr <- local({
    subset_arg_string <- arg_subset_nm # e.g. "my_subset"
    subset_arg_symbol <- parse(text = subset_arg_string)[[1]] # e.g. `my_subset`
    substitute_subset_arg_symbol <- substitute(
      substitute(SUBSET_ARG_SYMBOL),
      list(SUBSET_ARG_SYMBOL = subset_arg_symbol)
    ) # e.g. substitute(my_subset)
    subset_expr <- eval(substitute_subset_arg_symbol, eval_env) # e.g. `a == 1`
    subset_expr
  })

  subset_value <- eval(
    subset_expr,
    envir = eval_env[[dataset_nm]],
    enclos = calling_env
  )
  if (!inherits(subset_value, c("NULL", "data.table", "integer", "logical"))) {
    stop("Subsetting argument `", arg_subset_nm, "` did not evaluate to ",
         "an object of class NULL, data.table, integer, nor logical. Instead ",
         "it had class ", deparse1(class(subset_value)))
  }
  if (output_type == "as-is") {
    return(subset_value)
  }
  if (inherits(subset_value, "data.table")) {
    subset_value <- eval_env[[dataset_nm]][
      i = subset_value,
      on = names(subset_value),
      which = TRUE
    ] # to integer
  }
  if (inherits(subset_value, "integer")) {
    subset_value <- switch(
      output_type,
      integer = subset_value,
      logical = local({
        out <- rep(FALSE, nrow(eval_env[[dataset_nm]]))
        out[subset_value] <- TRUE
        out
      })
    )
  } else if (inherits(subset_value, "logical")) {
    subset_value <- switch(
      output_type,
      integer = which(subset_value),
      logical = subset_value
    )
  } else if (inherits(subset_value, "NULL")) {
    subset_value <- switch(
      output_type,
      integer = seq_len(nrow(eval_env[[dataset_nm]])),
      logical = rep(TRUE, nrow(eval_env[[dataset_nm]]))
    )
  }
  return(subset_value)
}
