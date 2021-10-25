




#' @title `array`s, `data.frame`s and `ratetable`s
#' @description
#' Utilities to transform objects between `array`, `data.frame`, and
#' [survival::ratetable].
#' @param x `[data.frame, data.table, array, ratetable]` (mandatory, no default)
#' 
#' - `long_df_to_array`: a `data.frame`
#' - `long_df_to_ratetable`: a `data.frame`
#' - `long_dt_to_array`: a `data.table`
#' - `long_dt_to_ratetable`: a `data.table`
#' - `array_to_long_df`: an `array`
#' - `array_to_long_dt`: an `array`
#' - `array_to_ratetable`: an `array`
#' - `ratetable_to_array`: a [survival::ratetable]
#' - `ratetable_to_long_df`: a [survival::ratetable]
#' - `ratetable_to_long_dt`: a [survival::ratetable]
#' @name array_df_ratetable_utils
#' @examples
#' 
#' long_dt <- popEpi::popmort
#' arr <- long_df_to_array(long_dt, c("agegroup", "year", "sex"), "haz") 
#' rt <- array_to_ratetable(arr, dim.types = c(2L, 4L, 1L))
#' 
#' arr2 <- ratetable_to_array(rt)
#' long_df2 <- array_to_long_df(arr2)
#' 
#' identical(sort(long_dt[["haz"]]), sort(long_df2[["value"]]))
#' @return
#' 
#' - `long_df_to_array`: an `array`
#' - `long_df_to_ratetable`: a [survival::ratetable]
#' - `long_dt_to_array`: an `array`
#' - `long_dt_to_ratetable`: a [survival::ratetable]
#' - `array_to_long_df`: an `data.frame`
#' - `array_to_long_dt`: an `data.table`
#' - `array_to_ratetable`: a [survival::ratetable]
#' - `ratetable_to_array`: an `array`
#' - `ratetable_to_long_df`: a `data.frame`
#' - `ratetable_to_long_dt`: a `data.table`



#' @rdname array_df_ratetable_utils
#' @export
#' @param stratum.col.nms `[character]` (mandatory, no default)
#' 
#' a vector of column names in `x` by which values are stratified
#' 
#' @param value.col.nm `[character]` (mandatory, no default)
#' 
#' name of column in `x` containing values (these will be contents of the
#' array)
#' @details
#' - `long_df_to_array`: converts a long-format `data.frame` to an `array`
#'   with one or more dimensions
long_df_to_array <- function(x, stratum.col.nms, value.col.nm) {
  stopifnot(
    is.data.frame(x),
    
    is.character(stratum.col.nms),
    length(stratum.col.nms) >= 1,
    stratum.col.nms %in% names(x),
    !duplicated(stratum.col.nms),
    
    length(value.col.nm) == 1,
    value.col.nm %in% names(x)
  )
  
  dn <- lapply(stratum.col.nms, function(col_nm) {
    sort(unique(x[[col_nm]]))
  })
  names(dn) <- stratum.col.nms
  
  d <- vapply(dn, length, integer(1L))
  n_dims <- length(d)
  
  arr <- array(x[[value.col.nm]][0L], d)
  
  wh <- do.call(cbind, lapply(stratum.col.nms, function(col_nm) {
    match(x[[col_nm]], dn[[col_nm]])
  }))
  arr[wh] <- x[[value.col.nm]]
  
  dimnames(arr) <- dn
  arr
}

#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `long_df_to_ratetable`: calls `long_df_to_array` and then 
#'   `array_to_ratetable`
long_df_to_ratetable <- function(
  x, 
  stratum.col.nms, 
  value.col.nm, 
  dim.types, 
  cut.points = NULL
) {
  arr <- long_df_to_array(x = x, stratum.col.nms = stratum.col.nms, 
                          value.col.nm = value.col.nm)
  array_to_ratetable(x = arr, dim.types = dim.types, cut.points = cut.points)
}


#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `long_dt_to_array`: simply asserts that `x` is a `data.table` and 
#' calls `long_df_to_array`
#' @importFrom data.table is.data.table
long_dt_to_array <- function(x, stratum.col.nms, value.col.nm) {
  stopifnot(data.table::is.data.table(x))
  long_df_to_array(x, stratum.col.nms, value.col.nm)
}



#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `long_dt_to_ratetable`: calls `long_dt_to_array` and then 
#'   `array_to_ratetable`
long_dt_to_ratetable <- function(
  x, 
  stratum.col.nms, 
  value.col.nm, 
  dim.types, 
  cut.points = NULL
) {
  arr <- long_dt_to_array(x = x, stratum.col.nms = stratum.col.nms, 
                          value.col.nm = value.col.nm)
  array_to_ratetable(x = arr, dim.types = dim.types, cut.points = cut.points)
}

#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `array_to_long_df`: converts an array with one or more dimensions into
#'   a long-format `data.frame`; any [dimnames] are used to name and fill the 
#'   stratifying columns; for dimensions without a name, `".dX"` is used
#'   for stratifying column number `X`; for each `k`, if there are no contents
#'   in `dimnames(x)[[k]]`, the elements of `seq(dim(x)[k])` are used to fill 
#'   the corresponding stratifying column; the value column always has the name
#'   `"value"`
array_to_long_df <- function(x) {
  stopifnot(
    is.array(x)
  )
  
  d <- dim(x)
  
  dn <- dimnames(x)
  if (is.null(dn)) {
    dn <- vector("list", length(d))
  }
  dn[] <- lapply(seq_along(dn), function(k) {
    nm_vec <- dn[[k]]
    if (is.null(nm_vec)) {
      nm_vec <- seq(d[k])
    }
    nm_vec
  })
  if (length(setdiff(names(dn), "")) == 0L) {
    names(dn) <- rep("", length(dn))
  }
  names(dn) <- vapply(seq_along(dn), function(k) {
    nm <- names(dn)[k]
    if (nm == "") {
      nm <- paste0(".d", k)
    }
    nm
  }, character(1))
  
  df <- as.data.frame(which(array(TRUE, d), arr.ind = TRUE))
  names(df) <- names(dn)
  df[, ] <- lapply(seq_along(d), function(k) {
    dn[[k]][df[[k]]]
  })
  
  df[["value"]] <- as.vector(x)
  df
}


#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `array_to_long_dt`: calls `array_to_long_df` and converts result to a 
#'   `data.table` for convenience
#' @importFrom data.table setDT
array_to_long_dt <- function(x) {
  df <- array_to_long_df(x)
  data.table::setDT(df)
  df[]
}






#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `array_to_ratetable`: converts an array to a [survival::ratetable]
#' @param dim.types `[integer]` (mandatory, no default)
#' 
#' see `type` under **Details** in [survival::ratetable] 
#' @param cut.points `[NULL, list]` (optional, default `NULL`)
#' 
#' see `cutpoints` under **Details** in [survival::ratetable] 
#' 
#' - `NULL`: automatically set using `dimnames(x)` and `dim.types`
#' - `list`: one element for each dimensions of `x`
#' @importFrom data.table copy setattr
array_to_ratetable <- function(x, dim.types, cut.points = NULL) {
  stopifnot(
    is.array(x),
    length(dim.types) == length(dim(x)),
    dim.types %in% 1:4,
    
    is.null(cut.points) || inherits(cut.points, "list")
  )
  if (is.null(cut.points)) {
    cut.points <- lapply(seq_along(dim.types), function(k) {
      if (dim.types[k] == 1L) {
        return(NULL)
      }
      cp <- as.numeric(dimnames(x)[[k]])
      if (all(cp %% 1L == 0L)) {
        cp <- as.integer(cp)
      }
      cp
    })
  }
  
  x <- data.table::copy(x)
  data.table::setattr(x, "type", as.integer(dim.types))
  data.table::setattr(x, "cutpoints", cut.points)
  data.table::setattr(x, "class", "ratetable")
  x
}





#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `ratetable_to_array`: converts a [survival::ratetable] to an array
#' @importFrom data.table copy setattr
ratetable_to_array <- function(x) {
  stopifnot(
    inherits(x, "ratetable")
  )
  array(x, dim = dim(x), dimnames = dimnames(x))
}

#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `ratetable_to_long_df`: calls `ratetable_to_array` and then
#'   `array_to_long_df`
#' @importFrom data.table copy setattr
ratetable_to_long_df <- function(x) {
  array_to_long_df(ratetable_to_array(x))
}

#' @rdname array_df_ratetable_utils
#' @export
#' @details
#' - `ratetable_to_long_dt`: calls `ratetable_to_array` and then
#'   `array_to_long_dt`
#' @importFrom data.table copy setattr
ratetable_to_long_dt <- function(x) {
  array_to_long_dt(ratetable_to_array(x))
}













