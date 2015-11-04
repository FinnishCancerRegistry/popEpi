
checkBreaksList <- function(x, breaks = list(fot = 0:5)) {
  if (is.null(breaks)) stop("breaks is NULL")
  if (!is.list(breaks)) stop("breaks needs to be a list")
  if (!is.data.frame(x)) stop("x needs to be a data.frame")
  timeScales <- names(breaks)
  if (length(timeScales) != length(breaks)) stop("breaks needs to be a fully named list")
  
  bad_scales <- setdiff(timeScales, names(x))
  if (length(bad_scales) > 0) stop("at least one breaks list name wasn't a variable in data; bad names: ", paste0("'", bad_scales, "'", collapse = ", "))
}

globalVariables(c("lex.dur", "lex.Xst", "lex.Cst"))
intelliCrop <- function(x, breaks = list(fot = 0:5), allScales = NULL, cropStatuses = FALSE, tol = .Machine$double.eps^0.5) {
  
  checkBreaksList(x = x, breaks = breaks)
  if (!is.data.table(x)) stop("x needs to be a data.table")
  
  cropScales <- names(breaks)
  
  all_names_present(x, "lex.dur")
  
  mi <- lapply(breaks, min)
  ma <- lapply(breaks, max)
  
  if (cropStatuses) {
    ## wrapped in data.table to avoid conflicting variable names with x
    ## (maybe origDur might exist in x)
    durDT <- data.table(origDur = x$lex.dur)
  }
  
  for (k in seq_along(breaks)) {
    SC <- cropScales[k]
    mik <- mi[[SC]]
    mak <- ma[[SC]]
    
    delta <- pmax(mik, x[[SC]]) -x[[SC]] ## pos if e.g. per was 1994, but min break was 1995
    
    for (l in allScales) {
      set(x, j = l,  value = x[[l]] + delta)
    }
    rm(delta)
    
    maxDur <- mak - x[[SC]]
    set(x, j = "lex.dur", value = pmin(maxDur, x$lex.dur))
  }
  
  if (cropStatuses) {
    x[lex.dur < durDT$origDur, lex.Xst := lex.Cst]
    rm(durDT)
  }
  
  invisible(x)
}

harmonizeStatuses <- function(x, C = "lex.Cst", X = "lex.Xst") {
  
  clC <- class(x[[C]])
  clX <- class(x[[X]])
  tyC <- typeof(x[[C]])
  tyX <- typeof(x[[X]])
  cl <- c(clC, clX)
  
  if (tyC != tyX && clC != clX) {
    if (is.numeric(x[[C]]) && is.numeric(x[[X]])) {
      harmonizeNumeric(x = x, v1="lex.Cst", v2="lex.Xst")
      
    } else if (is.factor(x[[C]]) || is.factor(x[[X]])) {
      if (!is.factor(x[[C]])) set(x, j = C, value = as.factor(x[[C]]))
      if (!is.factor(x[[X]])) set(x, j = X, value = as.factor(x[[X]]))
      
    }
  }
  
  if (any(cl == "factor")) {
    harmonizeFactors(x = x,  v1="lex.Cst", v2="lex.Xst")
  }
  
}

harmonizeNumeric <- function(x, v1="lex.Cst", v2="lex.Xst") {
  ## assumes v1, v2 are numeric variable names in x  
  
  if (!is.numeric(x[[v1]]) || !is.numeric(x[[v2]])) {
    print(class(x[[v1]]))
    print(class(x[[v2]]))
    stop("v1 and/or v2 is/are not of class numeric")
  }
  
  if (!is.integer(x[[v1]])) set(x, j = v1, value = try2int(x[[v1]]))
  if (!is.integer(x[[v2]])) set(x, j = v2, value = try2int(x[[v2]]))
  
  if (typeof(x[[v1]]) != typeof(x[[v2]])) {
    
    if (is.double(x[[v1]]))  set(x, j = v1, value = as.double(x[[v1]]))
    if (is.double(x[[v2]]))  set(x, j = v2, value = as.double(x[[v2]]))
    
  }  
  
}

harmonizeFactors <- function(x, v1="lex.Cst", v2="lex.Xst") {
  ## assumes v1, v2 are factor names in x
  
  if (!is.factor(x[[v1]]) || !is.factor(x[[v2]])) {
    stop("v1 and/or v2 is/are not of class factor")
  }
  
  glab1 <- union(levels(x[[v1]]), levels(x[[v2]]))
  glab2 <- union(levels(x[[v2]]), levels(x[[v1]]))
  
  
  
  setattr(x[[v1]], "levels", glab1)
  setattr(x[[v2]], "levels", glab2)
  
}

intelliDrop <- function(x, breaks = list(fot = 0:5), dropNegDur = TRUE, check = FALSE, tol = .Machine$double.eps^0.5, roundDig = NULL)  {
  
  checkBreaksList(x = x, breaks = breaks)
  timeScales <- names(breaks)
  
  if (check) {
    if (!inherits(x, "Lexis")) stop("x needs to be a Lexis object")
    
    for (k in timeScales) {
      if (is.null(attr(x, "breaks")[[k]])) stop("x isnt a product of splitting it seems")
      if (any(!breaks[[k]] %in% attr(x, "breaks")[[k]])) {
        stop("given breaks not a subset of breaks used to split x")
      }
    }
  }
  
  ra <- lapply(breaks, range)
  ra <- lapply(ra, diff)
  ts <- names(sort(unlist(ra))) ## shortest first
  mi <- lapply(breaks, min)
  ma <- lapply(breaks, max)
  
  x <- data.table(x)
  if (dropNegDur) x <- x[lex.dur > 0L]
  
  for (k in ts) {
    mik <- mi[[k]]
    mak <- ma[[k]]
    if (!is.null(roundDig)) {
      
      x <- x[round(get(k) + lex.dur, roundDig) <=  round(mak + tol, roundDig), ]
      x <- x[round(get(k) + tol, roundDig) > round(mik, roundDig), ]
    } else {
      
      x <- x[get(k) + lex.dur <=  mak + tol, ]
      x <- x[get(k) + tol > mik, ]
    }
    
  }
  
  x[]
}


matchBreakTypes <- function(lex, breaks, timeScale, modify.lex = FALSE) {
  if (is.character(breaks)) {
    breaks <- as.IDate(breaks)
  }
  clb <- class(breaks)
  clb <- clb[length(clb)]
  cts <- class(lex[[timeScale]])
  cts <- cts[length(cts)]
  
  if (clb != cts) {
    if (is.Date(breaks) && !is.Date(lex[[timeScale]])) {
      breaks <- as.integer(breaks)
    } else if (is.integer(breaks) && is.double(lex[[timeScale]])) {
      breaks <- as.double(breaks)
    } else if (is.double(breaks) && is.integer(lex[[timeScale]])) {
      breaks <- try2int(breaks)
    }
    
  }
  
  if (modify.lex && clb != cts) {
    if (!is.Date(breaks) && is.Date(lex[[timeScale]])) {
      
      if (clb == "double") {
        set(lex, j = timeScale, value = as.double(lex[[timeScale]]))
      } else {
        set(lex, j = timeScale, value = as.integer(lex[[timeScale]]))
      }
      
    } else if (is.double(breaks) && is.integer(lex[[timeScale]])) {
      set(lex, j = timeScale, value = as.double(lex[[timeScale]]))
    }
    
  }
  breaks
}

protectFromDrop <- function(breaks, lower = FALSE) {
  if (is.Date(breaks)) {
    breaks <- c(breaks, as.IDate("3000-01-01"))
    if (lower) breaks <- c(as.IDate("1000-01-01"), breaks)
    
  } else if (is.integer(breaks))  {
    breaks <- c(breaks, 1e6L)
    if (lower) breaks <- c(-1e6L, breaks)
    
  } else if (is.double(breaks)) {
    breaks <- c(breaks, Inf)
    if (lower) breaks <- c(-Inf, breaks)
    
  } else {
    stop("breaks were not Date, integer or double")
  }
  breaks
}






setLexisDT <- function(data, entry, exit, entry.status, exit.status, id = NULL, select = NULL) {
  
  if (!is.data.table(data)) stop("not a data.table")
  if (inherits(data, "Lexis")) stop("already a Lexis object")
  
  if (!is.null(select) && !is.character(select)) stop("select was not a character vector of names")
  
  entry <- substitute(entry)
  exit <- substitute(exit)
  entry <- eval(entry, envir = data, enclos = parent.frame())
  exit <- eval(exit, envir = data, enclos = parent.frame())
  enNames <- names(entry)
  exNames <- names(exit)
  
  timeScales <- union(enNames, exNames)
  if (any(timeScales %in% names(data))) stop("at least one named time scales already present in data; original names mandatory")
  enNeeded <- setdiff(timeScales, enNames)
  enPresent <- setdiff(timeScales, enNeeded)
  durVar <- intersect(enNames, exNames)
  if (length(durVar) > 1) stop("you have more than 1 time scales in both entry and exit; only one mandatory")
  
  enVars <- paste0(enNames, "_en")
  exVars <- paste0(exNames, "_ex")
  setattr(entry, "names", enVars)
  setattr(exit, "names", exVars)
  
  l <- as.data.table(c(entry, exit))
  rm(entry, exit)
  
  ## duration
  exV <- paste0(durVar, "_ex")
  enV <- paste0(durVar, "_en")
  set(l, j = "lex.dur", value = l[[exV]] - l[[enV]])
  rm(exV, enV)
  
  ## time scale starting points
  if (length(enNeeded) > 0) {
    for (ts in enNeeded) {
      exV <- paste0(ts, "_ex")
      set(l, j = ts, value = l[[exV]] - l$lex.dur)
    }
  }
  setnames(l, paste0(enPresent, "_en"), enPresent)
  
  # no longer need time scale end points
  for (k in exVars) {
    set(l, j = k, value = NULL)
  }
  
  ## status definition
  data[, lex.Cst := entry.status]
  data[, lex.Xst := exit.status]
  
  harmonizeStatuses(data, C = "lex.Cst", X = "lex.Xst")
  
  ## all time scales etc. into data
  data[, names(l) := l]
  
  
  id <- substitute(id)
  id <- eval(id, envir = data, enclos = parent.frame())
  if (!is.null(id)) set(data, j = "lex.id", value = id)
  rm(id)
  
  if (!is.null(select)) {
    
    delVars <- setdiff(names(data), c(names(l), select))
    if (length(delVars) > 0) {
      l[, (delVars) := NULL]
    }
  }
  
  rm(l)
  lexVars <- c("lex.id", timeScales, "lex.dur", "lex.Cst", "lex.Xst")
  setcolorder(data, c(lexVars, setdiff(names(data), lexVars)))
  
  setattr(data, "time.scales", timeScales)
  setattr(data, "time.since", rep("", times = length(timeScales)))
  setattr(data, "class", c("Lexis", "data.table", "data.frame"))
  
  
}





doComparisonWithEpi <- function(lexDT=lexDT, lexDTdrop=lexDTdrop, lexDF=lexDF, breaks = BL) {
  BL <- NULL
  if (!is.list(breaks)) stop("breaks needs to be a list")
  requireNamespace("Epi")
  requireNamespace("testthat")
  
  allScales <- attr(lexDF, "time.scales")
  sc1 <- allScales[1]
  setDT(lexDT)
  setDT(lexDTdrop)
  setDT(lexDF)
  setkeyv(lexDT, c("lex.id", sc1))
  setkeyv(lexDTdrop, c("lex.id", sc1))
  setkeyv(lexDF, c("lex.id", sc1))
  
  #   test_that(paste0("attributes are the same with and without dropping and vs. splitLexis"), {
  testthat::expect_equal(attr(lexDT, "time.scales"), attr(lexDF, "time.scales"))
  testthat::expect_equal(attr(lexDT, "time.since"), attr(lexDF, "time.since"))
  
  testthat::expect_equal(attr(lexDTdrop, "time.scales"), attr(lexDF, "time.scales"))
  testthat::expect_equal(attr(lexDTdrop, "time.since"), attr(lexDF, "time.since"))
  #   })
  
  #   test_that("results agree without dropping", {
  doTestBarrage(dt1 = lexDT, dt2 = lexDF, allScales = allScales)
  #   })
  rm(lexDT)
  
  lexDF <- intelliDrop(x = lexDF, breaks = breaks)
  
  #   test_that("results agree with dropping", {
  doTestBarrage(dt1 = lexDTdrop, dt2 = lexDF, allScales = allScales)
  #   })
  
}

doTestBarrage <- function(dt1, dt2, allScales, testTimes = TRUE, testStatuses = TRUE) {
  requireNamespace("Epi")
  requireNamespace("testthat")
  
  #   test_that("lex.durs sum to same values", {
  testthat::expect_equal(sum(dt1$lex.dur), sum(dt2$lex.dur), check.attributes = FALSE)
  testthat::expect_equal(dt1[, sum(lex.dur), keyby = lex.id]$V1, dt2[, sum(lex.dur), keyby = lex.id]$V1, check.attributes = FALSE)
  #   })
  
  
  if (testTimes) {
    for (k in allScales) {
      
      #       test_that(paste0("time scale ", k, " is the same in both split sets"), {
      testthat::expect_equal(dt1[[k]], dt2[[k]], check.attributes = TRUE)
      #       })
      
    }
  }
  
  if (testStatuses) {
    #     test_that(paste0("entry and exit statuses have the same values"), {
    testthat::expect_equal(dt1$lex.Cst, dt2$lex.Cst, check.attributes = FALSE)
    testthat::expect_equal(dt1$lex.Xst, dt2$lex.Xst, check.attributes = FALSE)
    #     })
    
    #     test_that(paste0("statuses have same levels"), {
    testthat::expect_equal(levels(dt1$lex.Cst), levels(dt2$lex.Cst), check.attributes = FALSE)
    testthat::expect_equal(levels(dt1$lex.Xst), levels(dt2$lex.Xst), check.attributes = FALSE)
    #     })
    
    #     test_that(paste0("status class at least contains class of splitLexis status"), {
    testthat::expect_true(all(class(dt2$lex.Cst) %in% class(dt1$lex.Cst)))
    testthat::expect_true(all(class(dt2$lex.Xst) %in% class(dt1$lex.Xst)))
    #     })
  }
  
  invisible(NULL)
}

compareSLDTWithEpi <- function(data, breaks, timeScale) {
  requireNamespace("Epi")
  requireNamespace("testthat")
  
  if (!inherits(data, "Lexis")) stop("data gotta be a Lexis object broseph")
  
  lexDT <- splitLexisDT(data, breaks = breaks, timeScale = timeScale, merge = TRUE, drop = FALSE)
  lexDTdrop <- splitLexisDT(data, breaks = breaks, timeScale = timeScale, merge = TRUE, drop = TRUE)
  lexDF <- splitLexis(data, breaks = breaks, time.scale = timeScale) ## without dropping
  
  BL <- list(breaks)
  setattr(BL, "names", timeScale)
  
  doComparisonWithEpi(lexDT = lexDT, lexDTdrop = lexDTdrop, lexDF = lexDF, breaks = BL)
  
  invisible(NULL)
}

splitMultiEpi <- function(data, breaks = list(fot = 0:5), drop) {
  
  for (k in names(breaks)) {
    data <- splitLexis(data, breaks = breaks[[k]], time.scale = k)
  }
  
  if (drop) data <- intelliDrop(data, breaks = breaks)
  setDT(data)
  
  data
}

compareSMWithEpi <- function(data, breaks = list(fot=0:5)) {
  requireNamespace("Epi")
  requireNamespace("testthat")
  
  lexDT <- splitMulti(data, breaks = breaks, merge = TRUE, drop = FALSE)
  lexDTdrop <- splitMulti(data, breaks = breaks, merge = TRUE, drop = TRUE)
  lexDF <- splitMultiEpi(data, breaks = breaks, drop = FALSE)
  
  doComparisonWithEpi(lexDT=lexDT, lexDTdrop = lexDTdrop, lexDF=lexDF, breaks = breaks)
  
  invisible(NULL)
}
