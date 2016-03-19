context("aggre")

test_that("aggre leaves original data untouched", {
  
  x <- sire[1:100,]
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(x, birth = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  
  ## scramble order
  set.seed(1L)
  x <- x[sample(x = .N, size = .N, replace = FALSE)]
  setkeyv(x, NULL)
  
  setDT(x)
  forceLexisDT(x, breaks = BL, allScales = c("fot", "per", "age"), key = FALSE)
  
  xor <- copy(x)
  
  ag1 <- aggre(x, by = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  
  expect_identical(x, xor)
})

test_that("aggre works with by = NULL", {
  
  sr <- popEpi::sire[dg_date < ex_date,][1:1000,]
  
  BL <- list(fot= seq(0,20,1), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  
  ag1 <- aggre(x, by = NULL)
  expect_equal(as.numeric(ag1), c(9539.1903286174274, 1000, 373, 627))
  
})

test_that("aggre and lexpand produce the same results", {
  # skip_on_cran()
  
  sr <- popEpi::sire[dg_date < ex_date,][1:1000,]
  
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  if (!is.data.table(x)) setDF2DT(x)
  
  e <- quote(list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  v <- c("gender", "sex", "sex", "surv.int", "per", "agegr")
  
  forceLexisDT(x, breaks = BL, allScales = c("fot", "per", "age"))
  x2 <- aggre(x, by = e, verbose = FALSE)
  x3 <- aggre(x, by = e, type = "full", verbose = FALSE)
  x4 <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre.type = "non-empty",
                breaks=BL, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  x5 <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre.type = "cartesian",
                breaks=BL, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  
  x[, fot := popEpi:::cutLow(fot, BL$fot)]
  x[, age := popEpi:::cutLow(age, BL$age)]
  x[, per := popEpi:::cutLow(per, BL$per)]
  
  x <- x[, list(pyrs = sum(lex.dur), obs = sum(lex.Xst)), keyby = e]
  x <- x[pyrs > 0 & !is.na(pyrs)]
  
  if (!is.data.table(x2)) setDF2DT(x2)
  if (!is.data.table(x3)) setDF2DT(x3)
  if (!is.data.table(x4)) setDF2DT(x4)
  if (!is.data.table(x5)) setDF2DT(x5)
  
  setkeyv(x, v)
  setkeyv(x2, v)
  setkeyv(x3, v)
  setkeyv(x4, v)
  setkeyv(x5, v)
  
  expect_equal(x2$pyrs, x$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to1, x$obs, tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x3$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x3$from0to1), tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x4$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x4$from0to1), tolerance = 1e-05)
  
  expect_equal(x3$pyrs, x5$pyrs, tolerance = 1e-05)
  expect_equal(x3$from0to0, x5$from0to0, tolerance = 1e-05)
  expect_equal(sum(x3$from0to1), sum(x5$from0to1), tolerance = 1e-05)
  
  expect_equal(x2$pyrs, x4$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to0, x4$from0to0, tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x4$from0to1), tolerance = 1e-05)
})


test_that("aggre()'s by argument works flexibly", {
  # skip_on_cran()
  
  library(Epi)
  BL <- list(fot = 0:5, per = c(1995,2015))
  for (cond in c(FALSE, TRUE)) {
    x <- Lexis(data = sire[dg_date < ex_date,][1:500, ], entry = list(fot = 0, age = dg_age, per = get.yrs(dg_date)),
               exit = list(per = get.yrs(ex_date)), exit.status = status, 
               entry.status = 0)
    x <- splitMulti(x, breaks = BL)
    setDF(x)
    setattr(x, "class", c("Lexis", "data.frame"))
    x$agegr <- cut(x$dg_age, 2)
    if (cond) {
      forceLexisDT(x, breaks = BL, allScales = c("fot", "per", "age"))
      alloc.col(x)
    }
    
    a <- aggre(x, by = list(agegr = cut(dg_age, 2), sex, fot, per = per), type = "unique")
    b <- aggre(x, by = c("agegr", "sex", "fot", "per"), type = "unique")
    
    expect_equal(a, b)
    
    a <- aggre(x, by = cut(dg_age, 2), type = "unique")
    setnames(a, "cut", "agegr")
    attr(a, "aggre.meta")$by <- "agegr"
    b <- aggre(x, by = c("agegr"), type = "unique")
    c <- aggre(x, by = list(agegr = cut(dg_age, 2)), type = "unique")
    d<- aggre(x, by = agegr, type = "unique")
    
    expect_equal(a, b)
    expect_equal(b, c)
    expect_equal(c, d)
  }
  
  
})

test_that("subset argument works properly", {
  
  
  x <- sire[dg_date < ex_date, ][1:1000,]
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(x, birth = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  # setDT2DF(x)
  x2 <- x[x$dg_age <= 55L, ]
  
  setDT(x)
  setDT(x2)
  forceLexisDT(x, breaks = BL, allScales = c("fot", "per", "age"), key = FALSE)
  forceLexisDT(x2, breaks = BL, allScales = c("fot", "per", "age"), key = FALSE)
  
  ag <- quote(list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  ag1 <- aggre(x, by = ag, subset = dg_age <= 55L)
  ag2 <- aggre(x2, by = ag)
  
  ag3 <- aggre(x, by = ag, type = "full", subset = dg_age <= 55L)
  ag4 <- aggre(x2, by = ag, type = "full") 
  
  expect_identical(ag1, ag2)
  expect_identical(ag3, ag4)
  
})


test_that("at.risk & event columns are congruent", {
  
  x <- sire[dg_date < ex_date, ][1:1000,]
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(x, birth = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  
  ag <- aggre(x, by = list(sex, surv.int = fot, per, age))
  setkey(ag, sex, per, age, surv.int)
  ag[, ndiff := at.risk - c(at.risk[-1], NA), by = list(sex, per, age)]
  ag[!is.na(ndiff), events := from0to0 + from0to1]
  
  expect_equal(ag$ndiff, ag$events)
})

