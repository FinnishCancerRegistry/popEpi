context("lexpand sanity checks")


test_that("lexpand arguments can be passed as symbol, expression, character name of variable, and symbol of a character variable", {
  skip_on_cran()
  sr <- copy(sire)[dg_date < ex_date, ]
  sr[, id := as.character(1:.N)]
  
  x <- lexpand(sr, fot = c(0, Inf), 
               birth = "bi_date", entry = dg_date, exit = "ex_date", 
               status = status %in% 1:2, id = "id")
  
  x2 <- lexpand(sr, fot = c(0, Inf), 
               birth = bi_date, entry = "dg_date", exit = ex_date, 
               status = status %in% 1:2, id = id)
  
  
  x3 <- lexpand(sr, fot = c(0, Inf), 
                birth = bi_date, entry = dg_date, exit = ex_date, 
                status = status %in% 1:2, id = id)
  
  expect_identical(x, x2)
  expect_identical(x, x3)
})



test_that("original total pyrs equals pyrs after splitting w/ large number of breaks", {
  skip_on_cran()
  x <- copy(sire)[dg_date < ex_date, ]
  x[, fot := get.yrs(ex_date, year.length = "actual") - get.yrs(dg_date, year.length = "actual")]
  totpyrs <- x[, sum(fot)]
  
  x <- lexpand(sire, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014)))
  setDT(x)
  totpyrs_splitted <- x[, sum(lex.dur)]
  
  expect_equal(totpyrs, totpyrs_splitted, tolerance = 1e-05)
})



test_that("pp not added to data if pp = FALSE but pop.haz is", {
  skip_on_cran()
  x <- lexpand(sire[dg_date < ex_date, ], 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot=0:5), pophaz=popmort, pp = FALSE)
  expect_equal(intersect(names(x), c("pp", "pop.haz")),  "pop.haz")
  expect_true(!any(is.na(x$pop.haz)))
})



test_that("lexpand produces the same results with internal/external dropping", {
  skip_on_cran()
  x <- lexpand(sire[dg_date < ex_date, ], 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot=0:5), pophaz=popmort, pp = TRUE, drop = TRUE)
  x2 <-lexpand(sire[dg_date < ex_date, ], 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot=0:5), pophaz=popmort, pp = TRUE, drop = FALSE)
  x2 <-popEpi:::intelliDrop(x2, breaks = list(fot=0:5), dropNegDur = TRUE)
  setDT(x)
  setDT(x2)
  popEpi:::doTestBarrage(dt1 = x, dt2 = x2, allScales = c("fot", "per", "age"))
})


test_that("lexpanding with aggre.type = 'unique' works", {
  skip_on_cran()
  ag1 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(fot = 0:5, age = seq(0,100, 5)), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date)
  setDT(ag1)
  ag1 <- ag1[, list(pyrs = sum(lex.dur), from0to1 = sum(lex.Xst == 1L)), 
           keyby = list(fot = cut(fot, 0:5, right = FALSE), age = cut(age, seq(0,100,5), right=FALSE))]
  ag2 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(fot = 0:5, age = seq(0,100, 5)), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date,
                 aggre = list(fot, age), aggre.type = "unique")
  setDT(ag2)
  expect_equal(ag1$pyrs, ag2$pyrs)
  expect_equal(ag1$from0to1, ag2$from0to1)
  
})

test_that("lexpanding with aggre.type = 'cross-product' works", {
  skip_on_cran()
  ag1 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(fot = 0:5, age = seq(0,100, 5)), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date)
  setDT(ag1)
  ag1[, `:=`(fot = as.integer(lower_bound(cut(fot, 0:5, right = FALSE))), 
             age = as.integer(lower_bound(cut(age, seq(0,100,5), right=FALSE))))]
  ceejay <- CJ(fot = 0:4, age = seq(0, 95, 5))
  setkey(ceejay, fot, age); setkey(ag1, fot, age)
  ag1 <- ag1[ceejay, list(pyrs = sum(lex.dur), from0to1 = sum(lex.Xst == 1L)), by = .EACHI]
  ag1[is.na(pyrs), pyrs := 0]
  ag1[is.na(from0to1), from0to1 := 0]
  
  ag2 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(fot = 0:5, age = seq(0,100, 5)), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date,
                 aggre = list(fot, age), aggre.type = "cross-product")
  setDT(ag2)
  expect_equal(ag1$pyrs, ag2$pyrs)
  expect_equal(ag1$from0to1, ag2$from0to1)
  
})


test_that("lexpanding and aggregating to years works", {
  ag1 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(per=2000:2014), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date)
  setDT(ag1)
  ag1[, `:=`(per = as.integer(cutLow(per, 2000:2014)))]
  ag1 <- ag1[, list(pyrs = sum(lex.dur), from0to1 = sum(lex.Xst == 1L)), keyby = per]
  
  ag2 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(per = 2000:2014), status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date,
                 aggre = list(per), aggre.type = "unique")
  setDT(ag2)
  ag3 <- lexpand(sire[dg_date < ex_date, ], 
                 breaks = list(per = 2000:2014, age = c(seq(0,100,5),Inf), fot = c(0:10, Inf)), 
                 status = status,
                 birth = bi_date, entry = dg_date, exit = ex_date,
                 aggre = list(y = per), aggre.type = "unique")
  setDT(ag3)
  expect_equal(ag1$pyrs, ag2$pyrs)
  expect_equal(ag1$from0to1, ag2$from0to1)
  expect_equal(ag1$pyrs, ag3$pyrs)
  expect_equal(ag1$from0to1, ag3$from0to1)
  
})

# Aggre check (to totpyrs) -----------------------------------------------------

test_that("lexpand aggre produces correct results", {
  skip_on_cran()
  x <- copy(sire)[dg_date < ex_date, ]
  x[, fot := get.yrs(ex_date, year.length = "actual") - get.yrs(dg_date, year.length = "actual")]
  totpyrs <- x[, sum(fot)]
  counts <- x[, .N, by = .(status)]
  
  x <- lexpand(sire[dg_date < ex_date, ], 
               birth = bi_date, entry = dg_date, exit = ex_date,
               breaks=list(fot=c(0,5,10,50,Inf), age=c(seq(0,85,5),Inf), per = 1993:2013), 
               status=status, aggre = list(fot, age, per))
  setDT(x)
  row_length <- x[,list( length(unique(age)), length(unique(per)), length(unique(fot)))]
  
  expect_equal( x[,sum(pyrs)], totpyrs, tolerance = 0.001)
  expect_equal( x[,sum(from0to0)], counts[1,N])
  expect_equal( x[,sum(from0to1)], counts[2,N])
  expect_equal( x[,sum(from0to2)], counts[3,N]) 
  #expect_equal( prod(row_length), x[,.N]) 
})

test_that('lexpand aggre: multistate column names correct', {
  skip_on_cran()
  x <- lexpand(sire[dg_date < ex_date, ], 
               birth = bi_date, entry = dg_date, exit = ex_date,
               breaks=list(fot=c(0,5,10,50,Inf), age=c(seq(0,85,5),Inf), per = 1993:2013), 
               status=status, aggre = list(fot, age, per))
  setDT(x)
  expect_equal( names(x)[c(5,6,7)], c('from0to0','from0to1','from0to2'))  
})


## TODO: pp weight checking ----------------------------------------------




# overlapping time lines --------------------------------------------------

test_that('lexpansion w/ overlapping = TRUE/FALSE produces double/undoubled pyrs', {
  skip_on_cran()
  
  sire2 <- copy(sire)[dg_date < ex_date, ]
  sire2[, dg_yrs := get.yrs(dg_date, "actual")]
  sire2[, ex_yrs := get.yrs(ex_date, "actual")]
  sire2[, bi_yrs := get.yrs(bi_date, "actual")]
  sire2[, id := 1:.N]
  sire2 <- sire2[rep(1:.N, each=2)]
  
  sire2[seq(2,.N, by=2), dg_yrs := (ex_yrs + dg_yrs)/2L]
  sire2[, dg_age := dg_yrs-bi_yrs]
  
  x <- lexpand(sire2, birth = "bi_yrs", entry = "bi_yrs", event="dg_yrs", 
               exit = "ex_yrs", status="status", entry.status = 0L, id = "id", overlapping = TRUE)
  setDT(x)
  expect_equal(x[, sum(lex.dur), keyby=lex.id]$V1, sire2[, sum(ex_yrs-bi_yrs), keyby=id]$V1)  
  
  x <- lexpand(sire2, birth = "bi_yrs", entry = "bi_yrs", event="dg_yrs", 
               exit = "ex_yrs", status="status", entry.status = 0L, id = "id", overlapping = FALSE)
  setDT(x)
  expect_equal(x[, sum(lex.dur), keyby=lex.id]$V1, sire2[!duplicated(id), sum(ex_yrs-bi_yrs), keyby=id]$V1)  
})


