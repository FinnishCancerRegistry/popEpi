context("Testing empty survival intervals in survtab")

test_that("removing consecutively bad surv.ints is logical w/ & w/out agegr.w.breaks", {
  
  sire2 <- copy(sire)[dg_date < ex_date, ]
  sire2[, agegr := cut(dg_age, c(0,45,60,Inf), right=FALSE, labels=FALSE)]
  
  ## consecutively bad surv.ints
  sire2 <- sire2[!(dg_age > 60 & as.integer(ex_date-dg_date)/365.25 > 5)]
  
  BL <- list(fot= seq(0,10,1/12), per=c(2008,2013))
  
  ## consecutively bad surv.ints -------------------------------------------------
  x <- lexpand(sire2, breaks=BL, status = status)
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  sta <-  survtab(x, surv.type="surv.obs", format=F, by.vars="agegr") ## should not give warning
  st <- survtab(x, surv.type="surv.obs", format=T, agegr.w.breaks=c(0,45,60,Inf))
  setDT(st)
  setDT(sta)
  
  
  expect_equal(sta[agegr==3 & Tstop>5, .N] ,  0L)
  expect_equal(st[is.na(surv.obs.as), .N] ,  60L)
  expect_equal(st[Tstop > 5 & is.na(surv.obs.as), .N] ,  60L)
})

## non-consecutively bad surv.ints ---------------------------------------------

test_that("messages & results by age group w/ consecutively & non-consecutively bad surv.ints are OK", {
  ## non-consecutively bad surv.ints (missing years 5-6)
  sire2 <- copy(sire)[dg_date < ex_date, ]
  sire2[, agegr := cut(dg_age, c(0,45,60,Inf), right=FALSE, labels=FALSE)]
  sire2 <- sire2[!(dg_age > 60 & as.integer(as.integer(ex_date-dg_date)/365.25) %in% 5:6)]
  BL <- list(fot= seq(0,10,1/12), per=c(2008,2013))
  x <- lexpand(sire2, breaks=BL, status = status)
  tf1 <- expression(
    st1 <-  survtab(x, surv.type="surv.obs", format=F, by.vars="agegr", 
                    subset=!(agegr==3 & fot >= 5 & fot < 7)) ## should not give warning
  )
  
  tf2 <- expression(
    st2 <- survtab(x, surv.type="surv.obs", format=T, agegr.w.breaks=c(0,45,60,Inf),
                   subset=!(agegr==3 & fot >= 5 & fot < 7))
  )
  
  ## note \\ needed before "(" or ")"
  msgs <- c("Some survival intervals were empty non-consecutively; 
              this will lead to NA cumulative estimates; please check 
              function output \\(for e.g. zero person-years in survival 
              intervals\\) and rethink function arguments",
            "Some cumulative surv.obs were zero or NA in the following strata:")
  expect_message(eval(tf1), msgs[1],ignore.case=TRUE)
  expect_message(eval(tf1), msgs[2],ignore.case=TRUE)
  
  setDT(st1)
  
  expect_equal(st1[agegr == 3 & is.na(surv.obs), .N], 60L)
  expect_equal(st1[is.na(surv.obs), .N], 60L)
  
  msgs <- c("Some survival intervals were empty non-consecutively 
              in at least one agegroup-by.vars combination; this 
              will lead to NA cumulative estimates; for a closer 
              look you may e.g. create your own agegroup variable
              and supply it to by.vars",
            "Some cumulative surv.obs were zero or NA in the following strata:")
  
  expect_message(eval(tf2), msgs[1])
  expect_message(eval(tf2), msgs[2])
  
  setDT(st2)
  expect_equal(st2[is.na(surv.obs.as), .N], 60L)
})





