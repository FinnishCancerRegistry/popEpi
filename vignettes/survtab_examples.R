## ----sireprint-----------------------------------------------------------
library(popEpi)

sr <- copy(popEpi::sire)
head(sr)

## ----lexample1, eval=FALSE-----------------------------------------------
#  x <- lexpand(sr, birth=bi_date, entry=dg_date, exit=ex_date,
#               breaks = list(fot = seq(0,5, 1/12)), status = status, pophaz=popmort)

## ----lexample2, eval=FALSE-----------------------------------------------
#  x <- lexpand(sr, birth=bi_date, entry=dg_date, exit=ex_date,
#               fot = seq(0,5, 1/12), status = status, pophaz=popmort)

## ----lexample3, eval=FALSE-----------------------------------------------
#  BL <- list(fot = seq(0, 5, 1/12))
#  x <- lexpand(sr, birth=bi_date, entry=dg_date, exit=ex_date,
#               breaks = BL, status = status, pophaz=popmort)

## ----lex1----------------------------------------------------------------
## up to 5 years of follow-up time in month-long intervals
x <- lexpand(sr, birth=bi_date, entry=dg_date, exit=ex_date, 
             fot = seq(0,5, 1/12), status = status, pophaz=popmort)

## ----surv1---------------------------------------------------------------
st <- survtab(x)
head(st)
plot(st, y = "r.e2")

## ----lex2----------------------------------------------------------------
 x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
              status = status,
              fot=seq(0, 5, by = 1/12),
              per = c("2008-01-01", "2013-01-01"), 
              pophaz = popmort)

## ----surv2---------------------------------------------------------------
st <- survtab(x)
head(st)
plot(st, y = "r.e2")

## ----surv.as1------------------------------------------------------------
st.as.int <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf))

## ----surv.as2------------------------------------------------------------
## get internal weights from the data with age group breaks c(0, 45, 65, 85, Inf)
## this means getting numbers of cases by age group at diagnosis.
## below the method using data.table syntax.

iw <- x[!duplicated(lex.id), .N, keyby = cut(dg_age, c(0,45,65,75,Inf), right=FALSE)]
iw <- iw$N/sum(iw$N)

iw

st.as.hand <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf), 
                      agegr.w.weights = iw)
plot(st.as.int, y = "r.e2.as", conf.int = FALSE, lwd=4)
lines(st.as.hand, y = "r.e2.as", conf.int = FALSE, 
      col = "green", lty = 2, lwd=4)

## ----surv.as3------------------------------------------------------------
## with ICSS1 weights; see ?ICSS
st.as.icss1 <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf), 
                       agegr.w.weights = "ICSS1")
st.as.icss2 <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf), 
                       agegr.w.weights = "ICSS2")

plot(st.as.icss1, conf.int = FALSE, lwd = 4, col = "black")
lines(st.as.icss2, conf.int = FALSE, lwd = 4, col = "blue")

## ----surv.cause----------------------------------------------------------
st.as.cause <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf), 
                       surv.type="surv.cause")
st.as.rel   <- survtab(x, agegr.w.breaks = c(0,45,65,75, Inf), 
                       surv.type="surv.rel")

plot(st.as.rel, y = "r.e2.as", conf.int = FALSE, lwd=4)
lines(st.as.cause, y = "surv.obs1.as", conf.int = FALSE, 
      lwd=4, col="red", lty=2)

## ----CIF.as--------------------------------------------------------------
st.co <- survtab(x,  surv.type="cif.obs")
st.cr <- survtab(x,  surv.type="cif.rel")

## the two are very similar; here CIF.rel is NA after 3 years because
## d < d.exp; could be alleviated with larger survival intervals
plot(st.co, "CIF_1", conf.int = FALSE, lwd = 4)
lines(st.cr, "CIF.rel", conf.int = FALSE, lwd = 4, col = "red")

