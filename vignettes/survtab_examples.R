## ----pkgs, eval = TRUE, echo = TRUE, message = FALSE---------------------
library(popEpi)
library(Epi)
library(survival)

## ------------------------------------------------------------------------

data(sire)
## NOTE: recommended to use factor status variable
x <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)), 
           exit = list(CAL = get.yrs(ex_date)), 
           data = sire[sire$dg_date < sire$ex_date, ],
           exit.status = factor(status, levels = 0:2, 
                                labels = c("alive", "canD", "othD")), 
           merge = TRUE)

## pretend some are male
set.seed(1L)
x$sex <- rbinom(nrow(x), 1, 0.5)

## observed survival - explicit method
st <- survtab(Surv(time = FUT, event = lex.Xst) ~ sex, data = x, 
              surv.type = "surv.obs",
              breaks = list(FUT = seq(0, 5, 1/12)))

## observed survival - easy method (assumes lex.Xst in x is the status variable)
st <- survtab(FUT ~ sex, data = x, 
              surv.type = "surv.obs",
              breaks = list(FUT = seq(0, 5, 1/12)))

## printing gives the used settings and 
## estimates at the middle and end of the estimated
## curves; more information available using summary()
st


## ------------------------------------------------------------------------
plot(st, col = c("blue", "red"))

## ----popmort-------------------------------------------------------------
data(popmort)
pm <- data.frame(popmort)
names(pm) <- c("sex", "CAL", "AGE", "haz")
head(pm)

