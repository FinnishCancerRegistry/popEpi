# Changes in 0.4.9

-   `popEpi::aggre` bugfix: `aggre` now correctly infers which
    stratifying variables use Lexis time scales when output is cartesian
    and argument `by` is of type character.

# Changes in 0.4.8

-   small internal fixes due to upcoming new package survival release
-   popEpi now includes a wrapper for survival::Surv, so you don’t need
    to do library(“survival”) when using Surv() in formulae (e.g. in
    survtab)

# Changes in 0.4.7

-   implemented small internal changes due to upcoming R 3.6.0

# Changes in 0.4.6

-   implemented small internal changes due to upcoming data.table
    version

# Changes in 0.4.5

-   fixed errors arising from new data.table version

# Changes in 0.4.4

-   splitLexisDT/splitMulti bug fix: splitting along multiple time
    scales *sometimes* produced duplicate transitions (e.g. alive ->
    dead in the last two rows). see
    <https://github.com/WetRobot/popEpi/issues/138> for details.
-   splitLexisDT/splitMulti now retain time.since attribute; this
    attribute plays a role in cutLexis
-   known issue: splitLexisDT/splitMulti not guaranteed to work
    identically to splitLexis from Epi when there are NA values in the
    time scale one is splitting along.

# Changes in 0.4.3

-   survtab adjusting was broken with older versions of data.table
    (tested 1.9.6); therefore popEpi now requires the newest version of
    data.table!

# Changes in 0.4.2

-   **`survtab()` bug fix: standard errors were mis-specified for
    adjusted curves, e.g. age-adjusted Ederer II estimates. This
    resulted in too wide confidence intervals! SEE HERE FOR EXAMPLE:
    [#135](https://github.com/WetRobot/popEpi/issues/135)**. The
    standard errors and confidence intervals of non-adjusted curves have
    always been correct.
-   `survtab()` bug fix: confidence level was always 95 % regardless of
    `conf.level` [#134](https://github.com/WetRobot/popEpi/issues/134)

# Changes in 0.4.1

-   `lexpand()` bug fixed (#120): observations were dropped if their
    entry by age was smaller than the smallest age value, though entry
    at exit is correct and used now.
-   `sir()` rewrite (#118, #122). New more consistent output, updates on
    plotting and minor changes in arguments. Introduce very simple
    `coef()` and `confint()` methods for sir class.
-   new functions in sir family: `sir_ag()`, `sir_lex()` and `sir_exp()`
    for extracting SMRs from `aggre` and `Lexis` objects.
-   fixed issue in internal test brought by pkg survival version 2.39.5;
    no changes in functions were needed (#125)
-   robustified `aggre()`; there were issues with Epi pkg dev version
    which are definitely avoided (#119)

# Changes in 0.4.0

-   removed previously deprecated shift.var (#35)
-   popEpi no longer depends on package data.table but imports it - this
    means the user will have to do library(data.table) separately to
    make data.table’s functions become usable. Formerly popEpi
    effectively did library(data.table) when you did library(popEpi).
-   summary.survtab: args t and q behaviour changed
-   survtab: internal weights now based on counts of subjects in
    follow-up at the start of follow-up (used to be sum of counts/pyrs
    over all of follow-up)
-   new functions: `rate_ratio()`, `sir_ratio()`
-   small internal changes in preparation for data.table 1.9.8

# Changes in 0.3.1

This is a hotfix. survtab() was causing warnings in certain situations,
which this update fixes. Also fixed plotting survtab objects so that
multiple strata are plotted correctly when one or more curves end before
the longest one as well other small fixes: See Github issues #89, #90,
#91, and #92.

# Changes in 0.3.0

## Adjusting

Direct adjusting (computing weighted averages of estimates) has been
generalized. Functions such as `survtab` and `survmean` allow for using
`adjust()` mini function within formulas, or a separate `adjust`
argument. Weights are passed separately. See the examples in the next
chapter. See also `?direct_adjusting`.

## Estimating functions of survival time

The `survtab` function computes observed, net/relative and
cause-specific survivals as well as cumulative incidence functions for
`Lexis` data. Any of the supported survival time functions can be easily
adjusted by any number of categorical variables if needed.

One can also use `survtab_ag` for aggregated data. This means the data
does not have to be on the subject-level to compute survival time
function estimates.

``` r
## prep data
data(sibr)
sire$cancer <- "rectal"
sibr$cancer <- "breast"
sr <- rbind(sire, sibr)

sr$cancer <- factor(sr$cancer)
sr <- sr[sr$dg_date < sr$ex_date, ]

sr$status <- factor(sr$status, levels = 0:2, 
                    labels = c("alive", "canD", "othD"))

## create Lexis object
library(Epi)
x <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)), 
           exit = list(CAL = get.yrs(ex_date)), 
           data = sr,
           exit.status = status)
#> NOTE: entry.status has been set to "alive" for all.

## population hazards file - see ?pophaz for general instructions
data(popmort)
pm <- data.frame(popmort)
names(pm) <- c("sex", "CAL", "AGE", "haz")

## simple usage - uses lex.Xst as status variable
st <- survtab(FUT ~ cancer, data = x,
              breaks = list(FUT = seq(0, 5, 1/12)),
              surv.type = "surv.rel", pophaz = pm)

## more explicit usage
st <- survtab(Surv(FUT, event = lex.Xst) ~ cancer, data = x,
              breaks = list(FUT = seq(0, 5, 1/12)),
              surv.type = "surv.rel", pophaz = pm)


## adjusting
x$agegr <- cut(x$dg_age, c(0,55,65,75,Inf))
w <- as.numeric(table(x$agegr))
st <- survtab(Surv(FUT, event = lex.Xst) ~ cancer + adjust(agegr), 
              data = x,
              breaks = list(FUT = seq(0, 5, 1/12)),
              surv.type = "surv.rel", 
              pophaz = pm, weights = w)
```

## Rates

The new `rate` function enables easy calculation of e.g. standardized
incidence rates:

``` r
## dummy data

a <- merge(0:1, 1:18)
names(a) <- c("sex", "agegroup")
set.seed(1)
a$obs <- rbinom(nrow(a), 100, 0.5)
set.seed(1)
a$pyrs <- rbinom(nrow(a), 1e4, 0.75)

## so called "world" standard rates (weighted to hypothetical world pop in 2000)
r <- rate(data = a, obs = obs, pyrs = pyrs, print = sex, 
          adjust = agegroup, weights = 'world_2000_18of5')
#> Warning in pyrJjCscXlsrH * pyrJjCscXlsrH: NAs produced by integer overflow

#> Warning in pyrJjCscXlsrH * pyrJjCscXlsrH: NAs produced by integer overflow
```

| sex | obs |   pyrs |  rate.adj | SE.rate.adj | rate.adj.lo | rate.adj.hi |      rate | SE.rate |   rate.lo |   rate.hi |
|----:|----:|-------:|----------:|------------:|------------:|------------:|----------:|--------:|----------:|----------:|
|   0 | 933 | 134986 | 0.0069947 |   0.0002541 |   0.0065140 |   0.0075108 | 0.0069118 |      NA | 0.0064822 | 0.0073699 |
|   1 | 875 | 134849 | 0.0064453 |   0.0002429 |   0.0059865 |   0.0069394 | 0.0064887 |      NA | 0.0060727 | 0.0069332 |
