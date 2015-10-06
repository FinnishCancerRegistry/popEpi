[![Build Status](https://travis-ci.org/WetRobot/popEpi.png?branch=master)](https://travis-ci.org/WetRobot/popEpi) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/popEpi)](http://cran.r-project.org/package=popEpi) [![codecov.io](http://codecov.io/github/WetRobot/popEpi/coverage.svg?branch=master)](http://codecov.io/github/WetRobot/popEpi?branch=master)

popEpi: Epidemiology with population data
=========================================

The purpose of popEpi is to facilitate computing certain epidemiological statistics where population data is used. Current main attractions:

Splitting, merging population hazards, and aggregating
------------------------------------------------------

the `lexpand` function allows users to split their subject-level follow-up data into sub-intervals along age, follow-up time and calendar time, merge corresponding population hazard information to those intervals, and to aggregate the resulting data if needed.

``` r
data(sire)
sr <- sire[1,]
print(sr)
#>    sex    bi_date    dg_date    ex_date status   dg_age
#> 1:   1 1952-05-27 1994-02-03 2012-12-31      0 41.68877
```

``` r
x <- lexpand(sr, birth = bi_date, entry = dg_date, exit = ex_date,
             status = status %in% 1:2, 
             fot = 0:5, per = 1994:2000)
print(x)
#>     lex.id      fot     per      age    lex.dur lex.Cst lex.Xst sex
#>  1:      1 0.000000 1994.09 41.68877 0.90958904       0       0   1
#>  2:      1 0.909589 1995.00 42.59836 0.09041096       0       0   1
#>  3:      1 1.000000 1995.09 42.68877 0.90958904       0       0   1
#>  4:      1 1.909589 1996.00 43.59836 0.09041096       0       0   1
#>  5:      1 2.000000 1996.09 43.68877 0.90958904       0       0   1
#>  6:      1 2.909589 1997.00 44.59836 0.09041096       0       0   1
#>  7:      1 3.000000 1997.09 44.68877 0.90958904       0       0   1
#>  8:      1 3.909589 1998.00 45.59836 0.09041096       0       0   1
#>  9:      1 4.000000 1998.09 45.68877 0.90958904       0       0   1
#> 10:      1 4.909589 1999.00 46.59836 0.09041096       0       0   1
#>        bi_date    dg_date    ex_date status   dg_age
#>  1: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  2: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  3: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  4: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  5: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  6: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  7: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  8: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#>  9: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
#> 10: 1952-05-27 1994-02-03 2012-12-31      0 41.68877
```

``` r
data(popmort)
x <- lexpand(sr, birth = bi_date, entry = dg_date, exit = ex_date,
             status = status %in% 1:2, 
             fot = 0:5, per = 1994:2000, pophaz = popmort)
print(x)
#>     lex.id      fot     per      age    lex.dur lex.Cst lex.Xst sex
#>  1:      1 0.000000 1994.09 41.68877 0.90958904       0       0   1
#>  2:      1 0.909589 1995.00 42.59836 0.09041096       0       0   1
#>  3:      1 1.000000 1995.09 42.68877 0.90958904       0       0   1
#>  4:      1 1.909589 1996.00 43.59836 0.09041096       0       0   1
#>  5:      1 2.000000 1996.09 43.68877 0.90958904       0       0   1
#>  6:      1 2.909589 1997.00 44.59836 0.09041096       0       0   1
#>  7:      1 3.000000 1997.09 44.68877 0.90958904       0       0   1
#>  8:      1 3.909589 1998.00 45.59836 0.09041096       0       0   1
#>  9:      1 4.000000 1998.09 45.68877 0.90958904       0       0   1
#> 10:      1 4.909589 1999.00 46.59836 0.09041096       0       0   1
#>        bi_date    dg_date    ex_date status   dg_age     pop.haz       pp
#>  1: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001170685 1.000651
#>  2: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001441038 1.000651
#>  3: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001200721 1.001856
#>  4: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001300846 1.001856
#>  5: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001400981 1.003207
#>  6: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.002142293 1.003207
#>  7: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.002202424 1.005067
#>  8: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.001771568 1.005067
#>  9: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.002222468 1.007277
#> 10: 1952-05-27 1994-02-03 2012-12-31      0 41.68877 0.002282603 1.007277
```

``` r
a <- lexpand(sr, birth = bi_date, entry = dg_date, exit = ex_date,
             status = status %in% 1:2,
             fot = 0:5, per = 1994:2000, aggre = list(fot, per))
print(a)
#>     fot  per       pyrs from0to0
#>  1:   0 1994 0.90958904        0
#>  2:   0 1995 0.09041096        0
#>  3:   1 1995 0.90958904        0
#>  4:   1 1996 0.09041096        0
#>  5:   2 1996 0.90958904        0
#>  6:   2 1997 0.09041096        0
#>  7:   3 1997 0.90958904        0
#>  8:   3 1998 0.09041096        0
#>  9:   4 1998 0.90958904        0
#> 10:   4 1999 0.09041096        1
```

SIRs / SMRs
-----------

One can make use of the `sir` function to estimate indirectly standardised incidence or mortality ratios (SIRs/SMRs). The data can be aggregated by `lexpand` or by other means. While `sir` is simple and flexible in itself, one may also use `sirspline` to fit spline functions for the effect of e.g. age as a continuous variable on SIRs.

``` r
data(popmort)
data(sire)
c <- lexpand( sire, status = status %in% 1:2, birth = bi_date, exit = ex_date, entry = dg_date,
              breaks = list(per = 1950:2013, age = 1:100, fot = c(0,10,20,Inf)), 
              aggre = list(fot, agegroup = age, year = per, sex) )
#> dropped 16 rows where entry == exit

se <- sir( coh.data = c, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
           ref.data = popmort, ref.rate = 'haz', 
           adjust = c('agegroup', 'year', 'sex'), print = 'fot')
#> Confidence intervals calculated from profile-likelihood.
se
#> SIR Standardized by:  agegroup year sex
#> 
#>  Total observed: 4559 
#>  Total expected: 1482.13 
#>  Total person-years: 
#> 39905.92 
#> 
#> Poisson modelled SIR: 
#>    fot observed expected     pyrs  sir 2.5 % 97.5 % p_value
#> 1:   0     4264  1214.54 34445.96 3.51  3.41   3.62   0.000
#> 2:  10      295   267.59  5459.96 1.10  0.98   1.23   0.094
#> 
#> Test for homogeneity p < 0.001
```

(Relative) survival
-------------------

The `survtab` function computes observed, relative and cause-specific survivals as well as cumulative incidence functions. Any of the supported survival time functions can be easily standardised by age if necessary. `survtab` currently needs splitted subject-level data, but may in the future work with aggregated data for computing some estimates.

``` r
x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
             status = status %in% 1:2,
             fot = seq(0, 5, 1/12), pophaz = popmort)
#> dropped 16 rows where entry == exit
#> 14 rows in expanded data had age values >= 101; assumed for these the same expected hazard as for people of age 100

st <- survtab(x, event.values = 1)
st[st$Tstop == 5, ]
#>    surv.int Tstart Tstop   delta  pyrs n.start  d n.cens d.exp surv.obs.lo
#> 1:       60  4.917     5 0.08333 253.1    3053 17     16 9.222      0.5032
#>    surv.obs surv.obs.hi SE.surv.obs r.e2.lo   r.e2 r.e2.hi  SE.r.e2
#> 1:   0.5148      0.5263    0.005901  0.5947 0.6086  0.6221 0.006976
```
