<!-- README.md is generated from README.Rmd. Please edit that file -->
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
             fot = 0:5, per = 1994:2000, pop.haz = popmort)
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
print(se)
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

st <- survtab(x)
#> event.values was NULL, so chose 0 as non-event value
head(st)
#>    surv.int  Tstart   Tstop   delta  pyrs n.start   d n.cens d.exp
#> 1:        1 0.00000 0.08333 0.08333 672.4    8227 297     25 25.64
#> 2:        2 0.08333 0.16670 0.08333 647.1    7905 215     44 23.63
#> 3:        3 0.16670 0.25000 0.08333 627.6    7646 205     36 22.19
#> 4:        4 0.25000 0.33330 0.08333 608.6    7405 165     42 21.19
#> 5:        5 0.33330 0.41670 0.08333 592.4    7198 152     35 20.26
#> 6:        6 0.41670 0.50000 0.08333 578.8    7011 114     26 19.62
#>    surv.obs.lo surv.obs surv.obs.hi SE.surv.obs r.e2.lo   r.e2 r.e2.hi
#> 1:      0.9596   0.9639      0.9677    0.002059  0.9626 0.9669  0.9707
#> 2:      0.9321   0.9375      0.9426    0.002673  0.9379 0.9434  0.9484
#> 3:      0.9060   0.9124      0.9183    0.003126  0.9143 0.9208  0.9267
#> 4:      0.8850   0.8920      0.8985    0.003436  0.8958 0.9028  0.9094
#> 5:      0.8657   0.8731      0.8802    0.003688  0.8787 0.8862  0.8934
#> 6:      0.8511   0.8589      0.8663    0.003861  0.8663 0.8743  0.8818
#>     SE.r.e2
#> 1: 0.002065
#> 2: 0.002690
#> 3: 0.003155
#> 4: 0.003477
#> 5: 0.003744
#> 6: 0.003930
tail(st)
#>    surv.int Tstart Tstop   delta  pyrs n.start  d n.cens d.exp surv.obs.lo
#> 1:       55  4.500 4.583 0.08333 269.1    3248 18     24 9.442      0.5170
#> 2:       56  4.583 4.667 0.08333 265.6    3206 19     26 9.402      0.5139
#> 3:       57  4.667 4.750 0.08333 262.0    3161 13     22 9.342      0.5117
#> 4:       58  4.750 4.833 0.08333 259.0    3126 17     16 9.324      0.5088
#> 5:       59  4.833 4.917 0.08333 256.2    3093 16     24 9.291      0.5061
#> 6:       60  4.917 5.000 0.08333 253.1    3053 17     16 9.222      0.5032
#>    surv.obs surv.obs.hi SE.surv.obs r.e2.lo   r.e2 r.e2.hi  SE.r.e2
#> 1:   0.5286      0.5400    0.005860  0.6020 0.6155  0.6287 0.006824
#> 2:   0.5254      0.5369    0.005869  0.6001 0.6137  0.6270 0.006855
#> 3:   0.5233      0.5347    0.005876  0.5993 0.6130  0.6263 0.006883
#> 4:   0.5204      0.5319    0.005884  0.5977 0.6115  0.6248 0.006914
#> 5:   0.5177      0.5292    0.005893  0.5964 0.6101  0.6236 0.006944
#> 6:   0.5148      0.5263    0.005901  0.5947 0.6086  0.6221 0.006976
```
