
# popEpi CRAN upload, v.0.4.5

## win-builder checks:
* R Under development (unstable) (2018-05-13 r74720)
* R version 3.5.0 (2018-04-23)

## R-hub checks: R-oldrel = R version 3.3.3 (2017-03-06); R-patched = 3.5.0 Patched (2018-05-12 r74712)
* Debian Linux, R-patched, GCC
* macOS 10.9 Mavericks, R-oldrel (experimental)
* Ubuntu Linux 16.04 LTS, R-devel with rchk
* Ubuntu Linux 16.04 LTS, R-devel, GCC

## Other checks:
* Travis CI, Ubuntu 14.04.5 LTS, R 3.5.0
* Appveyor, Windows Server 2012 R2 x64 (build 9600), R version 3.5.0 Patched (2018-05-13 r74720)
* Windows 8, R 3.4.4
* Windows Server 2008 SP1, R 3.4.2
* Ubuntu 18.04 LTS, R 3.4.4

## Checking results

Winbuilder R Under development (unstable) (2018-05-13 r74720) raised this error:

```
** running examples for arch 'i386' ... ERROR
Running examples in 'popEpi-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: sir
> ### Title: Calculate SIR or SMR
> ### Aliases: sir
> 
> ### ** Examples
> 
> data(popmort)
> data(sire)
> c <- lexpand( sire, status = status, birth = bi_date, exit = ex_date, entry = dg_date,
+               breaks = list(per = 1950:2013, age = 1:100, fot = c(0,10,20,Inf)), 
+               aggre = list(fot, agegroup = age, year = per, sex) )
dropped 16 rows where entry == exit
Error: cannot allocate vector of size 6.3 Mb
Execution halted
```

Is it necessary to refactor examples until it passes? 6.3 Mb does not seem like
a lot.
