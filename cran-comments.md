
# popEpi CRAN upload, v.0.2.1

This is the second submission of popEpi to CRAN with fixes covering the issues discovered on the first try (v.0.2.0).

## Feedback from first upload

The checks were successful on first upload (v.0.2.0) for R 3.2.2 on http://win-builder.r-project.org/. However, the developer version returned the following `NOTE`s and an `ERROR`:

```
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
   'cran-comments.md'

* checking R code for possible problems ... NOTE

as.pe.Lexis: no visible global function definition for 'head'
as.pe.Lexis: no visible global function definition for 'tail'
as.pe.data.table: no visible global function definition for 'head'
as.pe.data.table: no visible global function definition for 'tail'
as.pe.list: no visible global function definition for 'head'
as.pe.list: no visible global function definition for 'tail'
cast_simple: no visible global function definition for 'as.formula'
cast_simple: no visible binding for global variable 'formula'
comp.st.conf.ints: no visible global function definition for 'qnorm'
lines.meansurv: no visible global function definition for 'matlines'
lines.survtab: no visible global function definition for 'matlines'
ltable: no visible global function definition for 'na.omit'
makeTempVarName: no visible global function definition for 'runif'
plot.meansurv: no visible global function definition for 'plot'
plot.meansurv: no visible global function definition for 'abline'
plot.sir: no visible global function definition for 'par'
plot.sir: no visible global function definition for 'plot'
plot.sir: no visible global function definition for 'axis'
plot.sir: no visible global function definition for 'points'
plot.sir: no visible global function definition for 'segments'
plot.sirspline: no visible global function definition for 'par'
plot.sirspline : draw.plot: no visible global function definition for
   'plot'
plot.sirspline : draw.plot: no visible global function definition for
   'lines'
plot.survtab: no visible global function definition for 'plot'
poisson.ci : f1: no visible global function definition for 'ppois'
poisson.ci : f2: no visible global function definition for 'ppois'
poisson.ci : f2: no visible global function definition for 'dpois'
poisson.ci: no visible global function definition for 'uniroot'
print.pe: no visible global function definition for 'palette'
relpois: no visible global function definition for 'terms.formula'
relpois: no visible global function definition for 'poisson'
relpois: no visible global function definition for 'glm'
rpcurve: no visible global function definition for 'model.matrix'
setDFpe: no visible global function definition for 'head'
setDFpe: no visible global function definition for 'tail'
sir_est : chi.p: no visible global function definition for 'pchisq'
sir_est : poisson.ci : f1: no visible global function definition for
   'ppois'
sir_est : poisson.ci : f2: no visible global function definition for
   'ppois'
sir_est : poisson.ci : f2: no visible global function definition for
   'dpois'
sir_est : poisson.ci: no visible global function definition for
   'uniroot'
sir_est: no visible global function definition for 'p.adjust'
sir_est: no visible global function definition for 'terms'
sir_est: no visible global function definition for 'as.formula'
sir_est: no visible global function definition for 'poisson'
sir_est: no visible global function definition for 'anova'
sir_est: no visible global function definition for 'confint'
sir_est: no visible global function definition for 'vcov'
sir_est: no visible global function definition for 'qnorm'
sir_est: no visible global function definition for 'na.omit'
sir_est: no visible global function definition for 'coef'
sir_spline : spline.knots: no visible global function definition for
   'glm'
sir_spline : spline.knots: no visible global function definition for
   'poisson'
sir_spline : spline.knots: no visible global function definition for
   'quantile'
sir_spline : spline.estimates.dep: no visible global function
   definition for 'as.formula'
sir_spline : spline.estimates.dep: no visible binding for global
   variable 'poisson'
sir_spline : spline.estimates.uni: no visible global function
   definition for 'glm'
sir_spline : spline.estimates.uni: no visible global function
   definition for 'poisson'
sir_spline : fit.fun: no visible global function definition for
   'as.formula'
sir_spline : fit.fun: no visible binding for global variable 'poisson'
sir_spline: no visible global function definition for 'anova'
sir_spline : lrt.uni: no visible global function definition for 'glm'
sir_spline : lrt.uni: no visible global function definition for
   'poisson'
sir_spline : lrt.uni: no visible global function definition for 'anova'
sir_table: no visible global function definition for 'na.omit'
survmean: no visible global function definition for 'na.omit'
survtab: no visible global function definition for 'na.omit'

Undefined global functions or variables:
   abline anova as.formula axis coef confint dpois formula glm head
   lines matlines model.matrix na.omit p.adjust palette par pchisq plot
   points poisson ppois qnorm quantile runif segments tail terms
   terms.formula uniroot vcov
   
Consider adding
   importFrom("grDevices", "palette")
   importFrom("graphics", "abline", "axis", "lines", "matlines", "par",
              "plot", "points", "segments")
   importFrom("stats", "anova", "as.formula", "coef", "confint", "dpois",
              "formula", "glm", "model.matrix", "na.omit", "p.adjust",
              "pchisq", "poisson", "ppois", "qnorm", "quantile", "runif",
              "terms", "terms.formula", "uniroot", "vcov")
   importFrom("utils", "head", "tail")
to your NAMESPACE.


** running tests for arch 'i386' ...ERROR Running the tests in 'tests/testthat.R' failed.
Last 13 lines of output:
   > library(testthat)
   > library(popEpi)
   Loading required package: data.table
   > options("popEpi.datatable" = TRUE)
   > cat("Starting checking with popEpi.datatable = TRUE \n")
   Starting checking with popEpi.datatable = TRUE
   > test_check("popEpi")
   Confidence intervals calculated from profile-likelihood.
   Confidence intervals calculated from profile-likelihood.
   dropped 16 rows where entry == exit
   dropped 16 rows where entry == exit
   Error: file.exists(file) is not TRUE
   Execution halted
```
## Fixes based on feedback
- improved `.Rbuildignore`
- added `grDevices`, `graphics`, `stats`, and `utils` to imports in `NAMESPACE`
- rewrote a test that was causing `file.exists` to fail in R developer version

## New checks

### Checked on: 
* http://win-builder.r-project.org/, R 3.2.2 & R-devel
* Windows Server 2012 R2, R 3.2.2

### Checking results

Finished with `Status: OK` with no `NOTE`s, `WARNING`s nor `ERROR`s using http://win-builder.r-project.org/ for both R-release (3.2.2) and R-devel.

Checking on Windows Server gave a `WARNING` about the missing `qpdf` programme and a `NOTE` about a new contributor in CRAN (R 3.2.2).
