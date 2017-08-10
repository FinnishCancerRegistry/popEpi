
# popEpi CRAN upload, v.0.4.2

## Checked manually on:
* Windows 10, R 3.4.1
* Ubuntu 17.04, R 3.4.1

## R-hub checks:
* macOS 10.11 El Capitan, R-release (experimental)
* macOS 10.9 Mavericks, R-oldrel (experimental)
* Debian Linux, R-devel, GCC
* Debian Linux, R-devel, GCC ASAN/UBSAN
* Debian Linux, R-release, GCC
*	Debian Linux, R-patched, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit
* CentOS 6 with Redhat Developer Toolset, R from EPEL
* CentOS 6, stock R from EPEL
* Ubuntu Linux 16.04 LTS, R-devel, GCC
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Ubuntu Linux 16.04 LTS, R-devel with rchk
*	Fedora Linux, R-devel, clang, gfortran
* Fedora Linux, R-devel, GCC

## win-builder checks:
* win-builder: R Under development (unstable) (2017-08-08 r73068)

## Checking results

Finished with `Status: OK` with no `WARNING`s nor `ERROR`s. Only got this `NOTE` from win-builder R-devel:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Joonas Miettinen <joonas.miettinen@cancer.fi>'

Possibly mis-spelled words in DESCRIPTION:
  SIRs (10:71)
  SMRs (10:76)
```

The words are not mis-spelled.

# popEpi CRAN upload, v.0.4.1

## Checked on R-hub:
* Fedora Linux, R-devel, clang, gfortran
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC (R 3.2.2)

## Other checks:
* http://win-builder.r-project.org/, R 3.3.2 & R-devel
* Windows 7, R 3.3.2
* Ubuntu 16.04, R 3.3.2
* Travis-CI.org: Ubuntu 12.04, R 3.3.2

## Checking results

Finished with `Status: OK` with no `WARNING`s nor `ERROR`s. Only got this `NOTE` from R-devel:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Joonas Miettinen <joonas.miettinen@cancer.fi>'
```

## Re-submission notes

In our previous submission, non-English words in the Description field were asked to be single-quoted, and abbreviations explained. The previous text was

> Enables computation of epidemiological statistics where population data such as population counts and hazards are used. Currently supported: excess hazard models, rates,  mean survival times, relative survival, and SIRs/SMRs, all of which can be easily adjusted for e.g. age. Fast splitting of Lexis objects and other computations achieved using data.table.

and the new text is 

> Enables computation of epidemiological statistics where e.g. 
    counts or mortality rates of the reference population are used. Currently 
    supported: excess hazard models, rates, mean survival times, relative 
    survival, as well as standardized incidence and mortality ratios (SIRs/SMRs), 
    all of which can be easily adjusted for e.g. age. 
    Fast splitting and aggregation of 'Lexis' objects (from package 'Epi') 
    and other computations achieved using 'data.table'. 
