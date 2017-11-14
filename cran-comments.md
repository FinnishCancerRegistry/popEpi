
# popEpi CRAN upload, v.0.4.4

## Checked manually on:
* Ubuntu 17.04, R 3.4.2
* Windows 8, R 3.4.2
* Windows Server 2008 SP1, R 3.4.2

## win-builder checks:
* R Under development (unstable) (2017-09-12 r73242)
* R 3.4.2

## R-hub: R-release = 3.4.2, R-patched = 2017-11-14 r73715, R-devel = 2017-11-14 r73724
* Ubuntu Linux 16.04 LTS, R-devel, GCC
* Ubuntu Linux 16.04 LTS, R-devel with rchk
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
* macOS 10.11 El Capitan, R-release (experimental)
* Debian Linux, R-release, GCC
* Debian Linux, R-devel, GCC ASAN/UBSAN
* Debian Linux, R-devel, GCC
* Debian Linux, R-patched, GCC
* CentOS 6, stock R from EPEL
* CentOS 6 with Redhat Developer Toolset, R from EPEL

## Other checks
* Travis CI, Ubuntu 14.04.5 LTS, R 3.4.2 (2017-01-27)
* Appveyor, Windows Server 2012 R2 x64 (build 9600), 3.4.2 Patched (2017-11-10 r73713)

## Checking results

Finished with `Status: OK` with no `WARNING`s nor `ERROR`s. 
Only got this `NOTE` from win-builder R 3.4.2:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Joonas Miettinen <joonas.miettinen@cancer.fi>'

Possibly mis-spelled words in DESCRIPTION:
  SIRs (10:71)
  SMRs (10:76)
```

The words are not mis-spelled.
