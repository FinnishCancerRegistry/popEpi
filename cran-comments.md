
# popEpi CRAN upload, v.0.4.8

popEpi was archived because I missed your warning concerning R-devel changes
which caused an error in popEpi. Here's a new release which fixes that issue.
Below are a bunch of checks I ran using winbuilder and rhub.

* R-oldrel = R version 4.0.5 (2021-03-31)
* R-release = R version 4.1.1 (2021-08-10)
* R-patched = R version 4.0.5 (2021-03-31)
* R-devel = R Under development (unstable) (2021-10-14 r81057)

## win builder checks

* R-oldrel --- OK
* R-release --- OK
* R-devel --- OK

## R-hub checks

### Windows Server 2008 R2 SP1

* R-devel, 32/64 bit --- OK
* R-release, 32/64 bit --- OK
* R-patched, 32/64 bit --- OK
* R-oldrel, 32/64 bit --- OK

### Oracle Solaris 10, x86, 32 bit

* R-release --- OK

### Fedora Linux

* R-devel, GCC --- OK
* R-devel, clang, gfortran --- OK

### Debian Linux

* R-devel, GCC --- OK
* R-devel, clang, ISO-8859-15 locale --- OK
* R-release, GCC --- OK
* R-devel, GCC, no long double --- OK
* R-patched, GCC --- OK
* R-devel, GCC ASAN/UBSAN --- rhub preparation error

### Ubuntu Linux 20.04.1 LTS

* R-devel with rchk --- rhub preparation error
* R-devel, GCC --- OK
* R-release, GCC --- OK

### CentOS 8

* stock R from EPEL (R version 4.0.4 (2021-02-15)) --- OK

### macOS 10.13.6 High Sierra

* R-release, brew --- OK

### Apple Silicon (M1), macOS 11.6 Big Sur

* R-release --- OK
* R-release, CRAN's setup --- OK

## Other checks:

* Travis CI: Ubuntu 16.04.6 LTS, R version 3.6.1 (2017-01-27) --- OK

* Appveyor CI: Windows Server 2012 R2 x64 (build 9600), 
R version 4.1.1 Patched (2021-10-16 r81070) --- OK



