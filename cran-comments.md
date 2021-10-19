
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

* R-oldrel, 32/64 bit --- OK
* R-release, 32/64 bit --- OK
* R-devel, 32/64 bit --- OK

### Apple Silicon (M1), macOS 11.6 Big Sur

* R-release --- OK

### macOS 10.13.6 High Sierra

* R-release, brew --- OK
* R-release, CRAN's setup --- OK

### Debian Linux

* Debian Linux, R-release, GCC --- OK
* Debian Linux, R-patched, GCC --- OK
* Debian Linux, R-devel, GCC --- OK
* Debian Linux, R-devel, GCC ASAN/UBSAN --- Rhub installation error
* Debian Linux, R-devel, GCC, no long double --- OK
* Debian Linux, R-devel, clang, ISO-8859-15 locale --- OK

### Fedora Linux

* Fedora Linux, R-devel, clang, gfortran --- OK
* Fedora Linux, R-devel, GCC --- OK

### CentOS 8

* stock R from EPEL (R version 4.0.4 (2021-02-15)) --- OK

### Ubuntu Linux 20.04 LTS

* R-release, GCC --- OK
* R-devel, GCC --- OK
* R-devel with rchk --- Rhub installation error

### Oracle Solaris 10, x86, 32 bit

* R release, Oracle Developer Studio 12.6 --- OK
* R release --- OK

## Other checks:

* Travis CI: Ubuntu 16.04.6 LTS, R version 3.6.1 (2017-01-27) --- OK

* Appveyor CI: Windows Server 2012 R2 x64 (build 9600), 
R version 4.1.1 Patched (2021-10-16 r81070) --- OK



