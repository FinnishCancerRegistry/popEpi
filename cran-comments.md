
# popEpi CRAN upload, v.0.4.7

* R-oldrel = R version 3.5.3 (2019-03-11)
* R-release = R version 3.6.1 (2019-07-05)
* R-patched = R version 3.6.1 Patched 2019-09-14 r77195) 
* R-devel = R Under development (unstable) (2018-06-20 r74924)


## win builder checks

* R-oldrel --- OK
* R-release --- OK
* R-devel --- OK

## R-hub checks

Some checks could not be completed due to installation errors on R-hub.

### Windows Server 2008

* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit --- install error
* Windows Server 2008 R2 SP1, R-release, 32/64 bit --- OK
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit --- OK
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit --- OK

### Windows Server 2012

* Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental) --- OK

### macOS 10.11

*	macOS 10.11 El Capitan, R-release (experimental) --- OK

### Debian Linux

* Debian Linux, R-release, GCC --- OK
* Debian Linux, R-patched, GCC
* Debian Linux, R-devel, GCC --- OK
* Debian Linux, R-devel, GCC ASAN/UBSAN --- OK
* Debian Linux, R-devel, GCC, no long double --- OK
* Debian Linux, R-devel, clang, ISO-8859-15 locale

### Fedora Linux

* Fedora Linux, R-devel, clang, gfortran
* Fedora Linux, R-devel, GCC --- OK

### CentOS

* CentOS 6, stock R from EPEL (R version 3.5.2 (2018-12-20)) --- install error
* CentOS 6 with Redhat Developer Toolset, R from EPEL 
  (R version 3.5.2 (2018-12-20)) --- install error

### Ubuntu Linux

* Ubuntu Linux 16.04 LTS, R-release, GCC --- OK
* Ubuntu Linux 16.04 LTS, R-devel, GCC
* Ubuntu Linux 16.04 LTS, R-devel with rchk --- OK


### Oracle Solaris

* Oracle Solaris 10, x86, 32 bit, R-patched (experimental) --- install error


## Other checks:

* Travis CI: Ubuntu 16.04.6 LTS, R version 3.6.1 (2017-01-27) --- OK

* Appveyor CI: Windows Server 2012 R2 x64 (build 9600), 
R version 3.6.1 Patched (2019-09-23 r77211) --- OK

* Ubuntu Linux 19.04, R-release --- OK


