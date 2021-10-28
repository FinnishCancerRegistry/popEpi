
# popEpi CRAN upload, v.0.4.9

popEpi was archived because I missed your warning concerning R-devel changes
which caused an error in popEpi. Here's a new release which fixes that issue.

I also made the following changes requested by `julia.haider@wu.ac.at`: 

* added references to methods implemented in this package in 
  DESCRIPTION::Description
* added`\value` statements to all function docs
* removed examples for unexported functions
* replaced `\dontrun` in examples with `\donttest`
* when user settings are changed using `par()`, there is an immediate call to
  `on.exit` to return the settings to the pre-modified ones

I get one NOTE due to adding references to DESCRIPTION::Description: R CMD check
seems to think that peoples' names are potentially mis-spelled. There does not
seem to be a way to avoid this.

Below are a bunch of checks I ran using winbuilder and rhub.

* R-oldrel = R version 4.0.5 (2021-03-31)
* R-release = R version 4.1.1 (2021-08-10)
* R-patched = R version 4.1.1 Patched (2021-08-16 r80775)
* R-devel = R Under development (unstable) (2021-10-18 r81073)

## winbuilder checks

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



