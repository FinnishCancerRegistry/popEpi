
# popEpi CRAN upload, v.0.4.10

popEpi was archived because I missed your warning (thanks outlook) concerning 
unconditional use of Suggests packages. This release uses Suggests packages
conditionally in tests/vignettes/examples as per the Writing R Extensions
document. R CMD CHECK passes with _R_CHECK_DEPENDS_ONLY_=true.

I get one NOTE due to adding references to DESCRIPTION::Description: R CMD check
seems to think that peoples' names are potentially mis-spelled. There does not
seem to be a way to avoid this.

Below are a bunch of checks I ran using rhub / winbuilder.

* R-oldrel = 4.1.3 (2022-03-10)
* R-release = 4.2.1 (2022-06-23 ucrt)
* R-patched = 4.2.1 Patched (2022-08-14 r82721 ucrt)
* R-devel = Under development (unstable) (2022-08-15 r82721 ucrt)

## winbuilder checks

## R-hub checks

### Windows Server 2022

* R-patched, 32/64 bit --- OK
* R-release, 32/64 bit --- OK
* R-devel, 64 bit --- OK
* R-oldrel, 32/64 bit --- OK
* R-patched, 32/64 bit --- OK

### macOS 10.13.6 High Sierra

* R-release, brew --- OK
* R-release, CRAN's setup --- OK

## Other checks:

* Travis CI: Ubuntu 16.04.6 LTS, R version 3.6.1 (2017-01-27) --- OK

* Appveyor CI: Windows Server 2012 R2 x64 (build 9600), 
R version 4.1.1 Patched (2021-10-16 r81070) --- OK



