
# popEpi CRAN upload, v.0.4.12

No changes in the package itself --- fixed a unit test that used
the output of `survival::summmary.survfit` which had improved slightly
in 3.6-4.

Below are a bunch of checks I ran using winbuilder / rhub.

I also ran revdepcheck and found no issues.

* R-oldrel = 4.3.3 (2024-02-29 ucrt)
* R-release = 4.4.0 (2024-04-24 ucrt)
* R-patched = 
* R-devel = 2024-05-07 r86527 ucrt

## winbuilder checks

* R-oldrel --- OK
* R-release --- OK
* R-devel --- OK
