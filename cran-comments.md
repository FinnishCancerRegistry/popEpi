This is the first submission of popEpi to CRAN.

## Tested on:
* Windows 10, R 3.2.2
* Ubuntu 15.04, R 3.2.2

## R CMD check results

Using devtools 1.9.1 I received the following notable messages:

WARNING: 
'qpdf' is needed for checks on size reduction of PDFs

NOTE:
Maintainer: 'Joonas Miettinen <joonas.miettinen@cancer.fi>'
New submission

I cannot install qpdf on this computer. This passed without messages using devtools::win_build(). 
The latter I assume to be inconsequential.