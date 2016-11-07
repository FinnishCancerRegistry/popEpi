
# popEpi CRAN upload, v.0.4.1

## Checked on: 
* http://win-builder.r-project.org/, R 3.3.2 & R-devel
* Windows 7, R 3.3.2
* Ubuntu 16.04, R 3.3.2
* Travis-CI.org: Ubuntu 12.04, R 3.3.2

## Checking results

Finished with `Status: OK` with no `WARNING`s nor `ERROR`s. Got  `NOTE` about maintainer email for CRAN maintainers, and this `NOTE` from R-devel:

```
Possibly mis-spelled words in DESCRIPTION:
 Epi (7:356)
 SIRs (7:233)
 SMRs (7:238)
```

The words are not mis-spelled.

## Notes to CRAN maintainer

In our previous submission, the description text was asked to be made more specific. The old text was

> Enables computation of various epidemiological statistics where the use of population data such as population counts and hazards is required. Relatively high computation speed achieved by using data.table.

and new text is 

> Enables computation of epidemiological statistics where population data such as population counts and hazards are used. Currently supported: excess hazard models, rates,  mean survival times, relative survival, and SIRs/SMRs, all of which can be easily adjusted for e.g. age. Fast splitting of Lexis objects and other computations achieved using data.table.

