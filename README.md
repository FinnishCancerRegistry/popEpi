
The purpose of popEpi is to facilitate computing certain epidemiological 
statistics whrer population data is used. Current main attractions:

## Splitting, merging population hazards, and aggregating

the 'lexpand' function allows users to split their subject-level follow-up data into 
sub-intervals along age, follow-up time and calendar time, 
merge corresponding population hazard information to those intervals, 
and to aggregate the resulting data by if needed.

## SIRs / SMRs

One can make use of the 'sir' function to estimate indirectly standardised incidence or
mortality ratios (SIRs/SMRs). The data can be aggregated by 'lexpand' or by other means.
While 'sir' is simple and flexible in itself, one may also use 'sirspline' to estimate
spline functions for the effect of e.g. age on SIRs.

## (Relative) survival

The 'survtab' function computes observed, relative and cause-specific survivals as well as 
cumulative incidence functions. Any of the supported survival time functions can be
easily standardised by age if necessary. 'survtab' currently needs splitted subject-level data,
but should work with aggregated data in the future.

