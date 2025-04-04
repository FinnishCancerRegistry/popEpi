% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ltable.R
\name{ltable}
\alias{ltable}
\alias{expr.by.cj}
\title{Tabulate Counts and Other Functions by Multiple Variables into a
Long-Format Table}
\usage{
ltable(
  data,
  by.vars = NULL,
  expr = list(obs = .N),
  subset = NULL,
  use.levels = TRUE,
  na.rm = FALSE,
  robust = TRUE
)

expr.by.cj(
  data,
  by.vars = NULL,
  expr = list(obs = .N),
  subset = NULL,
  use.levels = FALSE,
  na.rm = FALSE,
  robust = FALSE,
  .SDcols = NULL,
  enclos = parent.frame(1L),
  ...
)
}
\arguments{
\item{data}{a \code{data.table}/\code{data.frame}}

\item{by.vars}{names of variables that are used for categorization,
as a character vector, e.g. \code{c('sex','agegroup')}}

\item{expr}{object or a list of objects where each object is a function
of a variable (see: details)}

\item{subset}{a logical condition; data is limited accordingly before
evaluating \code{expr} - but the result of \code{expr} is also
returned as \code{NA} for levels not existing in the subset. See Examples.}

\item{use.levels}{logical; if \code{TRUE}, uses factor levels of given
variables if present;  if you want e.g. counts for levels
that actually have zero observations but are levels in a factor variable,
use this}

\item{na.rm}{logical; if \code{TRUE}, drops rows in table that have
\code{NA} as values in any of \code{by.vars} columns}

\item{robust}{logical; if \code{TRUE}, runs the output data's
\code{by.vars} columns through \code{robust_values} before outputting}

\item{.SDcols}{advanced; a character vector of column names
passed to inside the data.table's brackets
\code{DT[, , ...]}; see \verb{[data.table::data.table]}; if \code{NULL},
uses all appropriate columns. See Examples for usage.}

\item{enclos}{advanced; an environment; the enclosing
environment of the data.}

\item{...}{advanced; other arguments passed to inside the
data.table's brackets \code{DT[, , ...]}; see \verb{[data.table::data.table]}}
}
\value{
A \code{data.table} of statistics (e.g. counts) stratified by the columns defined
in \code{by.vars}.
}
\description{
\code{ltable} makes use of \code{data.table}
capabilities to tabulate frequencies or
arbitrary functions of given variables into a long format
\code{data.table}/\code{data.frame}. \code{expr.by.cj} is the
equivalent for more advanced users.
}
\details{
Returns \code{expr} for each unique combination of given \code{by.vars}.

By default makes use of any and all \verb{[levels]} present for
each variable in  \code{by.vars}. This is useful,
because even if a subset of the data does not contain observations
for e.g. a specific age group, those age groups are
nevertheless presented in the resulting table; e.g. with the default
\code{expr = list(obs = .N)} all age group levels
are represented by a row and can have  \code{obs = 0}.

The function differs from the
vanilla \verb{[table]} by giving a long format table of values
regardless of the number of \code{by.vars} given.
Make use of e.g. \verb{[cast_simple]} if data needs to be
presented in a wide format (e.g. a two-way table).

The rows of the long-format table are effectively Cartesian products
of the levels of each variable in  \code{by.vars},
e.g. with  \code{by.vars = c("sex", "area")} all levels of
\code{area} are repeated for both levels of  \code{sex}
in the table.

The \code{expr} allows the user to apply any function(s) on all
levels defined by  \code{by.vars}. Here are some examples:
\itemize{
\item .N or list(.N) is a function used inside a \code{data.table} to
calculate counts in each group
\item list(obs = .N), same as above but user assigned variable name
\item list(sum(obs), sum(pyrs), mean(dg_age)), multiple objects in a list
\item list(obs = sum(obs), pyrs = sum(pyrs)), same as above with user
defined variable names
}

If  \code{use.levels = FALSE}, no \code{levels} information will
be used. This means that if e.g. the  \code{agegroup}
variable is a factor and has 18 levels defined, but only 15 levels
are present in the data, no rows for the missing
levels will be shown in the table.

\code{na.rm} simply drops any rows from the resulting table where
any of the  \code{by.vars} values was \code{NA}.
}
\section{Functions}{
\itemize{
\item \code{expr.by.cj()}: Somewhat more streamlined \code{ltable} with
defaults for speed. Explicit determination of enclosing environment
of data.

}}
\examples{
data("sire", package = "popEpi")
sr <- sire
sr$agegroup <- cut(sr$dg_age, breaks=c(0,45,60,75,85,Inf))
## counts by default
ltable(sr, "agegroup")

## any expression can be given
ltable(sr, "agegroup", list(mage = mean(dg_age)))
ltable(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))

## also returns levels where there are zero rows (expressions as NA)
ltable(sr, "agegroup", list(obs = .N,
                            minage = min(dg_age),
                            maxage = max(dg_age)),
       subset = dg_age < 85)

#### expr.by.cj
expr.by.cj(sr, "agegroup")

## any arbitrary expression can be given
expr.by.cj(sr, "agegroup", list(mage = mean(dg_age)))
expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))

## only uses levels of by.vars present in data
expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)),
           subset = dg_age < 70)

## .SDcols trick
expr.by.cj(sr, "agegroup", lapply(.SD, mean),
           subset = dg_age < 70, .SDcols = c("dg_age", "status"))
}
\seealso{
\verb{[table]}, \verb{[cast_simple]}, \verb{[data.table::melt]}
}
\author{
Joonas Miettinen, Matti Rantanen
}
