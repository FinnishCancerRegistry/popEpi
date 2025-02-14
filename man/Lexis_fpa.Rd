% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{Lexis_fpa}
\alias{Lexis_fpa}
\title{Create a Lexis Object with Follow-up Time, Period, and Age
Time Scales}
\usage{
Lexis_fpa(
  data,
  birth = NULL,
  entry = NULL,
  exit = NULL,
  entry.status = NULL,
  exit.status = NULL,
  subset = NULL,
  ...
)
}
\arguments{
\item{data}{a \code{data.frame}; mandatory}

\item{birth}{the time of birth; A character string naming the variable in
data or an expression to evaluate - see
\link[=flexible_argument]{Flexible input}}

\item{entry}{the time at entry to follow-up; supplied the
same way as \code{birth}}

\item{exit}{the time at exit from follow-up; supplied the
same way as \code{birth}}

\item{entry.status}{passed on to \verb{[Epi::Lexis]} if not \code{NULL};
supplied the same way as \code{birth}}

\item{exit.status}{passed on to \verb{[Epi::Lexis]} if not \code{NULL};
supplied the same way as \code{birth}}

\item{subset}{a logical condition to subset by before passing data
and arguments to \verb{[Epi::Lexis]}}

\item{...}{additional optional arguments passed on to
\verb{[Epi::Lexis]}}
}
\value{
A \code{Lexis} object with the usual columns that \code{Lexis} objects
have, with time scale columns \code{fot}, \code{per}, and \code{age}.
They are calculated as

\code{fot = entry - entry} (to ensure correct format, e.g. difftime)

\code{per = entry}

and

\code{age = entry - birth}
}
\description{
This is a simple wrapper around \verb{[Epi::Lexis]} for creating
a \code{Lexis} object with the time scales \code{fot}, \code{per},
and \code{age}.
}
\examples{

data("sire", package = "popEpi")

lex <- Lexis_fpa(sire,
                 birth = "bi_date",
                 entry = dg_date,
                 exit = ex_date + 1L,
                 exit.status = "status")

## some special cases
myVar <- "bi_date"
l <- list(myVar = "bi_date")
sire$l <- sire$myVar <- 1

## conflict: myVar taken from data when "bi_date" was intended
lex <- Lexis_fpa(sire,
                 birth = myVar,
                 entry = dg_date,
                 exit = ex_date + 1L,
                 exit.status = "status")

## no conflict with names in data
lex <- Lexis_fpa(sire,
                 birth = l$myVar,
                 entry = dg_date,
                 exit = ex_date + 1L,
                 exit.status = "status")
}
