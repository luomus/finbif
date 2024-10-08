% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finbif_occurrence_load.R
\name{finbif_occurrence_load}
\alias{finbif_occurrence_load}
\alias{fb_occurrence_load}
\title{Load FinBIF occurrence records from a file}
\usage{
finbif_occurrence_load(
  file,
  select = NULL,
  n = -1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = FALSE,
  date_time_method = NULL,
  tzone = getOption("finbif_tz"),
  write_file = tempfile(),
  dt = NA,
  keep_tsv = FALSE,
  facts = list(),
  type_convert_facts = TRUE,
  drop_na = FALSE,
  drop_facts_na = drop_na,
  locale = getOption("finbif_locale"),
  skip = 0
)
}
\arguments{
\item{file}{Character or Integer. Either the path to a Zip archive or
tabular data file that has been downloaded from "laji.fi", a URI
linking to such a data file (e.g.,
\url{https://tun.fi/HBF.49381}) or an integer
representing the URI (i.e., \code{49381}).}

\item{select}{Character vector. Variables to return. If not specified, a
default set of commonly used variables will be used. Use \code{"default_vars"}
as a shortcut for this set. Variables can be deselected by prepending a \code{-}
to the variable name. If only deselects are specified the default set of
variables without the deselection will be returned. Use \code{"all"} to select
all available variables in the file.}

\item{n}{Integer. How many records to import. Negative and other invalid
values are ignored causing all records to be imported.}

\item{count_only}{Logical. Only return the number of records available.}

\item{quiet}{Logical. Suppress the progress indicator for multipage
downloads. Defaults to value of option \code{finbif_hide_progress}.}

\item{cache}{Logical or Integer. If \code{TRUE} or a number greater than zero,
then data-caching will be used. If not logical then the cache will be
invalidated after the number of hours indicated by the argument. If a
length one vector is used, its value will only apply to caching
occurrence records. If the value is length two, then the second element
will determine how metadata is cached.}

\item{dwc}{Logical. Use Darwin Core (or Darwin Core style) variable names.}

\item{date_time_method}{Character. Passed to \code{lutz::tz_lookup_coords()} when
\code{date_time} and/or \code{duration} variables have been selected. Default is
\code{"fast"} when  less than 100,000 records are requested and \code{"none"} when
more. Using method \code{"none"} assumes all records are in timezone
"Europe/Helsinki", Use \code{date_time_method = "accurate"} (requires package
\code{sf}) for greater accuracy at the cost of slower computation.}

\item{tzone}{Character. If \code{date_time} has been selected the timezone of the
outputted date-time. Defaults to system timezone.}

\item{write_file}{Character. Path to write downloaded zip file to if \code{file}
refers to a URI. Will be ignored if \code{getOption("finbif_cache_path")} is not
\code{NULL} and will use the cache path instead.}

\item{dt}{Logical. If package, \code{data.table}, is available return a
\code{data.table} object rather than a \code{data.frame}.}

\item{keep_tsv}{Logical. Whether to keep the TSV file if \code{file} is a ZIP
archive or represents a URI. Is ignored if \code{file} is already a TSV. If
\code{TRUE} the tsv file will be kept in the same directory as the ZIP archive.}

\item{facts}{List. A named list of "facts" to extract from supplementary
"fact" files in a local or online FinBIF data archive. Names can include
one or more of \code{"record"}, \code{"event"} or \code{"document"}. Elements of the list
are character vectors of the "facts" to be extracted and then joined to the
return value.}

\item{type_convert_facts}{Logical. Should facts be converted from character
to numeric or integer data where applicable?}

\item{drop_na}{Logical. A vector indicating which columns to check for
missing data. Values recycled to the number of columns. Defaults to all
columns.}

\item{drop_facts_na}{Logical. Should missing or "all \code{NA}" facts be dropped?
Any value other than a length one logical vector with the value of TRUE
will be interpreted as FALSE. Argument is ignored if \code{drop_na} is TRUE for
all variables explicitly or via recycling. To only drop some
missing/\code{NA}-data facts use \code{drop_na} argument.}

\item{locale}{Character. One of the supported two-letter ISO 639-1 language
codes. Current supported languages are English, Finnish and Swedish. For
data where more than one language is available the language denoted by
\code{locale} will be preferred while falling back to the other languages in the
order indicated above.}

\item{skip}{Integer. The number of lines of the data file to skip before
beginning to read data (not including the header).}
}
\value{
A \code{data.frame}, or if \code{count_only =  TRUE} an integer.
}
\description{
Load occurrence data from a file as a \code{data.frame}.
}
\examples{
\dontrun{

# Get occurrence data
finbif_occurrence_load(49381)

}
}
