\name{NEWS}
\title{News for Package \pkg{finbif}}

\section{finbif version 0.4.0.9002}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New functions \code{scientific_name} and \code{common_name} are now
        available to get scientific and common names for taxa (given a taxon
        name or FinBIF ID code) or extract names from a \code{finbif_taxa}-
        class object.
    }
  }
}

\section{finbif version 0.4.0.9001}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Collection name can be selected as a variable for occurrence
        records. Previously it had to be manually translated from collection ID.
      \item Variables \code{restriction_reasons_conservation},
        \code{restriction_reason_embargo} and \code{restriction_reason_custom}
        are no longer in use upstream and can no longer be selected.
    }
  }
}

\section{finbif version 0.4.0.9000}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Add ability to extract complex record variables such as those
        associated with record media (currently undocumented).
    }
  }
}

\section{finbif version 0.4.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Occurrence record requests can now be made with aggregation,
        \code{aggregate = c("records", "species", "taxa")}. This returns the
        number of records, and/or species or taxa for each combination of the
        selected variables instead of the occurrence records themselves.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Error is triggered when attempting to request less than one record.
      \item New content on requesting aggregated occurrence records added to
        vignette: 2. Occurrence records from FinBIF.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed two bugs in print method for FinBIF occurrence records that
        were triggered when attempting to print only one row of data.
      \item Fixed bug triggering error when attempting to print occurrence
        record objects with zero rows.
      \item Fixed bug triggered when trying to print occurrence record data
        with NA values.
      \item Fixed bug causing incorrect subsetting of occurrence records when
        using logical vectors to subset rows.
      \item Fixed bug that printed occurrence records with multi-element data
        incorrectly when there was a single element.
      \item Fixed bug triggered by some system locales (#1).
    }
  }
}

\section{finbif version 0.3.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Occurrence records can be ordered by the total number of records or
        total number of Finnish records of the taxon (variables:
        \code{n_total_records} & \code{n_total_finnish_records}, currently
        undocumented).
      \item New Finnish occurrence status, \code{records_only}, added.
      \item New administrative statuses added to filters.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Filtering vignette updated to reflect changes to data quality
        filters.
    }
  }
}

\section{finbif version 0.3.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item All user facing functions with a \code{finbif_} prefix can now also
        be used with the alternative shorter prefix \code{fb_}.
      \item Crop Wild Relative, CWR, added to admin statuses.
      \item There are two new utility functions, \code{to_dwc} and
        \code{to_native} for converting variable names to and from Darwin Core
        style.
      \item Users can select and order by variables in Darwin Core style when
        using \code{finbif_occurrence}.
      \item Some changes in variables and filters have flowed from upstream
        changes to "api.laji.fi". The variables \code{is_unidentifiable},
        \code{record_reliable}, \code{collection_reliability},
        \code{taxon_reliability}, \code{taxon_reliability_message} and
        \code{taxon_reliability_source} have been deprecated and replaced with
        \code{requires_verification}, \code{requires_identification},
        \code{record_reliability} and \code{record_quality}. The filters
        \code{collection_reliability} and \code{taxon_reliability} have been
        deprecated and replaced with \code{requires_verification},
        \code{collection_quality}, \code{record_reliability},
        \code{record_quality}, and \code{expert_verified}.
      \item Vernacular names are now localised. Users can select a language to
        use for taxon vernacular names. Missing names will fallback gracefully
        to other languages. A package-wide locale can be set and is by default
        set to the system locale (if not set or can't be determined it will
        default to English).
      \item New vignettes on getting occurrence records, selecting and ordering
       variables, metadata and plotting have been added.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Caching defaults to in memory caching instead of relying on the
        temporary directory.
      \item Front matter of vignettes is now visible when using the R help
        browser.
      \item Retired "quiet" option for "on_check_fail" argument in function
        \code{finbif_occurrence()}.
      \item Improved error messages when taxa fail checking in
        \code{finbif_occurrence()}.
      \item Global option to set timezone, "finbif_tz", as default value for
        "tzone" argument to \code{finbif_occurrence()}. Avoids having to set
        system environment variable TZ or specify "tzone" every time
        \code{finbif_occurrence()} is run.
      \item Now when a record has no time information the start time is assumed
        to be midday. Previous behaviour was to assume start time was midnight,
        making errors potentially biased.
      \item Package options are now documented in the package level man page.
      \item News file is now accessible via R internal help system.
      \item The number of default variables selected when accessing occurrence
        records has been reduced to speed up downloads and improve the display
        of \code{finbif_occ} objects.
      \item The print method for occurrence record objects has been updated. It
        is now aware of console width and when truncating variable values is
        more considerate of the context.
      \item Variables can now be "deselected" when using the \code{select}
        argument to \code{finbif_occurrence} by prepending the variable name
        with a "\code{-}".
      \item After some failures, API requests are now automatically retried up
        to three times.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Ordering by descending variables did not work when ordering by both
        ascending and descending variables.
      \item Fixed bug in handling of duplicates that could result in an infinite
        recursion.
      \item Fixed bug that (when "on_check_fail" = "warn") all taxa failed
        checks `finbif_occurrence()` would proceed as if no taxa had been
        selected.
      \item Fixed bug in print method for \code{finbif_occ} objects that caused
        error when trying to display a single column objects with a list-col
        only.
    }
  }
}

\section{finbif version 0.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Add capacity to request a random sample of FinBIF records.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Add more content to vignettes.
    }
  }
}

\section{finbif version 0.1.0}{
  \itemize{
    \item Initial release.
  }
}
