\name{NEWS}
\title{News for Package \pkg{finbif}}

\section{finbif version 0.2.0.9007}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Package options are now documented in the package level man page.
      \item News file is now accessible via R internal help system.
      \item The number of default variables selected when accessing occurrence
        records has been reduced to speed up downloads and improve the display
        of \code{finbif_occ} objects.
    }
  }
}

\section{finbif version 0.2.0.9006}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Now when a record has no time information the start time is assumed
        to be midday. Previous behaviour was to assume start time was midnight,
        making errors potentially biased.
    }
  }
}

\section{finbif version 0.2.0.9005}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Global option to set timezone, "finbif_tz", as default value for
        "tzone" argument to finbif_occurrence(). Avoids having to set system
        environment variable TZ or specify "tzone" every time
        finbif_occurrence() is run.
    }
  }
}

\section{finbif version 0.2.0.9004}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Retired "quiet" option for "on_check_fail" argument in function
        `finbif_occurrence()`.
      \item Improved error messages when taxa fail checking in
        `finbif_occurrence()`.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed bug in handling of duplicates that could result in an infinite
        recursion.
      \item Fixed bug that (when "on_check_fail" = "warn") all taxa failed
        checks `finbif_occurrence()` would proceed as if no taxa had been
        selected.
    }
  }
}

\section{finbif version 0.2.0.9003}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item All user facing functions with a \code{finbif_} prefix can now also
        be used with the alternative shorter prefix \code{fb_}.
      \item Crop Wild Relative, CWR, added to admin statuses.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Front matter of vignettes is now visible when using the R help
        browser.
    }
  }
}

\section{finbif version 0.2.0.9002}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Ordering by descending variables did not work when ordering by both
        ascending and descending variables.
    }
  }
}

\section{finbif version 0.2.0.9001}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Caching defaults to in memory caching instead of relying on the
        temporary directory.
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
