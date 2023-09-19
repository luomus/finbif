\name{NEWS}
\title{News for Package \pkg{finbif}}

\section{finbif version 0.9.2.9000}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item New undocumented variables \code{institution_code} and
        \code{collection_code} can be selected when using functions
        \code{finbif_collections} and \code{finbif_occurrence}.
    }
  }
}

\section{finbif version 0.9.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a bug triggered when localising some variables and multiple
        pages of data had been requested.
    }
  }
}

\section{finbif version 0.9.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New option \code{finbif_use_cache_metadata} to set caching for
        metadata only. Is ignored if \code{finbif_use_cache} is \code{TRUE}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a bug triggered when trying to compute a variable from an
        identifier and data returned contained zero records.
    }
  }
}

\section{finbif version 0.9.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New function \code{finbif_update_cache} can be used to update all
        the cached objects that have reached their expiration and are no longer
        valid.
      \item A restricted data API token can be explicitly declared via the
        \code{restricted_api} argument to \code{finbif_occurrence}.
      \item Cached data can use a database as a backend by passing a \code{DBI}
        database connection object to the package option
        \code{finbif_cache_path}.
      \item Cached data can be assigned a timeout by using a number (hours)
        instead of a logical value for the \code{cache} argument to
        \code{finbif_} functions or the package option \code{finbif_use_cache}.
      \item Record aggregations with \code{finbif_occurrence} can now be
        combined with multi-filter requests. Removing duplicate records or
        aggregate record combinations can be controlled with the
        \code{duplicates} argument and adding a column indicating the filter
        sets can be added with the \code{filter_col} argument.
      }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a bug causing an infinite recursion when determining page size
        during record download pagination.
      \item Fix bug triggered when using aggregation with some selected
        variables.
      }
    }
  }
  \subsection{DEFUNCT}{
    \itemize{
      \item Mapping utilities \code{finland_map}, \code{breaks_xy},
        \code{hist_xy} and the plot method for \code{finbif_occ} objects are now
        defunct.
      \item User access to \code{finbif_records} and its classes and methods is
        now defunct.
    }
  }
}

\section{finbif version 0.8.0}{
  \subsection{DEPRECATION}{
    \itemize{
      \item Mapping utilities \code{finland_map}, \code{breaks_xy},
        \code{hist_xy} and the plot method for \code{finbif_occ} objects have
        been deprecated as they are out-of-scope.
      \item User access to \code{finbif_records} and its classes and methods is
        deprecated in favour of \code{finbif_occurrence}.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item Record aggregations with \code{finbif_occurrence} can now include
        "individual" and "pair" sums.
    }
  }
}

\section{finbif version 0.7.2}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Updated deprecated use of tidy::pivot_wider.
      \item Added new filters and variables from upstream API.
    }
  }
}

\section{finbif version 0.7.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New function \code{finbif_mod_date} to get last modified date for
        FinBIF occurrence data.
      \item Region is now a selectable variable for \code{finbif_occurrence}
        and can also be used to filter records.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item For the sake of clarity "province" is now referred to as
        "bio-province".
      \item Many variables, including \code{sex}, \code{life_stage} and
        \code{record_quality} are now localised.
      \item The variable \code{restriction_reasons} is now referred to in the
        singular as \code{restriction_reason}.
      \item For consistency, \code{lang} argument to \code{finbif_collections}
        has been changed to \code{locale}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed bug that caused locale to be set incorrectly for multipage
        requests.
    }
  }
}

\section{finbif version 0.7.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New function \code{taxon_id} that returns FinBIF taxon identifiers
        given a scientific name, vernacular name or other taxon identifier.
      \item New filters \code{complete_list_taxon_id} and
        \code{complete_list_type} now available.
      \item New global option \code{finbif_hide_progress} to suppress progress
        bars when downloading, importing or processing FinBIF records.
      \item For the sake of clarity and consistency the term "administrative
        status" is now referred to as "regulatory status" throughout.
      \item Regulatory status, informal taxonomic group and habitat data are now
        localised.
      \item Option \code{unlist} added to \code{finbif_occurrence} that casts
        list columns to strings. If a cell has multiple values the values are
        cast to a string separated by semi-colons.
      \item New argument \code{facts} added to \code{finbif_occurrence} that
        can be used to automatically extract facts when downloading occurrence
        records.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed bug triggered by incorrect data type applied to
        \code{taxonomic_order}.
      \item Fixed bug preventing locale switching of vernacular name data.
    }
  }
}

\section{finbif version 0.6.6}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New option to exclude all records with any NA values for any of the
       selected variables added to \code{finbif_occurrence}.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item The warning/error message returned when an invalid taxon is
        requested with \code{finbif_occurrence} has been made more detailed,
        warning the user they may be using synonyms or other invalid taxon
        names.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix applied for a print method error triggered by clashing class
        names.
      \item Fixed issue with ordering by some variables.
    }
  }
}

\section{finbif version 0.6.5}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New variables and filters (currently undocumented) have been added
        for the forthcoming Finnish atlas of breeding birds.
      \item Skip rows argument added to \code{finbif_occurrence_load} so files
        can be read in chunks.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item For clarity, KKJ coordinate reference system is now referred to as
        YKJ.
      \item When selecting all available variables for download file imports
        using \code{finbif_occurrence_load} "extra" variables will no longer
        be automatically added.
    }
  }
}

\section{finbif version 0.6.4}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item New administrative statuses added.
      \item The number of (and interval between) api request retries can be
        configured (currently undocumented).
      \item For some museum collections scientific names have been added for
        non-Finnish species.
      \item New fields have been added to citable download imports.
      \item New argument \code{aggregate_counts} (defaulting to \code{TRUE})
        added to \code{finbif_occurrence} to toggle the inclusion of
        aggregation count fields on and off.
      \item New bird association area, occurrence types and location tag
        metadata added.
      \item New filters and variables (currently undocumented) added from
        upstream API.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix bug triggered when printing occurrence records where all records
        have no coordinate uncertainty.
    }
  }
}

\section{finbif version 0.6.3}{
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Argument \code{drop_na_facts} has been changed to
        \code{drop_facts_na} for function \code{finbif_occurrence_load} to
        avoid problems due to partial matching of argument \code{drop_na}.
      \item Data quality description has been appended to description field in
        \code{data.frame} returned by \code{finbif_collections()}.
      \item Occurrence record filtering can now be done using open-ended date
        ranges.
      \item New administrative statuses added.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix bug in importing of file downloads triggered by string quotation
        errors.
    }
  }
}

\section{finbif version 0.6.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New package option, \code{finbif_allow_query}, with default value,
        \code{TRUE}. If set to \code{FALSE} then remote API queries will trigger
        an error meaning only cached requests will run if a cache is found.
      \item Records can now be aggregated by document.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item A client-side error will be triggered if an attempt is made to
        filter by taxon and aggregate data by event with
        \code{finbif_occurrence}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix bug preventing loading of FinBIF archives that is triggered
        when all variables are selected with \code{select = "all"}.
      \item Fix bug preventing \code{finbif_occ} objects from printing when one
        or more columns contain missing data only.
    }
  }
}

\section{finbif version 0.6.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a regression in the last release that prevented collection
        names being converted from their URIs.
    }
  }
}

\section{finbif version 0.6.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New variables \code{lat_euref} and \code{lon_euref} available for
        occurrence record downloads. These new variables represent the central
        point of a bounding box encompassing a record's geographic coverage in
        the EUREF (ETRS89/ETRS-TM35FIN) coordinate system.
      \item Option now available to retain the TSV data file when loading FinBIF
        occurrence data with \code{finbif_occurrence_load} from ZIP archive or
        online reference.
      \item Option now available to extract "facts" when using
        \code{finbif_occurrence_load} to import records from a ZIP archive. The
        user must supply a named list, where the names refer to one or more fact
        types: "record", "event" and/or "document", and the list elements
        are character vectors indicating which "facts" to extract.
      \item "Lite" download files (occurrence record datasets of less than
        10,000 records downloaded directly from laji.fi) can now be imported
        using \code{finbif_occurrence_load}.
      \item New filters (currently undocumented) \code{ely_center_id} and
        \code{region_id} are available for \code{finbif_occurrence}.
      \item New function, \code{from_schema}, to convert variable names from
        FinBIF schema to other styles.
      \item New computed variable \code{date_time_ISO8601} (currently
        undocumented) can be selected when creating \code{finbif_occ} objects.
      \item New computed variable \code{epsg} (currently undocumented). If
        selected, the column will have the EPSG code of the first column that
        contains geographic data.
    }
  }
  \subsection{MINOR IMPROVEMENTS}{
    \itemize{
      \item Can now select all available variables when importing a FinBIF
        download request file using \code{finbif_occurrence_load} with
        \code{select = "all"}.
      \item New option, \code{drop_na}, to drop columns that only contain
        missing data added to \code{finbif_occurrence} and
        \code{finbif_occurrence_load}.
      \item User can now select columns multiple times and rename them on the
        fly by using a named character vector as the value of the `select`
        argument.
      \item The variable \code{taxon_rank} is now converted to a string on the
        fly via \code{taxon_rank_id}.
      \item Variables that are computed from their identifiers are now localised
        when multiple languages are available.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed bug that prevented occurrence record counting when caching
        was not in use.
      \item Fixed bug that triggered an error when selected a variable that
        had to be computed from its ID when using DWC style variable names.
    }
  }
}

\section{finbif version 0.5.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Getting records from FinBIF can now be speed up (~1.5x) with
        asynchronous processing. If the \code{future} package is available and
        an asynchronous strategy (e.g., \code{plan(multisession, workers = 2)})
        is selected then downloading records and processing them will occur
        simultaneously in separate threads, often leading to a significant
        speed up over sequential processing.
      \item Occurrence records can now be requested using multiple sets of
        filters at the same time. If a list of filters (with unnamed elements)
        is supplied then a record request will be made for each set of filters
        and the results of all requests combined and all duplicate records
        removed.
      \item Occurrence records can now be read directly from FinBIF download
        request files. The function \code{finbif_occurrence_load} can load data
        from a local file or remotely using a persistent identifier URI (e.g.,
        \code{http://tun.fi/HBF.49381}).
      \item Aggregation of records can now be performed at the recording "event"
        level as well as the "record" level.
    }
  }
}

\section{finbif version 0.4.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New functions \code{scientific_name} and \code{common_name} are now
        available to get scientific and common names for taxa (given a taxon
        name or FinBIF ID code) or extract names from a \code{finbif_taxa}-
        class object.
      \item Add ability to extract complex record variables such as those
        associated with record media (currently undocumented).
    }
  }
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
