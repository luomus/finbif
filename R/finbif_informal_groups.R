#' FinBIF informal groups
#'
#' Display the informal taxonomic groups used in the FinBIF database.
#'
#' @aliases fb_informal_groups
#'
#' @param group Character. Optional, if supplied only display this top-level
#'   group and its subgroups.
#' @param limit Integer. The maximum number top-level informal groups (and their
#'   sub-groups) to display.
#' @param quiet Logical. Return informal group names without displaying them.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish and Swedish. For
#'   data where more than one language is available the language denoted by
#'   `locale` will be preferred while falling back to the other languages in the
#'   order indicated above.
#' @param cache Logical or Integer. If `TRUE` or a number greater than zero,
#'   then data-caching will be used. If not logical then cache will be
#'   invalidated after the number of hours indicated by the argument.
#' @return A character vector (invisibly).
#' @examples \dontrun{
#'
#' # Display the informal taxonomic groups used by FinBIF
#' finbif_informal_groups()
#'
#' }
#' @export

finbif_informal_groups <- function(
  group,
  limit = 5,
  quiet = FALSE,
  locale = getOption("finbif_locale"),
  cache = getOption("finbif_use_cache_metadata")
) {

  if (!locale %in% sysdata(list(which = "supported_langs"))) {

    locale <- "en"

  }

  cache <- getOption("finbif_use_cache")

  request <- list(
    path = "informal-taxon-groups/tree",
    query = list(pageSize = 1000L, lang = locale),
    cache = cache
  )

  informal_grps <- api_get(request)

  informal_grps <- informal_grps[[c("content", "results")]]

  grps <- vapply(informal_grps, getElement, "", "name")

  if (!missing(group)) {

    groups <- grps == group

    stopifnot("Group not found" = any(groups))

    grp <- which(groups)

    informal_grps <- informal_grps[grp]

  }

  if (!quiet) {

    obj <- list(informal_grps = informal_grps, cntr = 0L, limit = limit)

    print_informal_group(obj)

  }

  informal_grps <- unlist(informal_grps)

  informal_grps <- unname(informal_grps)

  informal_grps <- grep("^MVL\\.", informal_grps, invert = TRUE, value = TRUE)

  class(informal_grps) <- "translation"

  invisible(informal_grps)

}

#' @noRd

print_informal_group <- function(obj) {

  informal_grps <- obj[["informal_grps"]]

  times <- max(obj[["cntr"]] - 1L, 0L)

  pad <- rep("    ", times)

  cntr1 <- obj[["cntr"]] + 1L

  cntr2 <- 1L

  len <- length(informal_grps)

  limit <- min(obj[["limit"]], len)

  for (informal_grp in informal_grps) {

    branch <- ""

    is_branch <- cntr1 > 1L

    if (is_branch) {

      branch <- "  --"

    }

    if (!is_branch && cntr2 > limit) {

      sq <- seq_len(cntr2 - 1L)

      extras <- informal_grps[-sq]

      extras <- unlist(extras)

      extras <- grep("MVL\\.", extras)

      extra <- length(extras)

      s <- ""

      if (extra > 1L) {

        s <- "s"

      }

      cat("...", extra, " more group", s, sep = "")

      break

    }

    cntr2 <- cntr2 + 1L

    cat(pad, branch, informal_grp[["name"]], "\n", sep = "")

    if ("hasSubGroup" %in% names(informal_grp)) {

      obj_i <- list(
        informal_grps = informal_grp[["hasSubGroup"]],
        cntr = cntr1,
        limit = limit
      )

      print_informal_group(obj_i)

    }

  }

  invisible(NULL)

}
