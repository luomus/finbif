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
  locale = getOption("finbif_locale")
) {

  no_locale <- !locale %in% supported_langs

  if (no_locale) {

    locale <- "en"

  }

  query <- list(pageSize = 1000L, lang = locale)

  cache <- getOption("finbif_use_cache")

  request <- list(
    path = "informal-taxon-groups/tree", query = query, cache = cache
  )

  x <- api_get(request)

  results <- c("content", "results")

  x <- x[[results]]

  grps <- vapply(x, getElement, "", "name")

  use_group <- !missing(group)

  if (use_group) {

    groups <- grps == group

    has_group <- any(groups)

    stopifnot("Group not found" = has_group)

    grp <- which(groups)

    x <- x[[grp]]

  }

  if (!quiet) {

    obj <- list(x = x, cntr = 0L, limit = limit)

    print_informal_group(obj)

  }

  x <- unlist(x)

  x <- unname(x)

  x <- grep("^MVL\\.", x, invert = TRUE, value = TRUE)

  class(x) <- "translation"

  invisible(x)

}

#' @noRd

print_informal_group <- function(obj) {

  x <- obj[["x"]]

  cntr1 <- obj[["cntr"]]

  limit <- obj[["limit"]]

  times <- max(cntr1 - 1L, 0L)

  pad <- rep("    ", times)

  cntr1 <- cntr1 + 1L

  cntr2 <- 1L

  x_len <- length(x)

  limit <- min(limit, x_len)

  for (i in x) {

    branch <- ""

    is_branch <- cntr1 > 1L

    if (is_branch) {

      branch <- "  --"

    }

    at_limit <- !is_branch && cntr2 > limit

    if (at_limit) {

      sq <- cntr2 - 1L

      sq <- seq_len(sq)

      extras <- x[-sq]

      extras <- unlist(extras)

      extras <- grep("MVL\\.", extras)

      extra <- length(extras)

      has_extras <- extra > 1L

      s <- ""

      if (has_extras) {

        s <- "s"

      }

      cat("...", extra, " more group", s, sep = "")

      break

    }

    cntr2 <- cntr2 + 1L

    i_name <- i[["name"]]

    cat(pad, branch, i_name, "\n", sep = "")

    nms <- names(i)

    has_sub <- "hasSubGroup" %in% nms

    if (has_sub) {

      xi <- i[["hasSubGroup"]]

      obj_i <- list(x = xi, cntr = cntr1, limit = limit)

      print_informal_group(obj_i)

    }

  }

  invisible()

}
