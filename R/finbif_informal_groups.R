#' FinBIF informal groups
#'
#' Display the informal taxonomic groups used in the FinBIF database.
#'
#' @aliases fb_informal_groups
#'
#' @param group Character. Optional, if supplied only display this group and
#'  its subgroups.
#' @param limit Integer. The maximum number informal groups to display.
#' @param quiet Logical. Return informal group names without displaying them.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish, Swedish, Russian,
#'   and Sámi (Northern). For data where more than one language is available
#'   the language denoted by `locale` will be preferred while falling back to
#'   the other languages in the order indicated above.
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
  limit = 50,
  quiet = FALSE,
  locale = getOption("finbif_locale")
) {

  cols <- names(informal_groups)

  nm_cols <- grep("^name_", cols, value = TRUE)

  nm_df <- informal_groups[, nm_cols, drop = FALSE]

  locales <- sub("^name_", "", nm_cols)

  names(nm_df) <- locales

  nms <- with_locale(nm_df, locale)

  tree_cols <- grep("^tree", cols, value = TRUE)

  tree_df <- informal_groups[, tree_cols, drop = FALSE]

  names(tree_df) <- locales

  trees <- with_locale(tree_df, locale)

  has_group_arg <- !missing(group)

  if (has_group_arg) {

    group <- to_sentence_case(group)

    has_group <- group %in% nms

    stopifnot(has_group)

    is_group_nm <- nms == group

    begin <- which(is_group_nm)

    lvl <- regexpr("\\w", trees)

    lvl_begin <- lvl[begin]

    lvl_seq <- seq_along(lvl)

    is_lvl_begin <- lvl == lvl_begin

    after_lvl_begin <- lvl_seq > begin

    is_and_after_lvl <- is_lvl_begin & after_lvl_begin

    end <- which(is_and_after_lvl)

    end <- end[1L]

    end <- end - 1L

    begin_end_seq <- seq.int(begin, end)

    nms <- nms[begin_end_seq]

    trees <- trees[begin_end_seq]

  }

  n <- length(nms)

  limit <- min(limit, n)

  limit <- as.integer(limit)

  verbose <- !quiet

  if (verbose) {

    seq_limit <- seq_len(limit)

    trees_limit <- trees[seq_limit]

    cat(trees_limit, sep = "\n")

    extra <- n - limit

    has_extra <- extra > 0L

    if (has_extra) {

      has_extras <- extra > 1L

      s <- ""

      if (has_extras) {

        s <- "s"

      }

      cat("...", extra, " more group", s, sep = "")

    }

  }

  invisible(nms)

}
