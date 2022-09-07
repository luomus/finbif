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
#' }
#' @export

finbif_informal_groups <- function(
  group, limit = 50, quiet = FALSE, locale = getOption("finbif_locale")
) {

  df_names <- informal_groups[, grep("^name_", names(informal_groups))]
  names(df_names) <- sub("^name_", "", names(df_names))
  df_names <- with_locale(df_names, locale)

  df_trees <- informal_groups[, grep("^tree_", names(informal_groups))]
  names(df_trees) <- sub("^tree_", "", names(df_trees))
  df_trees <- with_locale(df_trees, locale)

  if (!missing(group)) {
    group <- to_sentence_case(group)
    stopifnot(group %in% df_names)
    begin <- which(df_names == group)
    lvl <- regexpr("\\w", df_trees)
    end <- which(lvl == lvl[begin] & seq_along(lvl) > begin)[1L] - 1L
    df_names <- df_names[seq.int(begin, end)]
    df_trees <- df_trees[seq.int(begin, end)]
  }
  n <- length(df_names)
  limit <- min(limit, n)
  if (!quiet) {
    cat(df_trees[seq_len(limit)], sep = "\n")
    extra <- n - limit
    if (extra > 0L) {
      cat(
        "...", extra, " more group", if (extra == 1) "" else "s", "\n", sep = ""
      )
    }
  }
  invisible(df_names)
}
