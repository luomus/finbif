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
#' @return A character vector (invisibly).
#' @examples \dontrun{
#'
#' # Display the informal taxonomic groups used by FinBIF
#' finbif_informal_groups()
#' }
#' @export

finbif_informal_groups <- function(group, limit = 50, quiet = FALSE) {
  df <- informal_group
  if (!missing(group)) {
    group <- to_sentence_case(group)
    stopifnot(group %in% df[["name"]])
    begin <- which(df[["name"]] == group)
    lvl <- regexpr("\\w", df[["tree"]])
    end <- which(lvl == lvl[begin] & seq_along(lvl) > begin)[1L] - 1L
    df  <- df[seq.int(begin, end), ]
  }
  n <- nrow(df)
  limit <- min(limit, n)
  if (!quiet) {
    cat(df[["tree"]][seq_len(limit)], sep = "\n")
    extra <- n - limit
    if (extra > 0L) {
      cat(
        "...", extra, " more group", if (extra == 1) "" else "s", "\n", sep = ""
      )
    }
  }
  invisible(df[["name"]])
}
