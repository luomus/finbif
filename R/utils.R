#' @noRd
reduce_merge <- function(df) {
  df <- Reduce(function(x, y) merge(x, y, all = TRUE), df)
  if (is.null(df)) data.frame() else df
}

to_sentence_case <- function(string) {
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))
}

get_next_lowest_factor <-
  function(x, y) ifelse(x %% y, get_next_lowest_factor(x, y - 1L), y)
