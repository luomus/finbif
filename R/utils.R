to_sentence_case <- function(string) {
  if (is.null(string)) return(NULL)
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))
}

get_next_lowest_factor <-
  function(x, y) ifelse(x %% y, get_next_lowest_factor(x, y - 1L), y)
