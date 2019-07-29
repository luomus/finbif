to_sentence_case <- function(string) {
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))
}

get_next_lowest_factor <- function(x, y) {
  while (x %% y) y <- y - 1L
  y
}
