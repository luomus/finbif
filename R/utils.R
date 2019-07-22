to_sentence_case <- function(string) {
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))
}
