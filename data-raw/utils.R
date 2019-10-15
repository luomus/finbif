documented_vars <- function(x) {
  x <- readLines(x)
  x <- grep("#' -", x,  value = TRUE)
  m <- regexpr("`(.*?)`", x)
  x <- regmatches(x, m)
  x <- gsub("`", "", x)
  x <- strsplit(gsub("\\{|\\}", "", x), "_")
  x <- lapply(x, strsplit, split = "\\|")
  x <- lapply(x, expand_string)
  unlist(x)
}

expand_string <- function(x) {
  x <- expand.grid(x)
  do.call(function(...) paste(..., sep = "_"), x)
}
