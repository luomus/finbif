documented_vars <- function(x) {
  x <- readLines(x)
  x <- grep("#' -", x,  value = TRUE, fixed = TRUE)
  m <- regexpr("`(.*?)`", x)
  x <- regmatches(x, m)
  x <- gsub("`", "", x, fixed = TRUE)
  x <- strsplit(gsub("\\{|\\}", "", x), "_", fixed = TRUE)
  x <- lapply(x, strsplit, split = "\\|")
  x <- lapply(x, expand_string)
  unlist(x)
}

expand_string <- function(x) {
  x <- expand.grid(x)
  do.call(function(...) paste(..., sep = "_"), x)
}
