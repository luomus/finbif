var_names <- read.csv(
  "data-raw/variables.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

class(var_names[["translated_var"]]) <- "translation"
