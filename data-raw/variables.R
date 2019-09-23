var_names <- read.csv(
  "data-raw/variables.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

vars <- httr::GET("https://api.laji.fi/explorer/swagger.json")
vars <- jsonlite::fromJSON(httr::content(vars, "text"), simplifyVector = FALSE)
vars <- vars[["paths"]][["/warehouse/query/list"]][["get"]][["parameters"]]
vars <- vars[[which(vapply(vars, getElement, "", "name") == "selected")]]
vars <- unlist(vars[["items"]][["enum"]])

stopifnot(identical(sort(row.names(var_names)), sort(vars)))

class(var_names[["translated_var"]]) <- "translation"
