source("data-raw/utils.R")
var_names <- read.csv(
  "data-raw/variables.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L, comment.char = "#"
)

vars <- httr::GET("https://api.laji.fi/explorer/swagger.json")
vars <- jsonlite::fromJSON(httr::content(vars, "text"), simplifyVector = FALSE)
vars <- vars[["paths"]][["/warehouse/query/unit/list"]][["get"]][["parameters"]]

select_vars <- vars[[which(vapply(vars, getElement, "", "name") == "selected")]]
select_vars <- unlist(select_vars[["items"]][["enum"]])
order_vars  <- vars[[which(vapply(vars, getElement, "", "name") == "orderBy")]]
order_vars  <- unlist(order_vars[["items"]][["enum"]])

select_vars_pkg <- row.names(var_names[var_names[["select"]], ])
select_vars_pkg <-
  grep("^computed_var", select_vars_pkg, value = TRUE, invert = TRUE)

in_pkg_only <- setdiff(select_vars_pkg, select_vars)
schema_only <- setdiff(select_vars, select_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

order_vars_pkg <- c(
  row.names(var_names[var_names[["order"]], ]) ,"RANDOM", "RANDOM:seed"
)

in_pkg_only <- setdiff(order_vars_pkg, order_vars)
schema_only <- setdiff(order_vars, order_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

doc_vars     <- var_names[var_names[["doc"]], "translated_var"]
vars_in_docs <- documented_vars("R/variables.R")

doc_vars_only <- setdiff(doc_vars, vars_in_docs)
in_docs_only  <- setdiff(vars_in_docs, doc_vars)

stopifnot(!length(c(doc_vars_only, in_docs_only)))

class(var_names[["translated_var"]]) <- "translation"
class(var_names[["dwc"]]) <- "translation"
