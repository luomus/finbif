source("data-raw/utils.R")
var_names_test <- read.csv(
  "data-raw/variables_test.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L, comment.char = "#"
)

vars <- httr::GET("https://apitest.laji.fi/explorer/swagger.json")
vars <- jsonlite::fromJSON(httr::content(vars, "text"), simplifyVector = FALSE)
vars <- vars[["paths"]][["/warehouse/query/unit/list"]][["get"]][["parameters"]]

select_vars <- vars[[which(vapply(vars, getElement, "", "name") == "selected")]]
select_vars <- unlist(select_vars[["items"]][["enum"]])
order_vars  <- vars[[which(vapply(vars, getElement, "", "name") == "orderBy")]]
order_vars  <- unlist(order_vars[["items"]][["enum"]])

select_vars_pkg <- row.names(var_names_test[var_names_test[["select"]], ])

in_pkg_only <- setdiff(select_vars_pkg, select_vars)
schema_only <- setdiff(select_vars, select_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

order_vars_pkg <- c(
  row.names(var_names_test[var_names_test[["order"]], ]), "RANDOM",
  "RANDOM:seed"
)

in_pkg_only <- setdiff(order_vars_pkg, order_vars)
schema_only <- setdiff(order_vars, order_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

class(var_names_test[["translated_var"]]) <- "translation"
class(var_names_test[["dwc"]]) <- "translation"
