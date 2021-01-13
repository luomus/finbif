source("data-raw/utils.R")
var_names_test <- read.csv(
  "data-raw/variables_test.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L, comment.char = "#"
)

vars <- httr::GET(Sys.getenv("DEV_WAREHOUSE_SWAGGER_URL"))
vars <- jsonlite::fromJSON(httr::content(vars, "text"), simplifyVector = FALSE)
vars <- vars[["paths"]]
select_order_vars <-
  vars[["/query/unit/list"]][["get"]][["parameters"]]

select_vars <- select_order_vars[[
  which(vapply(select_order_vars, getElement, "", "name") == "selected")
]]
select_vars <- unlist(select_vars[["items"]][["enum"]])

order_vars <- select_order_vars[[
  which(vapply(select_order_vars, getElement, "", "name") == "orderBy")
]]
order_vars <- unlist(order_vars[["items"]][["enum"]])

agg_vars <-
  vars[["/query/unit/aggregate"]][["get"]][["parameters"]]
agg_vars <-
  agg_vars[[which(vapply(agg_vars, getElement, "", "name") == "aggregateBy")]]
agg_vars <- unlist(agg_vars[["items"]][["enum"]])

select_vars_pkg <- row.names(var_names_test[var_names_test[["select"]], ])
select_vars_pkg <-
  grep("^computed_var", select_vars_pkg, value = TRUE, invert = TRUE)

in_pkg_only <- setdiff(select_vars_pkg, select_vars)
schema_only <- setdiff(select_vars, select_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

order_vars_pkg <- c(
  row.names(var_names_test[var_names_test[["order"]], ]) ,"RANDOM",
  "RANDOM:seed"
)
order_vars_pkg <-
  grep("^computed_var", order_vars_pkg, value = TRUE, invert = TRUE)

in_pkg_only <- setdiff(order_vars_pkg, order_vars)
schema_only <- setdiff(order_vars, order_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

agg_vars_pkg <- row.names(var_names_test[var_names_test[["aggregate"]], ])
agg_vars_pkg <-
  grep("^computed_var", agg_vars_pkg, value = TRUE, invert = TRUE)

in_pkg_only <- setdiff(agg_vars_pkg, agg_vars)
schema_only <- setdiff(agg_vars, agg_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

class(var_names_test[["translated_var"]]) <- "translation"
class(var_names_test[["dwc"]]) <- "translation"
