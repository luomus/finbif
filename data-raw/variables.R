source("data-raw/utils.R")
var_names <- read.csv(
  "data-raw/variables.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L, comment.char = "#"
)

vars <- httr::GET("https://dw.laji.fi/swagger")

if (identical(Sys.getenv("BRANCH"), "dev")) {

  vars <- httr::GET("https://staging.laji.fi/laji-etl/swagger")

}

vars <- httr::content(vars)
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

agg_gath_vars <-
  vars[["/query/gathering/aggregate"]][["get"]][["parameters"]]
agg_gath_vars <- agg_gath_vars[[
  which(vapply(agg_gath_vars, getElement, "", "name") == "aggregateBy")
]]
agg_gath_vars <- unlist(agg_gath_vars[["items"]][["enum"]])

agg_doc_vars <-
  vars[["/query/document/aggregate"]][["get"]][["parameters"]]
agg_doc_vars <- agg_doc_vars[[
  which(vapply(agg_doc_vars, getElement, "", "name") == "aggregateBy")
]]
agg_doc_vars <- unlist(agg_doc_vars[["items"]][["enum"]])

remove_vars <- "^computed_var|^missing_var"

select_vars_pkg <- row.names(var_names[var_names[["select"]], ])
select_vars_pkg <-
  grep(remove_vars, select_vars_pkg, value = TRUE, invert = TRUE)

in_pkg_only <- setdiff(select_vars_pkg, select_vars)
schema_only <- setdiff(select_vars, select_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

order_vars_pkg <- c(
  row.names(var_names[var_names[["order"]], ]), "RANDOM", "RANDOM:seed"
)
order_vars_pkg <- grep(
  remove_vars, order_vars_pkg, value = TRUE, invert = TRUE
)

in_pkg_only <- setdiff(order_vars_pkg, order_vars)
schema_only <- setdiff(order_vars, order_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

agg_vars_pkg <- row.names(var_names[var_names[["aggregate"]], ])
agg_vars_pkg <- grep(
  remove_vars, agg_vars_pkg, value = TRUE, invert = TRUE
)

in_pkg_only <- setdiff(agg_vars_pkg, agg_vars)
schema_only <- setdiff(agg_vars, agg_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

agg_gath_vars_pkg <- row.names(var_names[var_names[["aggregate_events"]], ])
agg_gath_vars_pkg <- grep(
  remove_vars, agg_gath_vars_pkg, value = TRUE, invert = TRUE
)

in_pkg_only <- setdiff(agg_gath_vars_pkg, agg_gath_vars)
schema_only <- setdiff(agg_gath_vars, agg_gath_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

agg_doc_vars_pkg <- row.names(var_names[var_names[["aggregate_documents"]], ])
agg_doc_vars_pkg <- grep(
  remove_vars, agg_doc_vars_pkg, value = TRUE, invert = TRUE
)

in_pkg_only <- setdiff(agg_doc_vars_pkg, agg_doc_vars)
schema_only <- setdiff(agg_doc_vars, agg_doc_vars_pkg)

stopifnot(!length(c(in_pkg_only, schema_only)))

doc_vars     <- var_names[var_names[["doc"]], "translated_var"]
vars_in_docs <- documented_vars("R/variables.R")

doc_vars_only <- setdiff(doc_vars, vars_in_docs)
in_docs_only  <- setdiff(vars_in_docs, doc_vars)

stopifnot(!length(c(doc_vars_only, in_docs_only)))

has_value <- var_names[
  grep(remove_vars, rownames(var_names), value = TRUE, invert = TRUE),
  c("translated_var", "dwc"),
  drop = FALSE
]

class(var_names[["translated_var"]]) <- "translation"
class(var_names[["dwc"]]) <- "translation"
class(has_value[["translated_var"]]) <- "translation"
class(has_value[["dwc"]]) <- "translation"

var_names_df <- var_names
has_value_df <- has_value
