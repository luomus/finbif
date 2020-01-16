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

stopifnot(
  identical(
    sort(row.names(var_names[var_names[["select"]], ])),
    sort(select_vars)
  )
)

stopifnot(
  identical(
    sort(
      c(
        row.names(var_names[var_names[["order"]], ])
       ,"RANDOM", "RANDOM:seed"
      )
    ),
    sort(order_vars)
  )
)

stopifnot(
  identical(
    sort(var_names[var_names[["doc"]], "translated_var"]),
    sort(documented_vars("R/variables.R"))
  )
)

class(var_names[["translated_var"]]) <- "translation"
class(var_names[["dwc"]]) <- "translation"
