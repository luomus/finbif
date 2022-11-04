langs <- c("en", "fi", "sv")

informal_groups <- lapply(
  langs,
  function(x) {
    finbif:::api_get(
      "informal-taxon-groups/tree", list(pageSize = 1000, lang = x), FALSE
    )
  }
)

informal_groups <- lapply(informal_groups, getElement, "content")

informal_groups <- lapply(informal_groups, getElement, "results")

informal_groups <- lapply(informal_groups, data.tree::as.Node)

informal_groups <- lapply(
  informal_groups, data.tree::ToDataFrameTree, "pathString", "id"
)

informal_groups <- lapply(informal_groups, function(x) x[!is.na(x[["id"]]), ])

informal_groups <- lapply(
  informal_groups,
  function(x) {
    x[["pathString"]] <- gsub("hasSubGroup/", "", x[["pathString"]])
    x
  }
)

informal_groups <- lapply(informal_groups, data.tree::as.Node)

informal_groups <- lapply(
  informal_groups, data.tree::ToDataFrameTree, "name", "id"
)

informal_groups <- lapply(informal_groups, function(x) x[-1L, ])

informal_groups <- lapply(
  informal_groups, stats::setNames, c("tree", "name", "id")
)

ord <- informal_groups[[1]][["id"]]

informal_groups <- mapply(
  function(x, y) {
    row.names(x) <- make.unique(x[["id"]], ".DUPLICATE.")
    x[["id"]] <- NULL
    x[["tree"]] <- substring(x[["tree"]], 5L)
    x[["tree"]] <- sub("°", "¦", x[["tree"]])
    names(x) <- paste(names(x), y, sep = "_")
    x
  },
  informal_groups,
  langs,
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE
)

informal_groups[[2]] <- informal_groups[[2]][ord, ]
informal_groups[[3]] <- informal_groups[[3]][ord, ]

informal_groups <- do.call(cbind, informal_groups)

class(informal_groups[["name_en"]]) <- "translation"
class(informal_groups[["name_fi"]]) <- "translation"
class(informal_groups[["name_sv"]]) <- "translation"

informal_groups_reported <- informal_groups
