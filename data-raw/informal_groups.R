informal_group <- finbif:::finbif_api_get(
  "informal-taxon-groups/tree", list(pageSize = 1000), FALSE
)
informal_group <- data.tree::as.Node(informal_group[["content"]][["results"]])
informal_group <-
  data.tree::ToDataFrameTree(informal_group, "pathString", "id")
informal_group <- informal_group[!is.na(informal_group[["id"]]), ]
informal_group[["pathString"]] <-
  gsub("hasSubGroup/", "", informal_group[["pathString"]])
informal_group <- data.tree::as.Node(informal_group)
informal_group <-
  data.tree::ToDataFrameTree(informal_group, "name", "id")[-1L, ]
informal_group <- stats::setNames(informal_group, c("tree", "name", "id"))
row.names(informal_group) <- informal_group[["id"]]
informal_group[["id"]] <- NULL
informal_group[["tree"]] <- substring(informal_group[["tree"]], 5L)
class(informal_group[["name"]]) <- "translation"
informal_group_reported <- informal_group
