informal_groups <- finbif:::finbif_api_get(
  "informal-taxon-groups/tree", list(pageSize = 1000), FALSE
)
informal_groups <- data.tree::as.Node(informal_groups[["content"]][["results"]])
informal_groups <-
  data.tree::ToDataFrameTree(informal_groups, "pathString", "id")
informal_groups <- informal_groups[!is.na(informal_groups[["id"]]), ]
informal_groups[["pathString"]] <-
  gsub("hasSubGroup/", "", informal_groups[["pathString"]])
informal_groups <- data.tree::as.Node(informal_groups)
informal_groups <-
  data.tree::ToDataFrameTree(informal_groups, "name", "id")[-1L, ]
informal_groups <- stats::setNames(informal_groups, c("tree", "name", "id"))
row.names(informal_groups) <- informal_groups[["id"]]
informal_groups[["id"]] <- NULL
informal_groups[["tree"]] <- substring(informal_groups[["tree"]], 5L)
