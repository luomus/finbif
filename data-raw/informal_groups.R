informal_groups <- finbif:::finbif_api_get(
  "v0/informal-taxon-groups/tree", list(pageSize = 1000)
)
informal_groups <- data.tree::as.Node(informal_groups$content$results)
informal_groups <- data.tree::ToDataFrameTree(informal_groups, "pathString")
informal_groups <- informal_groups[
  !grepl("hasSubGroup$", informal_groups$pathString),
]
informal_groups$pathString <- gsub(
  "hasSubGroup/", "", informal_groups$pathString
)
informal_groups <- data.tree::as.Node(informal_groups)
informal_groups <- data.tree::ToDataFrameTree(informal_groups, "name")[-1, ]
informal_groups <- stats::setNames(informal_groups, c("tree", "name"))
informal_groups$tree <- substring(informal_groups$tree, 5)
