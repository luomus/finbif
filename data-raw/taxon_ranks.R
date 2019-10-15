metadata_ranges <-
  finbif:::finbif_api_get("metadata/ranges", list(), FALSE)[["content"]]

taxon_rank <- metadata_ranges[["MX.taxonRankEnum"]]
taxon_rank <- matrix(unlist(taxon_rank), ncol = 2L, byrow = TRUE)
taxon_rank <- as.data.frame(
  tolower(unique(taxon_rank[, -1L])), row.names = unique(taxon_rank[, 1L]),
  stringsAsFactors = FALSE
)
taxon_rank <- stats::setNames(taxon_rank, "rank")
class(taxon_rank[["rank"]]) <- "translation"
