metadata_ranges <-
  finbif:::finbif_api_get("v0/metadata/ranges", list(), FALSE)$content

taxon_ranks <- metadata_ranges[["MX.taxonRankEnum"]]
taxon_ranks <- matrix(unlist(taxon_ranks), ncol = 2L, byrow = TRUE)
taxon_ranks <- as.data.frame(
  tolower(taxon_ranks[, -1]), row.names = taxon_ranks[, 1],
  stringsAsFactors = FALSE
)
taxon_ranks <- stats::setNames(taxon_ranks, "rank")
