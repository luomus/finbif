metadata_ranges <-
  finbif:::api_get(
    list(path = "metadata/ranges", query = list(), cache = FALSE)
  )[["content"]]

taxon_rank <- metadata_ranges[["MX.taxonRankEnum"]]
taxon_rank <- matrix(unlist(taxon_rank), ncol = 2L, byrow = TRUE)
taxon_rank <- as.data.frame(
  tolower(unique(taxon_rank[, -1L])), row.names = unique(taxon_rank[, 1L]),
  stringsAsFactors = FALSE
)
taxon_rank <- stats::setNames(taxon_rank, "name_en")
class(taxon_rank[["name_en"]]) <- "translation"

orig_taxon_rank <- taxon_rank
