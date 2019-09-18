quality_issues <- data.frame(
  options = c("with_issues", "without_issues", "both"),
  row.names = c("only_issues", "no_issues", "both"),
  stringsAsFactors = FALSE
)

class(quality_issues[["options"]]) <- "translation"

taxon_reliability  <- data.frame(
  options = c("reliable", "likely", "neutral", "unlikely", "unreliable"),
  row.names = c("reliable", "likely", "neutral", "suspicious", "unreliable"),
  stringsAsFactors = FALSE
)

class(taxon_reliability[["options"]]) <- "translation"

superrecord_basis <- data.frame(
  options = c("human_observation", "machine_observation", "specimen"),
  row.names = c(
    "human_observation_unspecified", "machine_observation_unspecified",
    "preserved_specimen"
  ),
  stringsAsFactors = FALSE
)

class(superrecord_basis[["options"]]) <- "translation"
