quality_issues <- data.frame(
  options = c("with_issues", "without_issues", "both"),
  row.names = c("only_issues", "no_issues", "both"),
  stringsAsFactors = FALSE
)

class(quality_issues[["options"]]) <- "translation"

collection_quality  <- data.frame(
  options = c("professional", "hobbyist", "amateur"),
  row.names = c("professional", "hobbyist", "amateur"),
  stringsAsFactors = FALSE
)

class(collection_quality[["options"]]) <- "translation"

record_reliability  <- data.frame(
  options = c("reliable", "unassessed", "unreliable"),
  row.names = c("reliable", "undefined", "unreliable"),
  stringsAsFactors = FALSE
)

class(record_reliability[["options"]]) <- "translation"

record_quality  <- data.frame(
  options = c(
    "expert_verified", "community_verified", "unassessed", "uncertain",
    "errorneous"
  ),
  row.names = c(
    "expert_verified", "community_verified", "neutral", "uncertain",
    "errorneous"
  ),
  stringsAsFactors = FALSE
)

class(record_quality[["options"]]) <- "translation"

superrecord_basis <- data.frame(
  options = c("human_observation", "machine_observation", "specimen"),
  row.names = c(
    "human_observation_unspecified", "machine_observation_unspecified",
    "preserved_specimen"
  ),
  stringsAsFactors = FALSE
)

class(superrecord_basis[["options"]]) <- "translation"

supported_langs <- c(
  English           = "en",
  Finnish           = "fi",
  Swedish           = "sv",
  Russian           = "ru",
  `Sami (Northern)` = "se"
)
