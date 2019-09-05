quality_issues <- data.frame(
  options = c("with_issues", "without_issues", "both"),
  row.names = c("only_issues", "no_issues", "both"),
  stringsAsFactors = FALSE
)

class(quality_issues[["options"]]) <- "translation"

taxon_reliability  <- data.frame(
  options = c("reliable", "likely", "neutral", "unlikey", "unreliable"),
  row.names = c("reliable", "likely", "neutral", "suspicious", "unreliable"),
  stringsAsFactors = FALSE
)

class(taxon_reliability[["options"]]) <- "translation"
