#' @noRd

coords <- function(lat, lon, system, ratio) {
  coords <- paste(c(lat, lon), collapse = ":")
  if (missing(system)) return(coords)
  system <- toupper(gsub("^kkj$", "ykj", system))
  system <- match.arg(system, c("WGS84", "EUREF", "YKJ"))
  if (missing(ratio)) return(paste(coords, system, sep = ":"))
  paste(coords, system, ratio, sep = ":")
}
