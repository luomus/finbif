#' @noRd

coords <- function(obj) {

  obj <- as.list(obj)

  nms <- c("lat", "lon", "system", "ratio")

  names(obj) <- nms[seq_along(obj)]

  coords <- paste(obj[["lat"]], obj[["lon"]], sep = ":")

  if (is.null(obj[["system"]])) return(coords)

  obj[["system"]] <- toupper(obj[["system"]])

  obj[["system"]] <- match.arg(obj[["system"]], c("WGS84", "EUREF", "YKJ"))

  if (is.null(obj[["ratio"]])) return(paste(coords, obj[["system"]], sep = ":"))

  paste(coords, obj[["system"]], obj[["ratio"]], sep = ":")

}
