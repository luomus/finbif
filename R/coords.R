#' @noRd
coords <- function(obj) {
  obj <- as.list(obj)
  nms <- c("lat", "lon", "system", "ratio")
  obj_seq <- seq_along(obj)
  names(obj) <- nms[obj_seq]

  lat <- paste(obj[["lat"]], collapse = ":")
  lon <- paste(obj[["lon"]], collapse = ":")
  ans <- paste(lat, lon, sep = ":")

  if (!is.null(obj[["system"]])) {
    sys <- toupper(obj[["system"]])
    systems <- c("WGS84", "EUREF", "YKJ")
    sys <- match.arg(sys, systems)
    ans <- paste(ans, sys, sep = ":")

    if (!is.null(obj[["ratio"]])) {
      ans <- paste(ans, obj[["ratio"]], sep = ":")
    }
  }

  ans
}
