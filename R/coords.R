#' @noRd

coords <- function(obj) {

  obj <- as.list(obj)

  nms <- c("lat", "lon", "system", "ratio")

  obj_seq <- seq_along(obj)

  names(obj) <- nms[obj_seq]

  lat <- paste(obj[["lat"]], collapse = ":")

  lon <- paste(obj[["lon"]], collapse = ":")

  ans <- paste(lat, lon, sep = ":")

  has_sys <- !is.null(obj[["system"]])

  if (has_sys) {

    sys <- toupper(obj[["system"]])

    systems <- c("WGS84", "EUREF", "YKJ")

    sys <- match.arg(sys, systems)

    ans <- paste(ans, sys, sep = ":")

    has_ratio <- !is.null(obj[["ratio"]])

    if (has_ratio) {

      ans <- paste(ans, obj[["ratio"]], sep = ":")

    }

  }

  ans

}
