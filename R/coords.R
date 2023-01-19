#' @noRd

coords <- function(obj) {

  obj <- as.list(obj)

  nms <- c("lat", "lon", "system", "ratio")

  obj_seq <- seq_along(obj)

  names(obj) <- nms[obj_seq]

  lat <- obj[["lat"]]

  lon <- obj[["lon"]]

  ans <- paste(lat, lon, sep = ":")

  sys <- obj[["system"]]

  has_sys <- !is.null(sys)

  if (has_sys) {

    sys <- toupper(sys)

    systems <- c("WGS84", "EUREF", "YKJ")

    sys <- match.arg(sys, systems)

    ans <- paste(ans, sys, sep = ":")

    ratio <- obj[["ratio"]]

    has_ratio <- !is.null(ratio)

    if (has_ratio) {

      ans <- paste(ans, ratio, sep = ":")

    }

  }

  ans

}
