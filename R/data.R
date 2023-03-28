#' Border of Finland
#'
#' Vertices of the Finnish border at the (approximately) 1km scale. The finbif
#' package mapping utilities have been deprecated and will be removed in the
#' next release.
#'
#' @format A list:
#' \describe{
#'   \item{vertices}{A matrix of the vertices, in decimal degrees (WGS84) of the
#'     Finnish border at the (approximately) 1km scale.}
#'   \item{bbox}{A vector of coordinates for a box bounding the Finnish border.}
#'   ...
#' }
#' @examples
#' library(finbif)
#' with(
#'   finland_map,
#'   {plot.new()
#'    plot.window(
#'      bbox[c(1, 3)],
#'      bbox[c(2, 4)],
#'      asp = 2.4
#'    )
#'    polygon(vertices)}
#' )
#' @source \url{https://www.stat.fi/org/avoindata/paikkatietoaineistot.html}
"finland_map"
