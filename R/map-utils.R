#' 2-Dimensional Histogram
#'
#' Compute a histogram from xy data given set of breakpoints.
#'
#' @param xy Numeric. A `data.frame`, or object coercible to a `data.frame`
#'   with `as.data.frame()`, where the columns of the `data.frame` are,
#'   or can be, interpreted as XY coordinates.
#' @param breaks List. A list of vectors, one for each dimension of `xy`, where
#'   each vector gives the breakpoints between the histogram cells.
#'
#' @return A list with three components:
#' - `x` The X dimension breakpoints.
#' - `y` The Y dimension breakpoints.
#' - `z` A frequency table of the number of `xy` points in each cell.
#'
#' @examples
#' set.seed(1L)
#' hist_xy(matrix(runif(50), 25), list(seq(0, 1, .2), seq(0, 1, .2)))
#' @export

hist_xy <- function(xy, breaks) {

  nms <- c("x", "y")

  xy <- as.data.frame(xy)

  xy <- structure(xy, names = nms)

  breaks <- structure(breaks, names = nms)

  levels <- vapply(breaks, length, 0L)

  levels <- levels - 1L

  levels <- lapply(levels, seq_len)

  z <- mapply(findInterval, xy, breaks, SIMPLIFY = FALSE)

  z <- mapply(factor, z, levels, SIMPLIFY = FALSE)

  z <- table(z)

  z <- list(z = z)

  c(breaks, z)

}

#' Create XY Breakpoints
#'
#' Creates sets of equally spaced breakpoints for XY coordinate data.
#'
#' @param bbox Numeric vector. A vector of points of the form,
#'   `c(xmin, ymin, xmax, ymax)` giving the outer limits of the breakpoints when
#'   expanded to nearest multiple of `size`.
#' @param size Numeric. The size of the cells between the breakpoints.
#'
#' @return A list with two components:
#' - `x` The X dimension breakpoints.
#' - `y` The Y dimension breakpoints.
#' @examples
#' breaks_xy(c(5, -45, 67, 100), 10)
#' @export

breaks_xy <- function(bbox, size) {

  sqx <- c(1L, 3L)

  sqy <- c(2L, 4L)

  x <- list(bbox = bbox, sq = sqx, size = size)

  y <- list(bbox = bbox, sq = sqy, size = size)

  x <- breaks_x(x)

  y <- breaks_x(y)

  list(x = x, y = y)

}

#' @noRd

breaks_x <- function(obj) {

  bbox <- obj[["bbox"]]

  sq <- obj[["sq"]]

  size <- obj[["size"]]

  x <- bbox[sq]

  x <- x / size

  x1 <- x[[1L]]

  x1 <- floor(x1)

  x1 <- x1 * size

  x2 <- x[[2L]]

  x2 <- ceiling(x2)

  x2 <- x2 * size

  seq(x1, x2, size)

}
