dist2d <- function(x, ...){
  UseMethod('dist2d')
}

#' Distance points
#'
#' @param a
#' @param b
#' @param c
#'
#' @return
#' @export
#'
#' @examples
dist2d.default <- function(x, b, .c) {
  v1 <- b - x
  v2 <- .c - b
  m <- cbind(v1,v2) # Problem is here, v2 is a data frame. Consider vectorising this function somehow. For now, simply looping over all points should work.
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}


#' Perpendicular distance with an lm
#'
#' @param x
#' @param c
#'
#' @return
#' @export
#'
#' @examples
dist2d.lm <- function(x,  x1, y1) {

  point1 <- c(0, predict(x, data.frame(x = 0)))
  point2 <- c(1, predict(x, data.frame(x = 1)))

  # new_c  <- select(.c, x, y)
  pt_lst <- map2(x1, y1, ~c(.x, .y))

  map_dbl(pt_lst, ~dist2d(point1, point2, .x))
  # dist2d(point1, point2, new_c)
}
