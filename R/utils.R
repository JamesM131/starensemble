#' Utility abline
#'
#' @param ls
#' @param value
#' @param ...
#'
#' @description
#' This is a utility function that makes it easier to plot the results of mark_lines() function
#'
#' @return
#' @export
#'
#' @examples
util_abline <- function(ls, value = "value", ...) {
  # browser()
  # dots <- rlang::quos(...)
  if(ls$type == "vertical") {
    abline(v = ls[[value]], ...)
  } else if(ls$type == "horizontal") {
    abline(h = ls[[value]], ...)
  } else {
    abline(ls[[value]], ...)
  }
  Sys.sleep(0.5)
}



get_dist <- function(p1, p2) {
  # browser()

  # sqrt((p1[, 1] - p2[1])^2 + (p1[, 2] - p2[2])^2)

  (p1[, 1] - p2[1]) + (p1[, 2] - p2[2])
}


# df %>%
#   filter(y < predict(model, x))

#' Distance from point to line
#'
#' @details
#' The points b and c have a line travelling through them.
#' This function calculates the distance from  point a to this line.
#'
#' All arguments must be  of the form c(x, y, z)
#'
#'
#' @param a point
#' @param b line point 1
#' @param c line point 2
#'
#' @return
#' @export
#'
#' @examples
dist3d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  v3 <- cross3d_prod(v1,v2)
  area <- sqrt(sum(v3*v3))/2
  d <- 2*area/sqrt(sum(v1*v1))
}

cross3d_prod <- function(v1,v2){
  v3 <- vector()
  v3[1] <- v1[2]*v2[3]-v1[3]*v2[2]
  v3[2] <- v1[3]*v2[1]-v1[1]*v2[3]
  v3[3] <- v1[1]*v2[2]-v1[2]*v2[1]
  return(v3)
}


switch_names <- function(df, name1, name2){
  name1 <- enquo(name1)
  name2 <- enquo(name2)

  name1_text <- quo_name(name1)
  name2_text <- quo_name(name2)


  df %>%
    rename(!!name1_text := !!name2, !!name2_text := !!name1)
}


