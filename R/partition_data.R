#' Create Partitions
#'
#' @param df
#' @param ...
#'
#' @return
#' partitions
#' @export
#'
#' @examples
create_partition <- function(df, ...) {
  UseMethod("create_partition")
}

#' Create Partition
#'
#' @param df a data frame
#' @param time time of event
#' @param lon lon of event
#' @param lat lat of event
#'
#' @return
#' partitions
#' @export
#'
#' @examples
create_partition.default <- function(df, time, lon, lat) {
  point_list <- map2(lon, lat, c)

  encapsulating_dfs <- map2(point_list, time, ~get_encapsulating_data(df, .y, .x[[1]], .x[[2]]))
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = 25, clear = FALSE, width= 60
  )


  encapsulating_dfs %>%
    map(~{
      # print(glue::glue("{.y} of 25"))
      # pb$tick()
      .x %>%
        prep_data() %>%
        cut_circle(pb = pb) %>%
        cut_plane()
    })

}

get_encapsulating_data <- function(data, time_index, lon_pt, lat_pt) {
  # First I need to get all of the data in a certain lagged period
  # browser()
  lag_periods <- lubridate::days(4) # 4 days on either side of point.
  # browser()
  point <- c(lon_pt, lat_pt)
  coord_km_dist <- 500 # Within 100 km of the point
  coord_dist <- coord_km_dist * 1000
  # browser()

  data %>%
    # filter(geosphere::distHaversine(point, matrix(c(lon, lat), ncol = 2)) < coord_dist) %>%
    filter(date_time <= time_index, date_time > time_index - lag_periods)
}




#' Create a list of partitioned data frames for lm models
#'
#' @param df data frame with a
#' @param n_par the number of partitions to create, defaults to 10
#'
#' @return
#' list of data frames (length of n_par)
#' @export
#'
#' @examples
#'
create_partition.sf <- function(df, n_par = 10, centre, geometry_col) {
  geometry_col <- rlang::enquo(geometry_col)

}




#' Cut the circle into a list of useable tibbles
#'
#' @param df
#' @param cuts
#' @param point
#'
#' @details The variable that should be used in modelling is distance along
#' with anyany additional predictors in the original data frame.
#'
#' These should be given in formula notation to the modelling functions.
#'
#'
#' @return
#' @export
#'
#' @examples
cut_circle <- function(df, cuts = 5, point = c(0, 0), pb = NULL) {
  # browser()
  if(is.null(pb) == FALSE) {
    pb$tick()
  }
  interval <- 2
  lines <- mark_lines(n_lines = cuts, centre = point, interval = interval)

  lines_lms <- lines %>% purrr::map(5)

  #
  # lines %>%
  #   purrr::map(5) %>%
  #   purrr::map(~predict(.x, newdata = df))

  #

  map(lines_lms, ~cut_circle_mapper(.x, df, interval))

  # mutate(dist = get_dist(c(min(x), line_y[which(min(x) == x)]), cbind(x, line_y)))
  # mutate(dist = get_dist(c(min(x))))
  # # mutate(line_y = predict(.x, df), residual = abs(line_y - y)) %>%
  # mutate(perp_dist = dist2d(.x, x, y)) %>%

  # mutate(x_along = x + min(x))

  # map(lines, ~{
  #   df %>%
  #     filter(x < )
  # })
  # return(lines_lms)
}

create_circle <- function(n_points = 500) {
  theta <- runif(n_points, max = 2*pi)
  # hist(theta)
  date_time <-

  x <- sample(runif(n_points)*cos(theta))
  y <- sample(runif(n_points)*sin(theta))
  # y <- runif(n_points)*sin(theta)
  tibble::tibble(x = x, y = y, val = x^2 + y^2 + 0.2*rnorm(length(x)))

}

#' Mark the lines that will be used to cut the circle
#'
#'
#' @param n_lines
#' @param centre
#'
#' @details This function generates a set of lines that divide a circle and will
#' be used to make predictions. points will eventually be weighted based on their
#'
#' @return
#'
#'
#' @export
#'
#' @examples
mark_lines <- function(n_lines = 5, centre = c(0, 0), interval = 0.5) {
  #
  marks <- head(seq(0, pi, by = pi/n_lines), -1)

  x <- cos(marks)
  y <- sin(marks)

  lms <- marks %>%
    purrr::map(~{
      #
      y <- sin(.x)
      x <- cos(.x)

      df <- tibble::tibble(
        x = c(centre[1], x),
        y = c(centre[2], y)
      )

      #
      lm(y ~ x, data = df)
    })

  lines <- lms %>%
    purrr::map(~{
      #
      purrr::when(.x,
        # coef(.)[2] == 0 ~ list(value = c(coef(.)[1], coef(.)[2]), upper = coef(.)[1] + 1, lower = coef(.)[1] - 1, type = "horizontal"),
        coef(.)[2] == 0 ~ list(value = coef(.)[1],
                               upper = coef(.)[1] + interval,
                               lower = coef(.)[1] - interval,
                               type = "horizontal",
                               line = .),
        # coef(.)[2] > 1e10 ~ list(value = c(0, 1e10), upper = c(1, 1e10), type = "vertical"), ## keep going here
        coef(.)[2] > 1e10 ~ list(value = 0,
                                 upper = 0 + interval,
                                 lower = 0 - interval,
                                 type = "vertical",
                                 line = .), ## keep going here
        TRUE ~ list(value = coef(.),
                    upper = c(coef(.)[1] + interval, coef(.)[2]),
                    lower = c(coef(.)[1] - interval, coef(.)[2]),
                    type = "line",
                    line = .)
      )
    })
  return(lines)
}


#' Mark planes evenly around a spherical (or elipsoid) object
#'
#' @param n_planes
#' @param centre
#'
#' @return
#' @export
#'
#' @examples
#' mark_planes(5)
mark_planes <- function(n_planes, centre = c(0, 0, 0)) {
  if(n_planes < 4) {
    stop("too few planes specified")
  }
  #
  marks <- head(seq(0, pi, by = pi/n_planes), -1)

  x <- cos(marks)
  y <- sin(marks)

  lms <- marks %>%
    purrr::map(~{
      #
      y <- sin(.x)
      x <- cos(.x)

      df <- tibble::tibble(
        x = c(centre[1], x),
        y = c(centre[2], y)
      )

      #
      lm(y ~ x, data = df)
    })

  lines <- lms %>%
    purrr::map(~{
      #
      purrr::when(.x,
                  # coef(.)[2] == 0 ~ list(value = c(coef(.)[1], coef(.)[2]), upper = coef(.)[1] + 1, lower = coef(.)[1] - 1, type = "horizontal"),
                  coef(.)[2] == 0 ~ list(value = coef(.)[1],
                                         upper = coef(.)[1] + interval,
                                         lower = coef(.)[1] - interval,
                                         type = "horizontal",
                                         line = .),
                  # coef(.)[2] > 1e10 ~ list(value = c(0, 1e10), upper = c(1, 1e10), type = "vertical"), ## keep going here
                  coef(.)[2] > 1e10 ~ list(value = 0,
                                           upper = 0 + interval,
                                           lower = 0 - interval,
                                           type = "vertical",
                                           line = .), ## keep going here
                  TRUE ~ list(value = coef(.),
                              upper = c(coef(.)[1] + interval, coef(.)[2]),
                              lower = c(coef(.)[1] - interval, coef(.)[2]),
                              type = "line",
                              line = .)
      )
    })


}


cut_planes_full <- function(df, cuts = 5, point = c(0, 0)){
  # df must have lat lon and date_time columns

  # if col names of df contain x and y then rename them to lon and lat !!!!!!!!
  if(all(c("x", "y") %in% colnames(df))) {
    df <- df %>%
      rename(lon = x, lat = y)
  }

  if(!all(c("lat", "lon", "date_time") %in% colnames(df))) {
    stop("Please include lat lon and date_time columns")
  }

  # Scale to unit sphere
  # This isnt actually a unit sphere as it isnt centered at 0
  # TODO: Fix this ^^^^

  # Define x z lines that represent planes cutting the sphere
  plane_lines <- mark_lines(n_lines = 5)

  # Prepare params for cut_circle_planes
  n_lines = 5
  interval = 0.5
  point = c(0, 0)


  cut_planes <- df_scaled %>%
    rename(x = lon, y = lat) %>%
    switch_names(x, date_time) %>%
    cut_circle() %>%
    map(~switch_names(.x, x, date_time))

  # Pass each of these lines to a cut circle function
  plane_lines %>%
    map(~cut_circle_planes(.x, df_scaled, n_lines, interval, centre = point))
}


cut_circle_planes <- function(line, df, n_lines, interval, centre){

  # These are the lines within planes
  new_lines <- mark_lines(n_lines = n_lines, centre = centre, interval = interval)

  lines_lms <- new_lines %>% purrr::map(5)


  # Get the begining and end x, y and z
  x_start <- line

  #
  # lines %>%
  #   purrr::map(5) %>%
  #   purrr::map(~predict(.x, newdata = df))

  #
  map(lines_lms, ~{
    # browser()
    df %>%
      mutate(line_y = predict(.x, df), residual = abs(line_y - y)) %>%
      filter(residual < interval) %>%
      mutate(dist = get_dist(c(min(x), line_y[which(min(x) == x)]), cbind(x, line_y)))
  })

}


#' Create/simulate a plane with 3 dimensinos and a value level
#'
#' @param n_points The number of points to simulate
#'
#' @return
#' tbl_df
#' @export
#'
#' @examples
create_plane <- function(n_points = 500){
  circle <- create_circle(n_points = n_points)
  date_start <- lubridate::ymd_hms("2016-10-12 10:00:00")
  dates <- seq(date_start, by = "hour", length.out = nrow(circle)/10) %>%
    rep(10) %>%
    sample()

  circle %>%
    mutate(date_time = dates)
}


cut_plane <- function(df, cuts, point) {


  # scaled_data <- df %>%
  #   map(~{
  #     map_at(.x, c("lat", "lon", "date_time"), ~{
  #       num <- as.numeric(.x)
  #
  #       min_num <- min(num)
  #
  #       new_num <- num - min_num
  #
  #       new_num/max(new_num)
  #     }) %>%
  #       bind_cols()
  #   })
  # df_scaled <- map_at(df, c("lat", "lon", "date_time"), ~{
  #   num <- as.numeric(.x)
  #
  #   min_num <- min(num)
  #
  #   new_num <- num - min_num
  #
  #   new_num/max(new_num)
  # }) %>%
  #   bind_cols()


  df %>%
    map(~switch_names(.x, x, date_time)) %>%
    map(~{
      cut_circle(.x) %>%
        map(~switch_names(.x, x, date_time))
    })
}
