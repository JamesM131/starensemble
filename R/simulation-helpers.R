sim_time <- function(n = 1) {
  sample(sample_pol$date_time, n)
}

sim_lon <- function(n = 1) {
  sample(sample_pol$lon, n)
}

sim_lat <- function(n = 1) {
  sample(sample_pol$lat, n)
}

sim_spatial <- function(n = 1) {
  points <- map2(sample_pol$lon, sample_pol$lat, c)
  pt_index <- sample(length(points), n)
  points[[pt_index]]
}


sim_space_time <- function(n = 1, ppm10 = FALSE) {
  if(ppm10) {
    sample_pol <- sample_pol %>%
      filter(is.na(ppm10) == FALSE)
  }
  points <- map2(sample_pol$lon, sample_pol$lat, c)
  pt_index <- sample(length(points), n)
  spatial <- points[[pt_index]]

  time_bank <- sample_pol %>%
    filter(lon == spatial[[1]], lat == spatial[[2]]) %>%
    pull(date_time)

  time <- sample(time_bank, n)

  list(space = spatial, time = time)
}
