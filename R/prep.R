scale_data <- function(df) {
  if(all(c("lon", "lat") %in% colnames(df))) {
    df <- df %>%
      rename(x = lon, y = lat)
  }

  my_scale <- function(var) {
    var %>%
      as.numeric() %>%
      scale() %>%
      as.numeric()
  }

  df %>%
    mutate_at(c("x", "y", "date_time"), .funs = my_scale)

}


prep_data <- function(df) {
  scale_data(df)
}
