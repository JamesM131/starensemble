eval_split <- function(df, spatial_index, time) {
  browser()
  test <-  df %>%
    dplyr::filter(lon == spatial_index[[1]], lat == spatial_index[[2]], date_time == time)

  train <- df %>%
    dplyr::filter(lon != spatial_index[[1]], lat != spatial_index[[2]])

  list(train = train, test = test)
}


eval_model <- function(split){
  browser()
  train <- split$train
  test <- split$test

  predictions <- create_partition(train, test$date_time, test$lon, test$lat) %>%
    model_partitions() %>%
    predict_models() %>%
    flatten() %>%
    flatten_df()

  final_pred <- weighted.mean(predictions$prediction, w = predictions$weight, na.rm = TRUE)

  original <- test$ppm10

  list(prediction = final_pred, actual = original, train = train)
}


