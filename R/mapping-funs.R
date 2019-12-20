cut_circle_mapper <- function(.x, df, interval) {

  gradient_original <- coef(.x)[[2]]

  if("perp_dist" %in% colnames(df)) {
    # if("old_dist" %in% colnames(df)) {
    #   if(!all(df$old_dist == 0)) {
    #
    #   }
    # }

    if(!("old_dist" %in% colnames(df))) {
      #
      df <- df %>%
        mutate(old_dist = perp_dist)
    } else if(all(df$old_dist == 0)) {

      df <- df %>%
        mutate(old_dist = perp_dist)
    }
  } else {
    #
    df <- df %>%
      mutate(old_dist = 0)
  }
  if(gradient_original == 0) {

    df2 <- df %>%
      mutate(new_x = x, new_y = predict(.x, df), perp_dist = abs(new_y - y))
  }
  else{
    gradient_inverse <- -1/gradient_original
    points <-  map2(df$x, df$y, ~c(.x, .y))
    # This needs to be a list of x y paris. i had this somewhere, I think in the dist2d func
    point_2s <- map(points, ~ {
      x = .x[[1]] + 1
      y = .x[[2]] + gradient_inverse
      c(x, y)
    })


    #map over them to create individual lines
    original_line <- .x
    new_points <- map2(points, point_2s, ~{
      line_tibble <- tibble(
        x = c(.x[[1]], .y[[1]]),
        y = c(.x[[2]], .y[[2]])
      )
      inverse_fit <- lm(y ~ x, line_tibble)

      a <- coef(original_line)-coef(inverse_fit) ###
      x <- -a[[1]]/a[[2]]
      c(x = x, y = coef(original_line)[[2]]*x + coef(original_line)[[1]])
    })

    new_tbl <- new_points %>%
      map(~tibble(new_x = .x[[1]], new_y = .x[[2]])) %>%
      bind_rows()

    df2 <- new_tbl %>%
      bind_cols(df) %>%
      mutate(perp_dist = dist2d(.x, x, y))
  }

  df2 %>%
    filter(perp_dist < interval) %>%
    mutate(
      perp_dist = perp_dist + old_dist,
      dist  = get_dist(cbind(x, y), c(0, 0)) # Replace this with carried central point
    )

    # mutate(dist = get_dist(c(min(x), new_y[which(min(x) == x)]), cbind(x, new_y)))
}
