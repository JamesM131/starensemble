#' Model the partitions that have been made
#'
#' @param lst
#'  A list of lists. The first level represents the planar cuts,
#'  the second represents the linear cuts along the line.
#'
#'  @details
#'  This is used to create the models from the output of the
#'
#' @return
#' @export
#'
#' @examples
model_partitions <- function(lst, optim_span = FALSE, target) {
  target <- enexpr(target)
  map_depth(lst, 3, ~{
    model_mapper(df = .x, optim_span = optim_span, target = target)
  })
}



#' Predict the value for each model
#'
#' @param lst
#'
#' @return
#' @export
#'
#' @examples
predict_models <- function(lst) {
  map_depth(lst, 3, ~ {
    # browser()
    # dist_mean <- .x %>%
    #   broom::augment() %>%
    #   pull(dist) %>%
    #   mean()
    if(all(is.na(.x))) {
      return(list(prediction = NA, weight = Inf))
    }
    weight <- modelr::rmse(.x, data = broom::augment(.x))

    if(weight > 7){
      return(list(prediction = NA, weight = Inf))
    }

    prediction <- predict(.x, newdata = tibble(dist = 0))

    list(prediction = prediction, weight = weight^2) # Squaring could increase the contrast between good and bad predictions?
  })
}


model_mapper <- function(df, optim_span = FALSE, target) {

  non_na_observations <- df %>%
    filter(is.na(!!target) == FALSE) %>%
    nrow()

  if(non_na_observations <= 10) {
    return(NA)
  }

  # browser()
  if(optim_span == TRUE) {
    calcSSE <- function(x){
      loessMod <- try(eval(expr(loess(!!target ~ index, data= df, span=x))), silent=T)
      res <- try(loessMod$residuals, silent=T)
      if(class(res)!="try-error"){
        if((sum(res, na.rm=T) > 0)){
          sse <- sum(res^2)
        }
      }else{
        sse <- 99999
      }
      return(sse)
    }
    # browser()
    # Run optim to find span that gives min SSE, starting at 0.5
    opt <- optim(par=c(0.5), calcSSE, method="SANN")
    eval(expr(loess(!!target ~ dist, data = df, weights = 1/perp_dist, span = opt$par)))
  } else {
    eval(expr(loess(!!target ~ dist, data = df, weights = 1/perp_dist)))
  }
}
