
# %.in% operator
# version of %in% that evades machine tolerance issue
`%.in%` = function(a, b, eps = .Machine$double.eps ^ 0.5) {
  any(abs(b-a) <= eps)
}

#' snap_to_disc
#'
#' @description Prepare model inputs by snapping to desired disretization level.
#' @param x data to be disretized (either scalar or vector)
#' @param discretization_step default 0.2. Other water volume variables (inflow, inflow_forecast, reservoir_capacity, etc.) must all be discretized at this level.
#' @export
#'
snap_to_disc <- function(x, discretization_step){

  if(length(x) == 1){
    discs <- seq(0, x + discretization_step, discretization_step)
    return(
      discs[which.min(abs(discs - x))]
    )
  }else{
    sapply(x, snap_to_disc, discretization_step = discretization_step)
  }
}

#' convert_forecast_to_fisch_format
#'
#' @description Prepare model inputs by snapping to desired disretization level.
#' @param forecast vector of forecasted inflows of length equal to length(inflow)
#' @export
#'
convert_forecast_to_fisch_format <- function(forecast){
    matrix(
    c(inflow, rep(NA, 6 * length(forecast))),
    nrow = 7, byrow = TRUE
  )
}
