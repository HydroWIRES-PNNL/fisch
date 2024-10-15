#' DP
#'
#' @description Weekly release and storage operations guided by ISTARF wrapping around DP sub-daily scheduling.
#' @param n_periods number of sub-daily periods
#' @param inflow_forecast inflow forecast
#' @param S_upper_limit upper limit of storage
#' @param S_lower_limit lower limit of storage
#' @param R_disc_x release discretization
#' @param S_States storage stages
#' @param S_end_state storage end state
#' @param price_forecast price forecast
#' @param use_storage_target T/F on whether to use storage target
#' @param State_mat storage state matrix
#' @param Rev_to_go placeholder matrix for revenue to go
#' @param Bellman placeholder matrix for Bellman's function
#' @param Policy placeholder matrix for release policy
#'
DP <- function(n_periods,
               inflow_forecast,
               S_upper_limit,
               S_lower_limit,
               R_disc_x,
               S_states,
               State_mat,
               S_end_state,
               price_forecast,
               Rev_to_go,
               Bellman,
               Policy,
               use_storage_target){

  for (t in n_periods:1) {
    Release_mat <- matrix(rep(R_disc_x, length(S_states)),
                          ncol = length(R_disc_x), byrow = TRUE)

    Balance_mat <- State_mat + inflow_forecast[t]
    Balance_mat[which(Balance_mat < S_lower_limit)] <- NA
    #Balance_mat[which(Balance_mat > S_upper_limit)] <- NA
    #Balance_mat[which(Balance_mat < S_lower_limit)] <- S_lower_limit
    Balance_mat[which(Balance_mat > S_upper_limit)] <- S_upper_limit
    Implied_S_state <- round(1 + (((Balance_mat - S_lower_limit) / (S_upper_limit - S_lower_limit)) * (length(S_states) - 1)))

    Rev_mat <- Release_mat * price_forecast[t]
    Rev_mat2 <- Rev_mat + matrix(Rev_to_go[Implied_S_state],
                                 nrow = length(S_states))

    # create penalty for deviation from target storage
    if(use_storage_target == T & t == n_periods){
      -(1e6 * abs(Implied_S_state - S_end_state) ^ 1) -> penalties
      Rev_mat2 <- Rev_mat2 + penalties
    }

    Rev_mat2[is.na(Rev_mat2)] <- -Inf

    Rev_to_go <- apply(Rev_mat2, 1, max, na.rm = TRUE)
    Bellman[, t] <- Rev_to_go
    Policy[, t] <- max.col(Rev_mat2, ties.method = "first")#apply(Rev_mat2, 1, which.max)
  }

  return(Policy)
}


#' simulate_DP_policy
#'
#' @description Simulate the week-ahead
#' @param n_periods number of sub-daily periods
#' @param release_policy release policy
#' @param R_disc_x release discretization
#' @param R release vector
#' @param Q inflow vector
#' @param S storage vector
#' @param S_cap storage capacity constant
#' @param S_states storage states
#' @param Spill spill vector
#' @param price_forecast price forecast
#' @param MWh_per_MCM energy value of release
#' @param Revenue revenue vector
#' @param t_to_day timestep to day of week conversion
#' @param Policy placeholder matrix for release policy
#'
simulate_DP_policy <- function(
    n_periods,
    release_policy,
    R_disc_x,
    R, Q, S, S_cap,
    S_states,
    Spill,
    price_forecast = price_forecast,
    MWh_per_MCM = MWh_per_MCM,
    Revenue,
    t_to_day
    ){

  for (t in 1:n_periods) {

    # get day of week
    t_to_day[["day"]][t] -> day

    #message(paste0(day, " ", t))

    if(!is.list(release_policy)){
      # if policy is not a list then it's a pre-determined schedule
      R[t] <- release_policy[t]

    }else{
      # select release policy for day
      R_policy <- release_policy[[day]]

      # determine storage state (use character to ensure clean match)
      #S_state <- which(as.character(S_states) == as.character(S[t]))

      # NEED TO FIX THIS ROUNDING ISSUE!

      #S_state <- which(S_states == S[t])
      S_state <- which.min(abs(S_states - S[t]))

      #print(paste0(S[t], "   " , S_state))

      # select release
      R[t] <- R_disc_x[R_policy[S_state, t]]
    }


    # release cannot exceed availability
    if(R[t] > Q[t] + S[t]) R[t] <- Q[t] + S[t]

    # storage cannot exceed capacity
    if ( (S[t] - R[t] + Q[t]) > S_cap) {
      S[t + 1] <- S_cap
      Spill[t] <- S[t] - R[t] + Q[t] - S_cap
    } else {
      # storage cannot be negative
      S[t + 1] <- max(0, S[t] - R[t] + Q[t])
    }

    Revenue[t] <- R[t] * price_forecast[t] * MWh_per_MCM

  }

  tibble(
    time = 1:(n_periods + 1),
    storage_sim = S,
    release_spill = c(Spill,NA),
    release_turbine = c(R, NA),
    benefit_revenue = c(Revenue, NA),
    price_price = c(price_forecast, NA),
    inflow_actual = c(Q, NA)
    #inflow_forecast = c(inflow_forecast, NA),
    # inflow_forecast1 = c(forecast_set_[1,], NA),
    # inflow_forecast2 = c(forecast_set_[2,], NA),
    # inflow_forecast3 = c(forecast_set_[3,], NA),
    # inflow_forecast4 = c(forecast_set_[4,], NA),
    # inflow_forecast5 = c(forecast_set_[5,], NA),
    # inflow_forecast6 = c(forecast_set_[6,], NA),
    # inflow_forecast7 = c(forecast_set_[7,], NA),
  )

}

