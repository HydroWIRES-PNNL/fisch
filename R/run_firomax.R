#' schedule_release
#'
#' @description Optimizes then simulates one week of sub-daily release.
#' @param mode "single_fixed" => single forecast used at start of week; release is based on that forecast | "single_adaptive" => single forecast used at start of week, but release responds to policy during simulation (i.e., updates with storage) | "rolling_adaptive" => policy is updated daily with new forecast information; simulation is rolling horizon.
#' @param inflow actual inflow time series.
#' @param inflow_forecast inflow forecast with daily updating and same resolution as inflow. If NULL, perfect forecast is applied.
#' @param price_foreacst price forecast with same temporal resolution as inflow.
#' @param release_value power value of turbined water in MWh per MCM.
#' @param initial_storage storage level at start of week.
#' @param reservoir_capacity res
#' @param target_end_of_week_storage storage level targeted for end of week
#' @param max_release maximum
#' @param min_release minimum
#' @param discretization_step default 0.1 (allowable levels 0.0001, 0.001, 0.01, 0.1, 1, 10)
#' @param use_storage_target T/F
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export
#'
schedule_release <-  function(mode = "single_fixed",
                              inflow = TEST_inflow,
                              inflow_forecast = TEST_inflow_forecast,
                              price_forecast = TEST_price_forecast,
                              MWh_per_MCM = TEST_MWh_per_MCM,
                              initial_storage = TEST_initial_storage,
                              reservoir_capacity = TEST_reservoir_capacity,
                              target_end_of_week_storage = TEST_target_end_of_week_storage,
                              max_release = TEST_max_release,
                              min_release = TEST_min_release,
                              discretization_step = 0.1,
                              use_storage_target = TRUE){

  length(inflow) -> n_periods
  stopifnot(length(price_forecast) == n_periods)
  stopifnot(discretization_step %in% c(0.0001, 0.001, 0.01, 0.1, 1, 10))

  # set up discretization for release and storage

  rounding_level <- case_when(
    discretization_step == 10 ~ -1L,
    discretization_step == 1 ~ 0L,
    discretization_step == 0.1 ~ 1L,
    discretization_step == 0.01 ~ 2L,
    discretization_step == 0.001 ~ 3L,
    discretization_step == 0.0001 ~ 4L
  )

  inflow <- round(inflow, rounding_level)
  inflow_forecast <- round(inflow_forecast, rounding_level)

  max_in <- max(sum(inflow),
                sum(apply(inflow_forecast, 2, function(x) max(x, na.rm = T))))
  min_in <- min(sum(inflow),
                sum(apply(inflow_forecast, 2, function(x) min(x, na.rm = T))))
  max_out <- n_periods * max_release
  min_out <- 0 #n_periods * min_release


  S_upper_limit <- min(initial_storage + max_in, #- min_out,
                       reservoir_capacity)

  S_lower_limit <- min(max(initial_storage - max_out, 0), initial_storage)

  ## need to deal with this!
  target_end_of_week_storage <- round(min(target_end_of_week_storage,
                                    S_upper_limit), rounding_level)

  if(S_upper_limit < target_end_of_week_storage) S_upper_limit <- target_end_of_week_storage
  if(S_lower_limit > target_end_of_week_storage) S_lower_limit <- target_end_of_week_storage


  R_disc_x <- seq(min_release, max_release, discretization_step)
  S_states <- seq(S_lower_limit, S_upper_limit, discretization_step)
  S_end_state <- which(round(S_states, rounding_level) == target_end_of_week_storage)

  State_mat_skeleton <- matrix(0, nrow = length(S_states), ncol = length(R_disc_x))
  State_mat_S <- apply(State_mat_skeleton, 2, "+", S_states)
  State_mat <- t(apply(State_mat_S, 1, "-", R_disc_x))

  if(mode %in% c("single_fixed", "single_adaptive")){

    if(nrow(inflow_forecast) > 1){
      inflow_forecast <- inflow_forecast[1,]
    }

    Rev_to_go <- vector("numeric", length = length(S_states))
    Bellman <- matrix(0, nrow = length(S_states), ncol = length(inflow_forecast))
    Policy <- matrix(0, ncol = length(inflow_forecast), nrow = length(S_states))

    # POLICY OPTIMIZATION----------------------------------------------------------------

    DP(n_periods = n_periods,
       inflow_forecast = inflow_forecast,
       S_upper_limit = S_upper_limit,
       S_lower_limit = S_lower_limit,
       R_disc_x = R_disc_x,
       S_states = S_states,
       State_mat = State_mat,
       S_end_state = S_end_state,
       price_forecast = price_forecast,
       Rev_to_go = Rev_to_go,
       Bellman = Bellman,
       Policy = Policy,
       use_storage_target = use_storage_target) ->
      release_policy

  }

  if(mode == "rolling_adaptive"){

    1:7 %>% map(function(horizon){

      Rev_to_go <- vector("numeric", length = length(S_states))
      Bellman <- matrix(0, nrow = length(S_states), ncol = length(inflow_forecast[horizon,]))
      Policy <- matrix(0, ncol = length(inflow_forecast[horizon,]), nrow = length(S_states))

      DP(n_periods = n_periods,
         inflow_forecast = inflow_forecast[horizon,],
         S_upper_limit = S_upper_limit,
         S_lower_limit = S_lower_limit,
         R_disc_x = R_disc_x,
         S_states = S_states,
         State_mat = State_mat,
         S_end_state = S_end_state,
         price_forecast = price_forecast,
         Rev_to_go = Rev_to_go,
         Bellman = Bellman,
         Policy = Policy) ->
        release_policy

    }) -> release_policy

  }


  # ===================================================================================



  # SIMULATE POLICY--------------------------------------------------------------------

  if(mode == "single_fixed") {

    # double simulation required...
    # First simulation determines release schedule assuming forecasted inflow
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow_forecast,
      release_policy = rep(list(release_policy), 7),
      R_disc_x = R_disc_x,
      S_states = S_states,
      S = c(initial_storage, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods, day = rep(1:7, each = (n_periods / 7)))
    ) -> sim_for_release

    # Second simulation implements pre-determined release schedule using actual inflow
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow,
      release_policy = sim_for_release[["release_turbine"]],
      R_disc_x = R_disc_x,
      S_states = S_states,
      S = c(initial_storage, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods,day = rep(1:7, each = (n_periods / 7)))
    ) -> schedule

  }

  if(mode == "single_adaptive"){
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow,
      release_policy = rep(list(release_policy), 7),
      R_disc_x = R_disc_x,
      S_states = S_states,
      S = c(initial_storage, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods, day = rep(1:7, each = (n_periods / 7)))
    ) -> schedule
  }

  if(mode == "rolling_adaptive"){
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow,
      release_policy = release_policy,
      R_disc_x = R_disc_x,
      S_states = S_states,
      S = c(initial_storage, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods,day = rep(1:7, each = (n_periods / 7)))
    ) -> schedule
  }

  # ===================================================================================

  return(schedule)

}


