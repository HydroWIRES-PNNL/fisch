#' schedule_release
#'
#' @description Optimizes then simulates one week of sub-daily release.
#' @param mode "single_fixed" => single forecast used at start of week; release is based on that forecast | "single_adaptive" => single forecast used at start of week, but release responds to policy during simulation (i.e., updates with storage) | "rolling_adaptive" => policy is updated daily with new forecast information; simulation is rolling horizon | "day_ahead" => rolling horizon; within-day schedule fixed in simulation.
#' @param inflow actual inflow time series.
#' @param inflow_forecast inflow forecast with daily updating and same resolution as inflow. If NULL, perfect forecast is applied.
#' @param price_foreacst price forecast with same temporal resolution as inflow.
#' @param release_value power value of turbined water in MWh per MCM.
#' @param initial_storage storage level at start of week. If using "single_both" mode, two initial storage values may be supplied in order "fixed", "adaptive".
#' @param reservoir_capacity reservoir capacity
#' @param storage_limits lower and upper limits of evaluated storage. If NULL (default) this is calculated using initial storage.
#' @param target_end_of_week_storage storage level targeted for end of week
#' @param max_release maximum overall release; can be physically determined, or operationally and week-specific
#' @param min_release minimum overall release; can be physically determined, or operationally and week-specific
#' @param maxrelease_gen maximum power release
#' @param discretization_step default 0.2. Other water volume variables (inflow, inflow_forecast, reservoir_capacity, etc.) must all be discretized at this level.
#' @param use_storage_target T/F
#' @param storage_target_weight optimization weight placed on storage target
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
                              storage_limits = NULL,
                              target_end_of_week_storage = TEST_target_end_of_week_storage,
                              max_release = TEST_max_release,
                              min_release = TEST_min_release,
                              maxrelease_gen = NULL,
                              discretization_step = 0.2,
                              use_storage_target = TRUE,
                              storage_target_weight = 1e6){

  length(inflow) -> n_periods
  stopifnot(length(price_forecast) == n_periods)

  # is policy single or rolling horizon?
  policy_opt <- strsplit(mode, "_")[[1]][1]

  # duel simulation with single and adaptive modes?
  duel_sim <- (strsplit(mode, "_")[[1]][2] == "both")

  inflow_forecast_max_by_step <- apply(inflow_forecast, 2, function(x) max(x, na.rm = T))
  inflow_forecast_min_by_step <- apply(inflow_forecast, 2, function(x) min(x, na.rm = T))
  max_in <- max(sum(inflow), sum(inflow_forecast_max_by_step))
  min_in <- min(sum(inflow), sum(inflow_forecast_min_by_step))
  max_out <- n_periods * max_release

  # test to ensure that all data are supplied with compatible discretization
  discretization_grid <- seq(0,
                             max(c(reservoir_capacity, max_in, max_out)),
                             discretization_step)

  # use custom `%.in%` function to avoid machine tolerance errors
  # (see 'helpers.R' for function definition)
  if(duel_sim) stopifnot(length(initial_storage) == 2)

  stopifnot(max_release %.in% discretization_grid)
  stopifnot(min_release %.in% discretization_grid)
  stopifnot(all(sapply(initial_storage, function(x) x %.in% discretization_grid)))
  stopifnot(reservoir_capacity %.in% discretization_grid)
  stopifnot(all(sapply(inflow, function(x) x %.in% discretization_grid)))
  stopifnot(all(sapply(inflow_forecast[!is.na(inflow_forecast)],
                       function(x) x %.in% discretization_grid)))
  stopifnot(target_end_of_week_storage %.in% discretization_grid)

  # reduce computational burden by analyzing feasible storage range...
  # ... for week (rather than entire storage range)

  if(is.null(storage_limits)){
    S_lower_limit <- min(max(initial_storage - max_out, 0), initial_storage)
    S_upper_limit <- min(initial_storage + max_in, reservoir_capacity)
  }else{
    S_lower_limit <- storage_limits[1]
    S_upper_limit <- storage_limits[2]
  }

  # snap storage limits to grid
  S_lower_limit <- snap_to_disc(S_lower_limit, discretization_step)
  S_upper_limit <- snap_to_disc(S_upper_limit, discretization_step)

  # if upper limit of storage is 0, allow it to go up 1 disc step for computations
  if(S_upper_limit == 0){
    S_upper_limit = discretization_step
  }

  if(S_upper_limit < target_end_of_week_storage) S_upper_limit <- target_end_of_week_storage
  if(S_lower_limit > target_end_of_week_storage) S_lower_limit <- target_end_of_week_storage

  R_disc_x <- seq(min_release, max_release, discretization_step)
  S_states <- seq(S_lower_limit, S_upper_limit, discretization_step)

  if(is.null(maxrelease_gen)){
    maxrelease_gen <- max_release
  }else{
    maxrelease_gen <- R_disc_x[which.min(abs(R_disc_x - maxrelease_gen))]

  }

  # catch issue of expanded sequence missing capacity value

  if(near(S_upper_limit, last(S_states))){
    S_end_state <- which(near(S_states, target_end_of_week_storage))
  }else{
    S_states <- c(S_states, S_upper_limit)
    S_end_state <- which(near(S_states, target_end_of_week_storage))
  }

  State_mat_skeleton <- matrix(0, nrow = length(S_states), ncol = length(R_disc_x))
  State_mat_S <- apply(State_mat_skeleton, 2, "+", S_states)
  State_mat <- t(apply(State_mat_S, 1, "-", R_disc_x))

  # if min_release == max_release, length(R_disc_x) = 1, and State_mat is transposed incorrectly
  # with length(R_disc_x) > 1, apply(State_mat_S, 1, "-", R_disc_x) results in a long matrix
  # that has to be transposed; with length(R_disc) = 1, this does not happen, and transposition
  #
  if(length(R_disc_x) == 1){
    State_mat = t(State_mat)
  }

  # POLICY OPTIMIZATION-----------------------------------------------------------

  # SINGLE -----------------------------------
  if(policy_opt == "single"){

    if(nrow(inflow_forecast) > 1){
      inflow_forecast <- inflow_forecast[1,]
    }

    Rev_to_go <- vector("numeric", length = length(S_states))
    Bellman <- matrix(0, nrow = length(S_states), ncol = length(inflow_forecast))
    Policy <- matrix(0, ncol = length(inflow_forecast), nrow = length(S_states))

    DP(n_periods = n_periods,
       inflow_forecast = inflow_forecast,
       S_upper_limit = S_upper_limit,
       S_lower_limit = S_lower_limit,
       R_disc_x = R_disc_x,
       maxrelease_gen = maxrelease_gen,
       S_states = S_states,
       State_mat = State_mat,
       S_end_state = S_end_state,
       price_forecast = price_forecast,
       Rev_to_go = Rev_to_go,
       Bellman = Bellman,
       Policy = Policy,
       use_storage_target = use_storage_target,
       storage_target_weight = storage_target_weight) ->
      release_policy

  }

  # ROLLING-ADAPTIVE AND DAY_AHEAD -----------
  if(mode == "rolling_adaptive" | mode == "day_ahead"){

    1:7 %>% map(function(horizon){

      Rev_to_go <- vector("numeric", length = length(S_states))
      Bellman <- matrix(0, nrow = length(S_states), ncol = length(inflow_forecast[horizon,]))
      Policy <- matrix(0, ncol = length(inflow_forecast[horizon,]), nrow = length(S_states))

      DP(n_periods = n_periods,
         inflow_forecast = inflow_forecast[horizon,],
         S_upper_limit = S_upper_limit,
         S_lower_limit = S_lower_limit,
         R_disc_x = R_disc_x,
         maxrelease_gen = maxrelease_gen,
         S_states = S_states,
         State_mat = State_mat,
         S_end_state = S_end_state,
         price_forecast = price_forecast,
         Rev_to_go = Rev_to_go,
         Bellman = Bellman,
         Policy = Policy,
         use_storage_target = use_storage_target,
         storage_target_weight = storage_target_weight) ->
        release_policy

    }) -> release_policy

  }


  # ===================================================================================



  # SIMULATE POLICY--------------------------------------------------------------------

  if(mode == "single_fixed" | mode == "single_both") {

    if(duel_sim){
      initial_storage_ <- initial_storage[1]
    }else{
      initial_storage_ <- initial_storage
    }

    # double simulation required...
    # First simulation determines release schedule assuming forecasted inflow
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow_forecast,
      release_policy = rep(list(release_policy), 7),
      R_disc_x = R_disc_x,
      maxrelease_gen = maxrelease_gen,
      S_states = S_states,
      S = c(initial_storage_, vector("numeric", n_periods)),
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
      maxrelease_gen = maxrelease_gen,
      S_states = S_states,
      S = c(initial_storage_, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods,day = rep(1:7, each = (n_periods / 7)))
    ) -> schedule_fixed

  }

  if(mode == "single_adaptive" | mode == "single_both"){

    if(duel_sim){
      initial_storage_ <- initial_storage[2]
    }else{
      initial_storage_ <- initial_storage
    }

    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow,
      release_policy = rep(list(release_policy), 7),
      R_disc_x = R_disc_x,
      maxrelease_gen = maxrelease_gen,
      S_states = S_states,
      S = c(initial_storage_, vector("numeric", n_periods)),
      S_cap = reservoir_capacity,
      R = vector("numeric", n_periods),
      Spill = vector("numeric", n_periods),
      price_forecast = price_forecast,
      MWh_per_MCM = MWh_per_MCM,
      Revenue = vector("numeric", n_periods),
      t_to_day = tibble(t = 1:n_periods, day = rep(1:7, each = (n_periods / 7)))
    ) -> schedule_adaptive
  }

  if(mode == "rolling_adaptive"){
    simulate_DP_policy(
      n_periods = n_periods,
      Q = inflow,
      release_policy = release_policy,
      R_disc_x = R_disc_x,
      maxrelease_gen = maxrelease_gen,
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

  if(mode == "day_ahead"){

    # for each day of the week, simulate the day ahead with its forecast,
    # then simulate with actual flow to determine end of day storage...
    # which is passed to the next day.

    daily_initial_S <- c(initial_storage, rep(NA, 6))
    periods_per_day <- n_periods / 7
    sim_daily_update <- tibble()

    for (day in 1:7) {

      day * periods_per_day -> day_end_period
      day_end_period - (periods_per_day - 1) -> day_start_period
      sim_period <- day_start_period:day_end_period

      # get schedule with forecast
      simulate_DP_policy(
        n_periods = n_periods / 7,
        Q = inflow_forecast[day,][sim_period],
        release_policy = list(release_policy[[day]][,sim_period]),
        R_disc_x = R_disc_x,
        maxrelease_gen = maxrelease_gen,
        S_states = S_states,
        S = c(daily_initial_S[day], vector("numeric", periods_per_day)),
        S_cap = reservoir_capacity,
        R = vector("numeric", periods_per_day),
        Spill = vector("numeric", periods_per_day),
        price_forecast = price_forecast[sim_period],
        MWh_per_MCM = MWh_per_MCM,
        Revenue = vector("numeric", periods_per_day),
        # "day" in tibble below is set to 1 to grab [[1]] from release policy,
        # ... which must be a list object to initiate policy.
        t_to_day = tibble(t = sim_period, day = 1)
      ) -> day_schedule_bid

      # simulate schedule with actual flow
      simulate_DP_policy(
        n_periods = n_periods / 7,
        Q = inflow[sim_period],
        release_policy = day_schedule_bid[["release_turbine"]],
        R_disc_x = R_disc_x,
        maxrelease_gen = maxrelease_gen,
        S_states = S_states,
        S = c(daily_initial_S[day], vector("numeric", periods_per_day)),
        S_cap = reservoir_capacity,
        R = vector("numeric", periods_per_day),
        Spill = vector("numeric", periods_per_day),
        price_forecast = price_forecast[sim_period],
        MWh_per_MCM = MWh_per_MCM,
        Revenue = vector("numeric", periods_per_day),
        t_to_day = tibble(t = sim_period, day = !!day)
      ) -> day_schedule_actual

      sim_daily_update <- bind_rows(sim_daily_update,
                                    day_schedule_actual[1:periods_per_day,] %>%
                                      mutate(time = sim_period))
      daily_initial_S[day + 1] <- last(day_schedule_actual[["storage_sim"]])

    }

    schedule <- bind_rows(
      sim_daily_update,
      tibble(time = n_periods + 1,
             storage_sim = last(daily_initial_S))
    )

  }

  # ===================================================================================

  if(mode == "single_fixed"){
    return(schedule_fixed)
  }

  if(mode == "single_adaptive"){
    return(schedule_adaptive)
  }

  if(mode == "single_both"){
    return(
      bind_rows(
        schedule_fixed %>% mutate(mode = "fixed"),
        schedule_adaptive %>% mutate(mode = "adaptive")
      )
    )
  }

  return(schedule)

}

