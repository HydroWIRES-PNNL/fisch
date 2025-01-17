% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DP.R
\name{simulate_DP_policy}
\alias{simulate_DP_policy}
\title{simulate_DP_policy}
\usage{
simulate_DP_policy(
  n_periods,
  release_policy,
  R_disc_x,
  R,
  Q,
  S,
  S_cap,
  S_states,
  Spill,
  price_forecast = price_forecast,
  MWh_per_MCM = MWh_per_MCM,
  Revenue,
  t_to_day
)
}
\arguments{
\item{n_periods}{number of sub-daily periods}

\item{release_policy}{release policy}

\item{R_disc_x}{release discretization}

\item{R}{release vector}

\item{Q}{inflow vector}

\item{S}{storage vector}

\item{S_cap}{storage capacity constant}

\item{S_states}{storage states}

\item{Spill}{spill vector}

\item{price_forecast}{price forecast}

\item{MWh_per_MCM}{energy value of release}

\item{Revenue}{revenue vector}

\item{t_to_day}{timestep to day of week conversion}

\item{Policy}{placeholder matrix for release policy}
}
\description{
Simulate the week-ahead
}
