
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Forecast Informed Scheduler for Hydropower (FIScH)

<!-- badges: start -->
<!-- badges: end -->

FIScH is a hydropower scheduler that optimizes hourly releases within a
week to maximize hydropower revenue. Dynamic programming is used to
identify the optimal release schedule given hourly electricity prices,
hourly inflow, and an end-of-week storage target that parameterizes
non-powered objectives and constraints.

## Installation

You can install the development version of FIScH from GitHub:

1.  Install the `devtools` package `install.packages("devtools")`
2.  Load the package `library(devtools)`
3.  Install `fisch` from GitHub
    `install_github("HydroWIRES-PNNL/fisch")`

``` r
install.packages("devtools")
library(devtools)
install_github("HydroWIRES-PNNL/fisch")
```

``` r
library(fisch)
```

## Description

`FIScH` is a hydropower scheduler that optimizes hourly releases to
maximize revenue. It is a generic scheduler that assumes revenue is
generated solely from day-ahead markets, and parameterizes non-powered
objectives and constraints using end-of-week storage targets. The
assumption here is that as long as end-of-week storage targets are
satisfied, and limits on minimum and maximum hourly power generation are
satisfied, all non-powered objectives are being satisfied. The
end-of-week storage targets were developed using the observation-based
data-driven method described in [Turner et al.
2021](https://doi.org/10.1016/j.jhydrol.2021.126843).

`FIScH` schedules hydropower releases on a week-by-week basis, with the
ending week conditions serving as the inital condtions for the
subsequent week. Within the week, schedules are developed using inflow
forecasts, and executed using actual inflow.

Weekly revenue is determined by:

$$R_w = \sum_{t=1}^{168} r_t C_{pw} p_t$$ where $r_t$ is the hourly
release, $C_{pw}$ is the water-to-power conversion factor, and $p_t$ is
the locational marginal price (LMP) for that hour.

Dynamic programming is used to determine the optimal hourly release
schedule that results in the most weekly revenue while also satisfying
and end-of-week storage target. The storage target is not provided as a
constraint, but rather an objective, with a large negative weight if it
is not satisfied.

## Modes

`FIScH` has three modes of operation: *fixed*, *adaptive*, and *rolling
adaptive*. *day-ahead*

- fixed: optimizes releases for each week once using fixed inflow
  forecasts and executes the schedule using observed inflow

- day-ahead: optimizes the hydropower schedule, and re-optimizes the
  schedule each day of the week with updated inflow forecasts. The
  schedule is still executed each day using observed inflow

- adaptive

- rolling adaptive

## Flexibility Options

`FIScH` currently has two options to allow for greater flexibility in
hydropower scheduling. These options allow for the decision space to be
extended beyond the end of a week. Optimization of the schedule is still
done week-by-week, but these options expand the decision space.

### Storage Target Deviations

`FIScH` retains a constraint that ensures that storage targets are
satisfied by the end of a month, but allow for deviations from
end-of-week storage targets for the first 3 weeks in a month. This
deviation is specified by a user-defined fraction deviation from the
storage target where no penalty is imposed. Typically this fraction
deviation is largest for the first week in a month, and narrows for
weeks 2 and 3, before returning to the original storage target deviation
penalty in week 4.

### Carryover Storage

`FIScH` also has an option to place a value on carryover storage which
can represent hedging in hydropower scheduling by retaining additional
water in one week so that it can be used in the following week.
Carryover storage is valued by multiplying the volume of storage by an
average LMP for the following week, and a user-defined weighting factor
that scales future revenue to weight it against revenue in the current
week when optimizing releases.

## Example

`fisch` contains an example set of inputs that are set as defaults in
the `schedule_release` function:

``` r

# run FIScH with default set of inputs
fisch_output <- schedule_release()

# view output
fisch_output
#> # A tibble: 29 × 7
#>     time storage_sim release_spill release_turbine benefit_revenue price_price
#>    <int>       <dbl>         <dbl>           <dbl>           <dbl>       <dbl>
#>  1     1       1000              0               1            6568       16.4 
#>  2     2       1000              0               1            9308       23.3 
#>  3     3       1000.             0               1            8052       20.1 
#>  4     4       1000.             0               1           10004       25.0 
#>  5     5       1001.             0               1            5664       14.2 
#>  6     6       1001              0               1            2912        7.28
#>  7     7       1001.             0               1           -3672       -9.18
#>  8     8       1002.             0               1           10660       26.6 
#>  9     9       1002.             0               1            8672       21.7 
#> 10    10       1002.             0               1           11216       28.0 
#> # ℹ 19 more rows
#> # ℹ 1 more variable: inflow_actual <dbl>
```

Although `FIScH` does not include any visualization functions, the
output table is designed to be easily processed using `ggplot2`. The
following function is recommended for comparing multiple `FIScH` runs in
a single display.

``` r
library(ggplot2) # for plotting
library(patchwork) # for combining plots

plot_fisch_output <- function(output_tables){

  # convert output to long form for plotting
  output_tables %>%
    tidyr::gather(variable, value, -time, -run) %>%
    tidyr::separate(variable, into = c("variable", "data")) %>%
    mutate(time = time - 1) -> data_for_plot

  # inflow plot
  data_for_plot %>%
    filter(variable == "inflow") %>%
    ggplot(aes(time, value, col = run)) + geom_line() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "Inflow (MCM)", y = NULL, x = NULL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) ->
    inflow_plot

  # price plot
  data_for_plot %>% filter(variable == "price") %>%
    select(time, data, value) %>% unique() %>%
    ggplot(aes(time, value)) + geom_line(col = "red") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs (title = "Price ($/MWh)", y = NULL, x = NULL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) ->
    price_plot

  # release plot
  data_for_plot %>%
    filter(variable == "release") %>%
    ggplot(aes(time, value, col = run, linetype = data)) + geom_line() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs (title = "Release (- - -) and spill (MCM)", y = NULL, x = NULL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) ->
    release_plot


  data_for_plot %>%
    filter(variable == "storage") %>%
    ggplot(aes(time, value, col = run)) + geom_line() +
    theme_minimal() +
    geom_point(data = tibble(time = 28, value = fisch:::TEST_target_end_of_week_storage),
               aes(col = NA), size = 7, pch = 4, col = "black") +
    theme(legend.position = "none") +
    labs (title = "Storage (MCM)", y = NULL, x = NULL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) ->
    storage_plot


  data_for_plot %>% filter(variable == "benefit") %>%
    tidyr::replace_na(list(value = 0)) %>%
    group_by(run) %>% mutate(value = cumsum(value)) %>%
    ggplot(aes(time, value, col = run)) + geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs (title = "Cumulative revenue ($)", y = NULL, x = NULL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) ->
    rev_plot

  inflow_plot + price_plot + release_plot + storage_plot + rev_plot +
    plot_layout(ncol = 1)
}
```

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

schedule_release(max_release = 10, min_release = 0) -> fisch_output_2

bind_rows(
  fisch_output %>% mutate(run = "default"),
  fisch_output_2 %>% mutate(run = "flexible release")
) -> output_tables

plot_fisch_output(output_tables = output_tables)
#> Warning: Removed 2 rows containing missing values (`geom_line()`).
#> Warning: Removed 1 row containing missing values (`geom_line()`).
#> Warning: Removed 4 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
