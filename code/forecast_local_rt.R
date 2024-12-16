# install.packages("remotes")
# remotes::install_github("dajmcdon/rtestim")
# the only required function in `{rtestim}` is `delay_calculator()`,
# but we'll use the estimator for the example below
#
# Also need `{tidyverse}` and `{here}` for the example
# install.packages("tidyverse")
# install.packages("here")
#
#' Estimate local Rt using global information
#'
#' @param global_incidence Vector. The observed global incidence. We assume that
#'   this has NOT been corrected for the missing local reports, and perform
#'   this correction internally. So the recent incidence is underreported.
#' @param local_incidence Vector. Observed local incidence. This vector should
#'   have length less than `global_incidence`. So missing values are implicit.
#' @param rt_estimator Function. This function should take only one argument,
#'   the observed incidence, and produce an estimated Rt with the same length.
#' @param n_recent_correction Integer. How many recently reported incidence
#'   from the availability of the overlap between global and local should be
#'   used to infer the relative proportion of incidence? Default is 1. Larger
#'   values may improve robustness to reporting noise.
#' @param recent_center_estimator Either "mean" or "median" for calculating
#'   the ratio of recent observed incidence..
#' @param ... Additional arguments passed to `rtestim::delay_calculator()`.
#'   Typically, these will be parameters of the serial interval distribution.
#'
#' @return An estimate of local Rt.
forecast_local_rt <- function(
    global_incidence,
    local_incidence,
    rt_estimator,
    n_recent_correction = 1,
    recent_center_estimator = c("mean", "median"),
    ...
) {
  n_global <- length(global_incidence)
  n_local <- length(local_incidence)
  stopifnot(n_global > n_local, is.function(rt_estimator))
  stopifnot(length(n_recent_correction) == 1, n_recent_correction > 0, n_recent_correction == round(n_recent_correction))
  recent_center_estimator <- get(match.arg(recent_center_estimator))

  global_Rt <- rt_estimator(global_incidence)
  global_Lambda <- rtestim::delay_calculator(global_incidence, ...)
  local_Lambda <- rtestim::delay_calculator(local_incidence, ...)

  # "Correct" global incidence for missing local
  recent_correction_idx <- (n_local - n_recent_correction + 1):n_local
  incidence_ratio <- recent_center_estimator(local_Lambda[recent_correction_idx], na.rm = TRUE) /
    recent_center_estimator(global_Lambda[recent_correction_idx], na.rm = TRUE)
  complete_reporting <- 1:n_local
  partial_reporting <- setdiff(1:n_global, complete_reporting)
  n_partial <- length(partial_reporting)
  global_incidence <- c(
    global_incidence[complete_reporting],
    global_incidence[partial_reporting] / (1 - incidence_ratio)
  )

  # set up the Algorithm (loop)
  last_Lambda <- tail(local_Lambda, 1)
  proj_I <- double(n_partial + 1)
  # proj_R <- double(n_partial)
  proj_Lambda <- double(n_partial + 1)
  proj_Lambda[1] <- last_Lambda
  recent_global_Rt <- tail(global_Rt, n_partial)

  for (ii in 1:n_partial) {
    proj_I[ii + 1] <- recent_global_Rt[ii] * proj_Lambda[ii]
    all_local_inc <- c(local_incidence, proj_I[2:(ii + 1)])
    proj_Lambda[ii + 1] <- tail(rtestim::delay_calculator(all_local_inc, ...), 1)
    # In the pseudo-code, but unused
    # proj_R[ii] <- global_Rt[ii] * proj_Lambda[ii] / proj_Lambda[ii + 1]
  }

  rt_estimator(all_local_inc)
}


# Examples ----------------------------------------------------------------

library(rtestim)
library(tidyverse)
bc <- readRDS(here::here("data","bc.rds"))
bc <- bc %>% filter(date >= "2022-12-01", date <= "2023-02-01")
ca <- filter(cancovid, date >= "2022-12-01", date <= "2023-02-01")
ca <- rename(ca, cases = incident_cases)
n_to_drop <- 7 # daily data
to_keep <- 1:(nrow(bc) - n_to_drop)

# Replace with your favorite.
rt_estimator <- function(z) estimate_rt(z, lambda = 5000)$Rt

ca_with_omission <- c(ca$cases[to_keep], ca$cases[-to_keep] - bc$cases[-to_keep])

# Default arguments to delay_calculator Gamma(2.5, scale = 2.5), matches that
# in rt_estimator()
local1 <- forecast_local_rt(ca_with_omission, bc$cases[to_keep], rt_estimator)

# Shorter delay Gamma(2.5, scale = 1.5)
rt_estimator <- function(z) {
  estimate_rt(z, lambda = 5000, dist_gamma = c(2.5, 1.5))$Rt
}
local2 <- forecast_local_rt(
  ca_with_omission, bc$cases[to_keep], rt_estimator,
  dist_gamma = c(2.5, 1.5)
)

local_complete <- rt_estimator(bc$cases) # Same delay as local2

tibble(date = bc$date, local_default = local1, local_short_delay = local2,
       complete_short_delay = local_complete) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, color = name)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = bc$date[tail(to_keep, 1)]) +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
