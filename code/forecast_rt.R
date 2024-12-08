library(rtestim)
library(tidyverse)
theme_set(theme_bw())
bc <- Stat406::bccovid
bc <- bc %>% filter(date >= "2022-12-01", date <= "2023-02-01")
ca <- filter(cancovid, date >= "2022-12-01", date <= "2023-02-01")
ca <- rename(ca, cases = incident_cases)
n_to_drop <- 15
to_keep <- 1:(nrow(bc) - n_to_drop)
bcrt <- estimate_rt(bc$cases[to_keep], lambda = 5000)
cart_all <- estimate_rt(ca$cases, lambda = 10000)
cart_drop <- estimate_rt(
  c(ca$cases[to_keep], ca$cases[-to_keep] - bc$cases[-to_keep]),
  lambda = 10000
)
pop_ratio <- 5000879 / 36991981
cart_imp <- estimate_rt(
  c(ca$cases[to_keep], (ca$cases[-to_keep] - bc$cases[-to_keep]) / (1 - pop_ratio)),
  lambda = 10000
)
inc_ratio <- tail(bcrt$weighted_past_counts, 1) /
  tail(cart_all$weighted_past_counts[to_keep], 1)
cart_inc_imp <- estimate_rt(
  c(ca$cases[to_keep], (ca$cases[-to_keep] - bc$cases[-to_keep]) / (1 - inc_ratio)),
  lambda = 10000
)
bcrt_full <- estimate_rt(bc$cases, lambda = 5000)

# build up some incidence

last_lam <- tail(bcrt$weighted_past_counts, 1)
proj_I <- double(n_to_drop + 1)
proj_R <- double(n_to_drop)
proj_lam <- double(n_to_drop + 1)
proj_lam[1] <- last_lam
global_R <- tail(cart_inc_imp$Rt, n_to_drop)
for (ii in 1:n_to_drop) {
  proj_I[ii + 1] <- global_R[ii] * proj_lam[ii]
  all_inc <- c(bc$cases[-to_keep], proj_I[2:(ii + 1)])
  proj_lam[ii + 1] <- tail(delay_calculator(all_inc), 1)
  proj_R[ii] <- global_R[ii] * proj_lam[ii] / proj_lam[ii + 1]
}

comb <- tibble(
  date = bc$date,
  local_dropout = c(bcrt$Rt, rep(NA, n_to_drop)),
  local_complete = bcrt_full$Rt,
  local_impute = estimate_rt(c(bc$cases[to_keep], proj_I[-1]), lambda = 5000)$Rt,
  # global_dropped = cart_drop$Rt,
  # global_pop_impute = cart_imp$Rt,
  global_inc_impute = cart_inc_imp$Rt,
  global = cart_all$Rt
) %>%
  pivot_longer(-date) %>%
  filter(!is.na(value)) %>%
  bind_rows(tibble(
    date = bc$date[-to_keep],
    value = interpolate_rt(bcrt, seq(to = nrow(bc), length.out = n_to_drop)),
    name = "local_extrapolate")
  )



labs <- c(
  global = "Global Truth",
  global_inc_impute = "Global (imputed after dropping the location)",
  local_complete = "Local Truth",
  local_dropout = "Local (only observations)",
  local_extrapolate = "Local (projected Rt from observations)",
  local_impute = "Our procedure"
)

ggplot(comb, aes(date, value)) +
  geom_line(aes(color = name)) +
  geom_hline(yintercept = 1) +
  labs(y = "Rt", x = "Date") +
  scale_color_brewer(palette = "Dark2", labels = labs, name = "") +
  geom_vline(xintercept = max(comb$date) - n_to_drop, linetype = "dashed") +
  theme(legend.position = "bottom")

ggsave(here::here("gfx/first-fig.pdf"), width = 8, height = 5)
