# ============================================================
# WEEK 10 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, optimizeR)

# ============================================================
# 1. LIKELIHOOD: A COIN TOSSED IN THREE ROUNDS ----
# ============================================================
# L(theta | y) = f(y | theta) = prod_i f(y_i | theta), log L = sum_i log f(y_i | theta)
# Three rounds of n = 10 tosses, with 7, 9 and 8 heads
example_tosses <- 10
example_heads <- c(7, 9, 8)

# L(0.7 | 7, 9, 8, n = 10) = f_Bin(7; 10, 0.7) * f_Bin(9; 10, 0.7) * f_Bin(8; 10, 0.7)
example_factors <- dbinom(example_heads, size = example_tosses, prob = 0.7)
example_factors

example_likelihood <- prod(example_factors)
example_likelihood

# the same for 20 values of p
example_grid <- expand_grid(
  p = seq(0.05, 1, by = 0.05),
  k = example_heads
) |>
  group_by(p) |>
  summarize(likelihood = prod(dbinom(k, size = example_tosses, prob = p)))
example_grid

example_grid |>
  ggplot(aes(x = p, y = likelihood)) +
  geom_segment(aes(xend = p, yend = 0), color = "grey70") +
  geom_line(color = "#2f7f93", linewidth = 1) +
  geom_point(color = "darkorange", size = 2) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = "The likelihood function",
    x     = "p",
    y     = "L(p | 7, 9, 8, n = 10)"
  )

example_p_hat <- example_grid |>
  slice_max(likelihood) |>
  pull(p)
example_p_hat

# ============================================================
# 2. DATA: FLIGHTS ----
# ============================================================
# Lufthansa flights, 180 seats, March 2026
flights <- read_csv("data/lufthansa_flights.csv") |>
  mutate(show_ups = pax_boarded + pax_boarding_declined)
flights

tickets_sold <- 191

flights |>
  ggplot(aes(x = show_ups)) +
  geom_histogram(binwidth = 1, fill = "#2f7f93") +
  labs(
    title = "Passengers showing up per flight",
    x     = "Show-ups",
    y     = "Flights"
  )

# ============================================================
# 3. THE LIKELIHOOD BY HAND: ONE FLIGHT ----
# ============================================================
first_flight <- flights |>
  slice_head(n = 1) |>
  pull(show_ups)
first_flight

# f_Bin(k; n, p) = choose(n, k) * p^k * (1 - p)^(n - k)
choose(tickets_sold, first_flight) * 0.85^first_flight * (1 - 0.85)^(tickets_sold - first_flight)
choose(tickets_sold, first_flight) * 0.9^first_flight * (1 - 0.9)^(tickets_sold - first_flight)

# L(p | k, n) = choose(n, k) * p^k * (1 - p)^(n - k)
binom_lik <- function(p, k, n) {
  choose(n, k) * p^k * (1 - p)^(n - k)
}

lik_one <- tibble(p = seq(0.8, 1, by = 0.001)) |>
  mutate(
    likelihood = binom_lik(p, first_flight, tickets_sold),
    dbinom     = dbinom(first_flight, size = tickets_sold, prob = p)
  )
lik_one

# p_hat_ML = arg max_p L(p | k, n)
p_hat_one <- lik_one |>
  slice_max(likelihood) |>
  pull(p)
p_hat_one

lik_one |>
  ggplot(aes(x = p, y = likelihood)) +
  geom_line(aes(color = "Likelihood"), linewidth = 1) +
  geom_vline(aes(xintercept = p_hat_one, color = "Maximum likelihood estimate"), linetype = "dashed") +
  scale_color_manual(values = c(
    "Likelihood"                  = "#2f7f93",
    "Maximum likelihood estimate" = "darkorange"
  )) +
  labs(
    title = "Likelihood of p for one flight",
    x     = "p",
    y     = "L(p)",
    color = NULL
  )

# ============================================================
# 4. THE LIKELIHOOD BY HAND: ALL FLIGHTS ----
# ============================================================
# L(p | k_1, ..., k_N, n) = prod_i choose(n, k_i) * p^k_i * (1 - p)^(n - k_i)
flights |>
  summarize(likelihood = prod(binom_lik(0.9, show_ups, tickets_sold)))

show_ups <- flights |>
  pull(show_ups)

lik_all <- tibble(p = seq(0.8, 1, by = 0.001)) |>
  rowwise() |>
  mutate(likelihood = prod(binom_lik(p, show_ups, tickets_sold))) |>
  ungroup()

# underflow: 0 for every p
lik_all |>
  count(likelihood == 0)

lik_all |>
  ggplot(aes(x = p, y = likelihood)) +
  geom_line(color = "#2f7f93", linewidth = 1) +
  labs(
    title = "Likelihood of p for all flights",
    x     = "p",
    y     = "L(p)"
  )

# ============================================================
# 5. THE LOG-LIKELIHOOD BY HAND ----
# ============================================================
# log L(p | k_1, ..., k_N, n) = sum_i [log choose(n, k_i) + k_i * log p + (n - k_i) * log(1 - p)]
binom_loglik <- function(p, k, n) {
  sum(log(choose(n, k)) + k * log(p) + (n - k) * log(1 - p))
}

# without the constant term: same maximum
binom_loglik_short <- function(p, k, n) {
  sum(k * log(p) + (n - k) * log(1 - p))
}

# log of each flight's likelihood first, then the sum
binom_loglik_logged <- function(p, k, n) {
  sum(log(binom_lik(p, k, n)))
}

tibble(
  full   = binom_loglik(0.9, show_ups, tickets_sold),
  logged = binom_loglik_logged(0.9, show_ups, tickets_sold),
  short  = binom_loglik_short(0.9, show_ups, tickets_sold)
)

loglik_all <- tibble(p = seq(0.8, 1, by = 0.001)) |>
  rowwise() |>
  mutate(
    full   = binom_loglik(p, show_ups, tickets_sold),
    logged = binom_loglik_logged(p, show_ups, tickets_sold),
    short  = binom_loglik_short(p, show_ups, tickets_sold)
  ) |>
  ungroup()
loglik_all

p_hat_grid <- loglik_all |>
  slice_max(full) |>
  pull(p)
p_hat_grid

loglik_all |>
  filter(p > 0.85, p < 0.97) |>
  pivot_longer(c(full, logged, short), names_to = "version", values_to = "loglik") |>
  mutate(panel = if_else(version == "short", "Without the constant term", "Full log-likelihood")) |>
  ggplot(aes(x = p, y = loglik)) +
  geom_line(aes(color = version, linetype = version), linewidth = 1) +
  geom_vline(xintercept = p_hat_grid, color = "darkorange", linetype = "dashed") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(
    full   = "#2f7f93",
    logged = "grey60",
    short  = "#2f7f93"
  )) +
  scale_linetype_manual(values = c(full = "solid", logged = "dashed", short = "solid")) +
  labs(
    title    = "Log-likelihood of p for all flights",
    subtitle = "Dashed orange line: maximum likelihood estimate",
    x        = "p",
    y        = "log L(p)",
    color    = "Version",
    linetype = "Version"
  )

# ============================================================
# 6. NUMERICAL OPTIMIZATION ----
# ============================================================
# p in [0, 1]: bounds lower = 0, upper = 1, starting value 0.5
optimizer_bounded <- Optimizer$new(which = "stats::nlminb")
optimizer_bounded$set_arguments(lower = 0, upper = 1)

binom_mle <- optimizer_bounded$maximize(
  objective = binom_loglik,
  initial   = 0.5,
  k         = show_ups,
  n         = tickets_sold
)

p_hat <- binom_mle |>
  pluck("parameter")
p_hat

# a single peak: every starting value gives the same estimate
tibble(initial = c(0.1, 0.5, 0.9)) |>
  rowwise() |>
  mutate(p_hat = pluck(
    optimizer_bounded$maximize(
      objective = binom_loglik,
      initial   = initial,
      k         = show_ups,
      n         = tickets_sold
    ),
    "parameter"
  )) |>
  ungroup()

# score function S(p) = d log L / dp = 0  =>  p_hat_ML = sum_i k_i / (N * n)
flights |>
  summarize(
    grid      = p_hat_grid,
    optimizer = p_hat,
    formula   = sum(show_ups) / (n() * tickets_sold)
  )

# ============================================================
# 7. FROM THE BY-HAND FUNCTION TO l() AND ll() ----
# ============================================================
# L = prod_i f_Bin(k_i; n, p), log L = sum_i log f_Bin(k_i; n, p)
l <- function(p, k, n, log = FALSE) {
  dbinom(k, size = n, prob = p, log = log)
}

ll <- function(p, k, n) {
  sum(l(p, k, n, log = TRUE))
}

# l(): one likelihood per flight
flights |>
  select(show_ups) |>
  mutate(likelihood = l(p_hat, show_ups, tickets_sold))

tibble(
  by_hand  = binom_loglik(p_hat, show_ups, tickets_sold),
  shortcut = ll(p_hat, show_ups, tickets_sold)
)

# ============================================================
# 8. LINEAR MODEL WITHOUT COVARIATES ----
# ============================================================
# GERDA 2025 without Berlin
gerda <- read_csv("data/federal_cty_harm.csv")

counties <- gerda |>
  filter(
    election_year == 2025,
    state != "11"
  ) |>
  select(county_code, afd, turnout, greens = gruene, fdp) |>
  mutate(across(afd:fdp, ~ .x * 100))
counties

# AfD_i ~ N(mu_i, sigma^2), mu_i = beta_0
# theta = (beta_0, log sigma): no bounds needed
l <- function(theta, y, log = FALSE) {
  dnorm(y, mean = pluck(theta, 1), sd = exp(pluck(theta, 2)), log = log)
}

ll <- function(theta, y) {
  sum(l(theta, y, log = TRUE))
}

afd_shares <- counties |>
  pull(afd)

# starting values: beta_0 = 0, sigma = 10
start_mean <- c(0, log(10))
ll(start_mean, afd_shares)

optimizer <- Optimizer$new(which = "stats::nlminb")

fit_mean <- optimizer$maximize(
  objective = ll,
  initial   = start_mean,
  y         = afd_shares
)
fit_mean |>
  pluck("parameter")

# sigma_hat_ML = exp(log sigma_hat)
theta_mean <- fit_mean |>
  pluck("parameter")

counties |>
  summarize(
    beta_0_ml = pluck(theta_mean, 1),
    mean      = mean(afd),
    sigma_ml  = exp(pluck(theta_mean, 2)),
    sd        = sd(afd)
  )

# one slice through the log-likelihood, sigma held at its estimate
sigma_log_hat <- pluck(theta_mean, 2)

loglik_beta_0 <- tibble(beta_0 = seq(10, 35, by = 0.1)) |>
  rowwise() |>
  mutate(loglik = ll(c(beta_0, sigma_log_hat), afd_shares)) |>
  ungroup()

loglik_beta_0 |>
  ggplot(aes(x = beta_0, y = loglik)) +
  geom_line(aes(color = "Log-likelihood"), linewidth = 1) +
  geom_vline(aes(xintercept = pluck(theta_mean, 1), color = "Maximum likelihood estimate"), linetype = "dashed") +
  scale_color_manual(values = c(
    "Log-likelihood"              = "#2f7f93",
    "Maximum likelihood estimate" = "darkorange"
  )) +
  labs(
    title = "Log-likelihood of the mean AfD share",
    x     = expression(beta[0]),
    y     = "log L",
    color = NULL
  )

# ============================================================
# 9. LINEAR MODEL WITH ONE COVARIATE ----
# ============================================================
counties |>
  ggplot(aes(x = turnout, y = afd)) +
  geom_point(color = "#2f7f93", alpha = 0.5) +
  labs(
    title = "AfD share and turnout by county, 2025",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)"
  )

# AfD_i ~ N(mu_i, sigma^2), mu_i = beta_0 + beta_1 * Turnout_i
x_beta <- function(theta, x) {
  pluck(theta, 1) + pluck(theta, 2) * x
}

l <- function(theta, y, x, log = FALSE) {
  dnorm(y, mean = x_beta(theta, x), sd = exp(pluck(theta, 3)), log = log)
}

ll <- function(theta, y, x) {
  sum(l(theta, y, x, log = TRUE))
}

turnout_values <- counties |>
  pull(turnout)

theta_lm <- optimizer$maximize(
  objective = ll,
  initial   = c(0, 0, log(10)),
  y         = afd_shares,
  x         = turnout_values
) |>
  pluck("parameter")
theta_lm

turnout_model <- lm(afd ~ turnout, data = counties)
turnout_model |>
  tidy()

beta_0_ml <- pluck(theta_lm, 1)
beta_1_ml <- pluck(theta_lm, 2)
sigma_ml  <- exp(pluck(theta_lm, 3)) # theta holds log sigma

ols_coefs <- turnout_model |>
  tidy() |>
  pull(estimate)
ols_sigma <- turnout_model |>
  glance() |>
  pull(sigma)

tibble(
  parameter = c("beta_0", "beta_1", "sigma"),
  ml        = c(beta_0_ml, beta_1_ml, sigma_ml),
  ols       = c(ols_coefs, ols_sigma)
)

# sigma_hat_ML = sqrt(RSS / N), sigma_hat_OLS = sqrt(RSS / (N - 2))
turnout_model |>
  glance() |>
  mutate(
    sigma_rss_n = sqrt(deviance / nobs),
    sigma_ml    = sigma_ml
  ) |>
  select(sigma, sigma_rss_n, sigma_ml)

# ============================================================
# 10. TRY IT: PASSING AN EXAM ----
# ============================================================
# Out of 10 students, 6 passed the exam. Each student passes with the
# same probability p.
# Your task: write l() and ll() for this Binomial and report the
# maximum likelihood estimate of p, with a plot of the log-likelihood and
# a line at the estimate.

# ============================================================
# 11. TRY IT: GREENS AND FDP ----
# ============================================================
# Greens_i ~ N(beta_0 + beta_1 * FDP_i, sigma^2)
# Your task: estimate this model on the 2025 counties by maximum
# likelihood and compare your estimates with lm() in one table.
