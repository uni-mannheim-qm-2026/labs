# ============================================================
# WEEK 3 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse)
set.seed(303)


# ============================================================
# 1. STATISTICAL INFERENCE: VOCABULARY ----
# ============================================================
# estimand:  the quantity the research question asks about
# parameter: the estimand as a fixed, unknown feature of a population/model
# estimator: a function applied to a sample to guess the parameter (mean())
# estimate:  the number an estimator returns for one particular sample
# confidence interval (CI): a range that contains the true parameter in a
# fixed proportion of repeated samples


# ============================================================
# 2. DATA: PRE-ELECTION POLLS (2025) ----
# ============================================================
# estimand: AfD's true support in the three weeks before the 2025-02-23
# federal election

polls <- read_csv("data/polls.csv")
polls

polls_long <- polls |>
  pivot_longer(CDUCSU:BSW, names_to = "party", values_to = "share")

afd_polls <- polls_long |>
  filter(party == "AfD", between(Date, as.Date("2025-02-02"), as.Date("2025-02-23")))
afd_polls


# ============================================================
# 3. POINT ESTIMATE AND STANDARD ERROR ----
# ============================================================
# SE(theta_hat) = sigma_hat / sqrt(N)

afd_stats <- afd_polls |>
  summarize(
    mean_share = mean(share),
    sd_share   = sd(share),
    n          = n()
  )
afd_stats

mean_share <- afd_stats |> pull(mean_share)
sd_share   <- afd_stats |> pull(sd_share)
n_polls    <- afd_stats |> pull(n)

se_share <- sd_share / sqrt(n_polls)
se_share


# ============================================================
# 4. CONFIDENCE INTERVALS FOR A MEAN ----
# ============================================================
#   1. analytically  -- quantiles of the Normal approximation
#   2. interpreting a CI (coverage demo)
#   3. by simulation -- draw from the assumed sampling distribution
#   4. by bootstrap  -- resample the polls themselves, with replacement

# 1. analytical: theta_hat +/- 1.96 * SE(theta_hat)
ci_analytical <- mean_share + qnorm(c(0.025, 0.975)) * se_share
ci_analytical

# 2. a 95% CI: ~95% of intervals from repeated samples contain the truth --
# demonstrated on an invented population with a known true mean
true_mean <- 21
demo_pop  <- rnorm(10000, true_mean, sd_share)

n_trials <- 100

ci_trials <- expand_grid(
  trial = 1:n_trials,
  draw  = 1:n_polls
) |>
  group_by(trial) |>
  mutate(value = sample(demo_pop, n(), replace = TRUE)) |>
  summarize(
    est     = mean(value),
    se      = sd(value) / sqrt(n()),
    lo      = est + qnorm(0.025) * se,
    hi      = est + qnorm(0.975) * se,
    .groups = "drop"
  ) |>
  mutate(missed = lo > true_mean | hi < true_mean)

ci_trials |> count(missed)

ci_trials |>
  ggplot(aes(x = est, y = trial, xmin = lo, xmax = hi, color = missed)) +
  geom_pointrange(linewidth = 0.3, size = 0.3) +
  geom_vline(xintercept = true_mean, color = "#2f7f93", linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "firebrick")) +
  labs(
    title = "100 samples, 100 CIs: most contain the true mean",
    x     = "Estimate",
    y     = "Trial",
    color = "Missed truth?"
  )

# 3. simulation: draw from N(theta_hat, SE^2), take empirical quantiles
nsim      <- 1000
sim_means <- rnorm(nsim, mean_share, se_share)

ci_simulated <- quantile(sim_means, c(0.025, 0.975))
ci_simulated

tibble(sim_means) |>
  ggplot(aes(x = sim_means)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  geom_vline(xintercept = ci_simulated, linetype = "dashed") +
  labs(
    title = "Simulated sampling distribution of the mean",
    x     = "Simulated sample mean (%)",
    y     = "Density"
  )

# 4. bootstrap: sample() draws from a plain vector
afd_shares <- afd_polls |> pull(share)

n_boot <- 1000

boot_means <- expand_grid(
  trial = 1:n_boot,
  row   = 1:n_polls
) |>
  group_by(trial) |>
  summarize(boot_mean = mean(sample(afd_shares, n(), replace = TRUE)), .groups = "drop")

boot_mean_values <- boot_means |> pull(boot_mean)
ci_bootstrap <- quantile(boot_mean_values, c(0.025, 0.975))
ci_bootstrap

# comparing the three CIs
ci_comparison <- tibble(
  method = c("Analytical", "Simulation", "Bootstrap"),
  lower  = c(ci_analytical[1], ci_simulated[1], ci_bootstrap[1]),
  upper  = c(ci_analytical[2], ci_simulated[2], ci_bootstrap[2])
)
ci_comparison

true_value <- 20.8 # the actual 2025 result -- computed properly in section 6

ci_comparison_plot <- ci_comparison |>
  mutate(
    mid = mean_share,
    y   = -2 * row_number()
  )

afd_polls |>
  ggplot(aes(x = share)) +
  geom_histogram(binwidth = 1, boundary = 0.5, fill = "#2f7f93", alpha = 0.5, color = "white") +
  geom_vline(xintercept = true_value, color = "firebrick", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = true_value, y = 11, label = "true result (Part 2)", color = "firebrick", size = 3.2, hjust = -0.05) +
  geom_pointrange(
    data = ci_comparison_plot,
    aes(x = mid, y = y, xmin = lower, xmax = upper),
    inherit.aes = FALSE,
    color = "#2f7f93",
    linewidth = 0.8
  ) +
  geom_text(
    data = ci_comparison_plot,
    aes(x = lower, y = y, label = method),
    inherit.aes = FALSE, hjust = 1.15, size = 3.3
  ) +
  scale_y_continuous(breaks = seq(0, 10, 5)) +
  labs(
    title = "The polls, three CIs, and the true result",
    x     = "AfD polled share (%)",
    y     = "Count"
  )


# ============================================================
# 5. CONFIDENCE INTERVAL FOR A RATIO OF PROPORTIONS ----
# ============================================================
# no clean SE formula for a ratio -- simulation still works
# lecture example: 55% of 500 men vs. 65% of 500 women vote left
# SE(p_hat) = sqrt(p_hat * (1 - p_hat) / n)

p_hat_men   <- 0.55
p_hat_women <- 0.65
n_men       <- 500
n_women     <- 500

se_men   <- sqrt(p_hat_men * (1 - p_hat_men) / n_men)
se_women <- sqrt(p_hat_women * (1 - p_hat_women) / n_women)

ratio_sim <- tibble(
  p_men   = rnorm(nsim, p_hat_men, se_men),
  p_women = rnorm(nsim, p_hat_women, se_women)
) |>
  mutate(ratio = p_men / p_women)

ratio_values <- ratio_sim |> pull(ratio)
ci_ratio <- quantile(ratio_values, c(0.025, 0.975))
ci_ratio


# ============================================================
# 6. DATA: GERMAN ELECTION RESULTS (2025) ----
# ============================================================
# GERDA county-level results -- the true, vote-weighted national AfD share

gerda <- read_csv("data/federal_cty_harm.csv")

gerda_2025 <- gerda |>
  filter(election_year == 2025)

national_afd <- gerda_2025 |>
  summarize(afd_national_pct = 100 * sum(afd * total_votes) / sum(total_votes))
national_afd


# ============================================================
# 7. REGIONAL VARIATION: EAST VS. WEST GERMANY ----
# ============================================================
# Berlin (state code 11) excluded -- its East/West split is ambiguous

west_codes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
east_codes <- c("12", "13", "14", "15", "16")

gerda_regions <- gerda_2025 |>
  filter(state %in% c(west_codes, east_codes)) |>
  mutate(
    region = case_when(
      state %in% west_codes ~ "West",
      state %in% east_codes ~ "East"
    ),
    afd_pct = afd * 100
  ) |>
  select(county_code, state, region, afd_pct)

gerda_regions |> count(region)

gerda_regions |>
  ggplot(aes(x = region, y = afd_pct, fill = region)) +
  geom_boxplot() +
  labs(
    title = "AfD vote share by county: East vs. West (2025)",
    x     = NULL,
    y     = "AfD share (%)"
  ) +
  theme(legend.position = "none")


# ============================================================
# 8. DIFFERENCE IN MEANS: THE EAST-WEST AFD GAP ----
# ============================================================

diff_stats <- gerda_regions |>
  group_by(region) |>
  summarize(
    mean_afd = mean(afd_pct),
    var_afd  = var(afd_pct),
    n        = n()
  )
diff_stats

mean_east <- diff_stats |>
  filter(region == "East") |>
  pull(mean_afd)
mean_west <- diff_stats |>
  filter(region == "West") |>
  pull(mean_afd)
var_east <- diff_stats |>
  filter(region == "East") |>
  pull(var_afd)
var_west <- diff_stats |>
  filter(region == "West") |>
  pull(var_afd)
n_east <- diff_stats |>
  filter(region == "East") |>
  pull(n)
n_west <- diff_stats |>
  filter(region == "West") |>
  pull(n)

diff_observed <- mean_east - mean_west
diff_observed


# ============================================================
# 9. CONFIDENCE INTERVALS FOR A DIFFERENCE IN MEANS ----
# ============================================================
#   1. analytically  -- Normal approximation, both groups' own variance
#   2. by simulation -- draw each group's sampling distribution, subtract
#   3. by bootstrap  -- resample each group with replacement, subtract
#   4. t.test()      -- same SE, t instead of Normal multiplier (Week 6)

# 1. analytical: SE = sqrt(sigma_1^2 / n_1 + sigma_2^2 / n_2)
se_diff <- sqrt(var_east / n_east + var_west / n_west)
ci_diff_analytical <- diff_observed + qnorm(c(0.025, 0.975)) * se_diff
ci_diff_analytical

# 2. simulation
se_east <- sqrt(var_east / n_east)
se_west <- sqrt(var_west / n_west)

diff_sim <- tibble(
  sim_east = rnorm(nsim, mean_east, se_east),
  sim_west = rnorm(nsim, mean_west, se_west)
) |>
  mutate(diff = sim_east - sim_west)

diff_values <- diff_sim |> pull(diff)
ci_diff_simulated <- quantile(diff_values, c(0.025, 0.975))
ci_diff_simulated

diff_sim |>
  ggplot(aes(x = diff)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Simulated distribution of the AfD gap",
    x     = "Simulated difference in means (percentage points)",
    y     = "Density"
  )

# 3. bootstrap: t.test() and sample() need plain vectors
afd_east <- gerda_regions |>
  filter(region == "East") |>
  pull(afd_pct)
afd_west <- gerda_regions |>
  filter(region == "West") |>
  pull(afd_pct)

n_boot_diff <- 1000

boot_east <- expand_grid(trial = 1:n_boot_diff, row = 1:n_east) |>
  group_by(trial) |>
  summarize(boot_mean = mean(sample(afd_east, n(), replace = TRUE)), .groups = "drop")

boot_west <- expand_grid(trial = 1:n_boot_diff, row = 1:n_west) |>
  group_by(trial) |>
  summarize(boot_mean = mean(sample(afd_west, n(), replace = TRUE)), .groups = "drop")

boot_east_values <- boot_east |> pull(boot_mean)
boot_west_values <- boot_west |> pull(boot_mean)
diff_boot_values <- boot_east_values - boot_west_values

ci_diff_bootstrap <- quantile(diff_boot_values, c(0.025, 0.975))
ci_diff_bootstrap

# 4. t.test()
t_result <- t.test(afd_east, afd_west)
t_result

ci_diff_t <- t_result |>
  pluck("conf.int") |>
  as.numeric()
ci_diff_t

# comparing the four CIs -- same result, different assumptions: analytical
# and simulation assume Normality, t.test() a t multiplier, bootstrap the
# data's own shape; all assume independent draws from one distribution
diff_comparison <- tibble(
  method = c("Analytical", "Simulation", "Bootstrap", "t.test()"),
  lower  = c(ci_diff_analytical[1], ci_diff_simulated[1], ci_diff_bootstrap[1], ci_diff_t[1]),
  upper  = c(ci_diff_analytical[2], ci_diff_simulated[2], ci_diff_bootstrap[2], ci_diff_t[2])
)
diff_comparison

diff_comparison_plot <- diff_comparison |>
  mutate(
    mid = diff_observed,
    y   = -0.02 * row_number()
  )

tibble(diff = diff_boot_values) |>
  ggplot(aes(x = diff)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_vline(xintercept = diff_observed, color = "firebrick", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = diff_observed, y = 0.44, label = "observed gap (no separate truth)", color = "firebrick", size = 3.2, hjust = -0.03) +
  geom_pointrange(
    data = diff_comparison_plot,
    aes(x = mid, y = y, xmin = lower, xmax = upper),
    inherit.aes = FALSE,
    color = "#2f7f93",
    linewidth = 0.8
  ) +
  geom_text(
    data = diff_comparison_plot,
    aes(x = lower, y = y, label = method),
    inherit.aes = FALSE, hjust = 1.15, size = 3.3
  ) +
  labs(
    title = "The bootstrap distribution and all four CIs",
    x     = "Difference in means, East minus West (percentage points)",
    y     = "Density"
  )


# ============================================================
# 10. TRY IT: A 99% CI FOR THE AFD'S POLLED MEAN SHARE ----
# ============================================================
# Your task: using the same final-stretch polling data and the
# analytical method, build a 99% (not 95%) confidence interval for the
# AfD's mean polled share. How does it compare to the 95% interval above?


# ============================================================
# 11. TRY IT: BOOTSTRAP CI FOR A SHARE OF STRONG POLLS ----
# ============================================================
# Your task: build a bootstrap 95% CI for the *proportion* of these
# same 25 polls that put the AfD at 20.5% or higher, reusing the same
# resampling pattern as the mean's bootstrap CI above.


# ============================================================
# 12. TRY IT: REPEAT THE DIFFERENCE-IN-MEANS FOR THE GREENS ----
# ============================================================
# Your task: redo the East/West difference-in-means comparison using
# Green vote share instead of AfD's (t.test() is enough). Does the gap
# point the same direction?
