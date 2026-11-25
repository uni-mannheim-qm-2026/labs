# ============================================================
# WEEK 12 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, modelsummary, marginaleffects, ggridges, lmtest, optimizeR)
set.seed(1212)

# ============================================================
# 1. DATA: REBELLIONS IN THE EUROPEAN PARLIAMENT ----
# ============================================================
# HowTheyVote.eu, one row per MEP: rebellions (votes against the majority of
# the own group) in 1,000 random votes (Open Database License)
group_labels <- tribble(
  ~ep_group,   ~group_label,
  "GUE_NGL",   "The Left",
  "GREEN_EFA", "Greens/EFA",
  "SD",        "S&D",
  "RENEW",     "Renew",
  "EPP",       "EPP",
  "ECR",       "ECR",
  "PFE",       "PfE",
  "ESN",       "ESN"
)

rebellions <- read_csv("data/ep_rebellions.csv") |>
  mutate(ep_group = fct_relevel(ep_group, "EPP"))
rebellions

rebellions |>
  ggplot(aes(x = rebellions)) +
  geom_histogram(binwidth = 10, fill = "#2f7f93") +
  labs(
    title = "Rebellions per MEP in 1,000 votes",
    x     = "Rebellions",
    y     = "MEPs"
  )

group_means <- rebellions |>
  group_by(ep_group) |>
  summarize(
    meps            = n(),
    mean_rebellions = mean(rebellions)
  ) |>
  left_join(group_labels, by = "ep_group") |>
  arrange(desc(mean_rebellions)) |>
  mutate(group_label = fct_inorder(group_label))
group_means

group_means |>
  ggplot(aes(x = group_label, y = mean_rebellions)) +
  geom_col(fill = "#2f7f93") +
  labs(
    title = "Average rebellions by group",
    x     = NULL,
    y     = "Average rebellions"
  )

# ============================================================
# 2. A COUNT OUTCOME: OLS ----
# ============================================================
# Rebellions_i = beta_0 + beta_1 * Age_i + group dummies (EPP = reference) + epsilon_i
ols_model <- lm(rebellions ~ age + ep_group, data = rebellions)
ols_model |>
  tidy()

ols_sigma <- ols_model |>
  glance() |>
  pull(sigma)
ols_sigma

# predicted value = fitted value + Normal error
ols_values <- ols_model |>
  augment() |>
  mutate(predicted = .fitted + rnorm(n(), 0, ols_sigma))

ols_negative <- ols_values |>
  summarize(share_negative = mean(predicted < 0)) |>
  pull(share_negative)
ols_negative

ols_values |>
  select(
    Observed        = rebellions,
    `OLS predicted` = predicted
  ) |>
  pivot_longer(everything(), names_to = "source", values_to = "count") |>
  ggplot(aes(x = count, fill = source)) +
  geom_histogram(binwidth = 10, boundary = 0, position = "identity", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c(
    "Observed"      = "grey50",
    "OLS predicted" = "#2f7f93"
  )) +
  labs(
    title = "OLS on a count outcome",
    x     = "Rebellions",
    y     = "MEPs",
    fill  = NULL
  )

# ============================================================
# 3. A COUNT OUTCOME: POISSON ----
# ============================================================
# Y_i ~ Poisson(lambda), lambda_hat_ML = mean(y)
lambda_hat <- rebellions |>
  summarize(lambda_hat = mean(rebellions)) |>
  pull(lambda_hat)
lambda_hat

rpois(10, lambda = lambda_hat)

# one Poisson vs. one per group: Y_i ~ Poisson(lambda_g)
one_poisson <- group_means |>
  mutate(
    panel    = "One Poisson",
    estimate = lambda_hat
  )
group_poissons <- group_means |>
  mutate(
    panel    = "One Poisson per group",
    estimate = mean_rebellions
  )

bind_rows(one_poisson, group_poissons) |>
  mutate(position = as.numeric(group_label)) |>
  ggplot(aes(x = group_label)) +
  geom_point(aes(y = mean_rebellions, color = "Observed mean per group"), size = 3) +
  geom_segment(
    aes(x = position - 0.5, xend = position + 0.5, y = estimate, yend = estimate, color = "Estimated expected count"),
    linewidth = 1
  ) +
  facet_wrap(~panel) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_color_manual(values = c(
    "Observed mean per group"  = "grey50",
    "Estimated expected count" = "#2f7f93"
  )) +
  labs(
    title = "One Poisson vs. one Poisson per group",
    x     = NULL,
    y     = "Rebellions",
    color = NULL
  )

# ============================================================
# 4. COMBINING BOTH WITH A LINK ----
# ============================================================
# eta_i = X_i * beta, lambda_i = g(eta_i) = exp(eta_i), Y_i ~ Poisson(lambda_i)
g <- function(eta) {
  exp(eta)
}

g(c(-2, 0, 2))

poisson_model <- glm(
  rebellions ~ age + ep_group,
  family = poisson(link = "log"),
  data   = rebellions
)
poisson_model |>
  tidy()

# rate ratios: factors on the expected number of rebellions
poisson_model |>
  tidy(exponentiate = TRUE) |>
  select(term, estimate)

poisson_age <- poisson_model |>
  tidy() |>
  filter(term == "age")
poisson_age_z <- poisson_age |>
  pull(statistic)
poisson_age_z

# ============================================================
# 5. OVERDISPERSION ----
# ============================================================
# Pearson chi-squared / residual df: about 1 if the Poisson is right
poisson_df <- poisson_model |>
  glance() |>
  pull(df.residual)

dispersion <- poisson_model |>
  augment(type.residuals = "pearson") |>
  summarize(dispersion = sum(.resid^2) / poisson_df) |>
  pull(dispersion)
dispersion

# ============================================================
# 6. THE NEGATIVE BINOMIAL ----
# ============================================================
# standard form: all failures before the r-th success, NegBin(r = size, p = prob)
# here: refusals before the 5th signature
signatures_needed <- 5
p_sign <- 0.5

dnbinom(3, size = signatures_needed, prob = p_sign) # exactly 3 refusals
rnbinom(10, size = signatures_needed, prob = p_sign)

# next to a Poisson with the same mean
mean_refusals <- signatures_needed * (1 - p_sign) / p_sign

tibble(refusals = 0:20) |>
  mutate(
    `Negative binomial` = dnbinom(refusals, size = signatures_needed, prob = p_sign),
    Poisson             = dpois(refusals, lambda = mean_refusals)
  ) |>
  pivot_longer(-refusals, names_to = "distribution", values_to = "probability") |>
  ggplot(aes(x = refusals, y = probability, fill = distribution)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Negative binomial" = "#2f7f93",
    "Poisson"           = "darkorange"
  )) +
  labs(
    title = "Refusals before the 5th signature",
    x     = "Refusals",
    y     = "Probability",
    fill  = NULL
  )

# form for regression: NegBinAlt(lambda, theta) = NegBin(r = theta, p = theta / (theta + lambda))
# theta = size, lambda = mu; same seed, same draws
set.seed(1212)
rnbinom(10, size = signatures_needed, mu = mean_refusals)

set.seed(1212)
rnbinom(10, size = signatures_needed, prob = signatures_needed / (signatures_needed + mean_refusals))

# Y_i ~ NegBinAlt(lambda_i, theta), Var(Y_i) = lambda_i + lambda_i^2 / theta
nb_model <- MASS::glm.nb(
  rebellions ~ age + ep_group,
  data = rebellions
)
nb_model |>
  tidy()

theta_hat <- nb_model |>
  pluck("theta")
theta_hat

nb_age_z <- nb_model |>
  tidy() |>
  filter(term == "age") |>
  pull(statistic) |>
  as.numeric()
nb_age_z

modelsummary(
  list("Poisson" = poisson_model, "Negative binomial" = nb_model),
  output   = "tinytable",
  coef_map = c(
    "age"               = "Age",
    "ep_groupGUE_NGL"   = "The Left",
    "ep_groupGREEN_EFA" = "Greens/EFA",
    "ep_groupSD"        = "S&D",
    "ep_groupRENEW"     = "Renew",
    "ep_groupECR"       = "ECR",
    "ep_groupPFE"       = "PfE",
    "ep_groupESN"       = "ESN",
    "(Intercept)"       = "Intercept"
  ),
  stars    = TRUE,
  gof_map  = c("nobs", "logLik", "aic"),
  fmt      = 3
)

# ============================================================
# 7. POISSON VS. NEGATIVE BINOMIAL ----
# ============================================================
# coefficients with 95% CIs, age per 10 years
age_step <- 10

poisson_coef <- poisson_model |>
  tidy(conf.int = TRUE) |>
  mutate(model = "Poisson")
nb_coef <- nb_model |>
  tidy(conf.int = TRUE) |>
  mutate(
    model     = "Negative binomial",
    statistic = as.numeric(statistic)
  )

model_coefs <- bind_rows(poisson_coef, nb_coef) |>
  filter(term != "(Intercept)") |>
  mutate(
    ep_group = str_remove(term, "ep_group"),
    step     = if_else(term == "age", age_step, 1),
    estimate = estimate * step,
    lower    = conf.low * step,
    upper    = conf.high * step
  ) |>
  left_join(group_labels, by = "ep_group") |>
  mutate(
    label = coalesce(group_label, "Age (10 years)"),
    label = fct_reorder(label, estimate)
  ) |>
  select(model, label, estimate, lower, upper)
model_coefs

model_coefs |>
  ggplot(aes(x = estimate, y = label, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = c(
    "Negative binomial" = "#2f7f93",
    "Poisson"           = "darkorange"
  )) +
  labs(
    title = "Coefficients, Poisson vs. negative binomial",
    x     = "Coefficient (log scale), EPP as reference",
    y     = NULL,
    color = NULL
  )

# expected values: the models agree on the means
expected_values <- rebellions |>
  mutate(
    Observed            = rebellions,
    Poisson             = predict(poisson_model, type = "response"),
    `Negative binomial` = predict(nb_model, type = "response")
  ) |>
  pivot_longer(
    c(Observed, Poisson, `Negative binomial`),
    names_to  = "source",
    values_to = "count"
  ) |>
  mutate(source = fct_relevel(source, "Poisson", "Negative binomial", "Observed"))

expected_values |>
  ggplot(aes(x = count, y = source, fill = source)) +
  geom_density_ridges(alpha = 0.7, bandwidth = 4, scale = 0.95, from = 0) +
  scale_fill_manual(values = c(
    "Observed"          = "grey50",
    "Negative binomial" = "#2f7f93",
    "Poisson"           = "darkorange"
  )) +
  labs(
    title = "Observed and expected rebellions, one per MEP",
    x     = "Rebellions",
    y     = NULL
  ) +
  theme(legend.position = "none")

# range in which each model expects 95% of the counts
average_age <- rebellions |>
  summarize(mean(age)) |>
  pull()
group_scenario <- group_labels |>
  mutate(age = average_age)

poisson_ranges <- group_scenario |>
  mutate(
    model    = "Poisson",
    expected = predict(poisson_model, newdata = group_scenario, type = "response"),
    lower    = qpois(0.025, lambda = expected),
    upper    = qpois(0.975, lambda = expected)
  )
nb_ranges <- group_scenario |>
  mutate(
    model    = "Negative binomial",
    expected = predict(nb_model, newdata = group_scenario, type = "response"),
    lower    = qnbinom(0.025, size = theta_hat, mu = expected),
    upper    = qnbinom(0.975, size = theta_hat, mu = expected)
  )

count_ranges <- bind_rows(poisson_ranges, nb_ranges) |>
  mutate(
    model       = fct_relevel(model, "Poisson"),
    group_label = fct_reorder(group_label, expected)
  )
count_ranges |>
  select(model, group_label, expected, lower, upper)

observed_counts <- rebellions |>
  left_join(group_labels, by = "ep_group")

count_ranges |>
  ggplot(aes(y = group_label)) +
  geom_jitter(
    data   = observed_counts,
    aes(x = rebellions),
    height = 0.2,
    width  = 0,
    alpha  = 0.25,
    color  = "grey50"
  ) +
  geom_pointrange(
    aes(x = expected, xmin = lower, xmax = upper, color = model),
    linewidth = 1
  ) +
  facet_wrap(~model) +
  scale_color_manual(values = c(
    "Negative binomial" = "#2f7f93",
    "Poisson"           = "darkorange"
  )) +
  labs(
    title = "Same mean, different spread",
    x     = "Rebellions",
    y     = NULL
  ) +
  theme(legend.position = "none")

# share of observed counts within these ranges
count_coverage <- rebellions |>
  mutate(
    expected_poisson = predict(poisson_model, type = "response"),
    expected_nb      = predict(nb_model, type = "response")
  ) |>
  summarize(
    poisson = mean(
      rebellions >= qpois(0.025, lambda = expected_poisson) &
        rebellions <= qpois(0.975, lambda = expected_poisson)
    ),
    nb = mean(
      rebellions >= qnbinom(0.025, size = theta_hat, mu = expected_nb) &
        rebellions <= qnbinom(0.975, size = theta_hat, mu = expected_nb)
    )
  )
count_coverage

# ============================================================
# 8. LIKELIHOOD RATIO TEST ----
# ============================================================
# LR = 2 * (log L_U - log L_R) ~ chi^2 with 1 df
loglik_poisson <- poisson_model |>
  glance() |>
  pull(logLik) |>
  as.numeric()
loglik_nb <- nb_model |>
  glance() |>
  pull(logLik) |>
  as.numeric()

lr <- 2 * (loglik_nb - loglik_poisson)
lr

critical_value <- qchisq(0.95, df = 1)
critical_value

tibble(x = seq(0, 10, by = 0.05)) |>
  mutate(density = dchisq(x, df = 1)) |>
  ggplot(aes(x = x, y = density)) +
  geom_line(aes(color = "Chi-squared, 1 df"), linewidth = 1) +
  geom_vline(aes(xintercept = critical_value, color = "Critical value"), linetype = "dashed") +
  scale_color_manual(values = c(
    "Chi-squared, 1 df" = "#2f7f93",
    "Critical value"    = "darkorange"
  )) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Null distribution of the likelihood ratio",
    x     = "LR",
    y     = "Density",
    color = NULL
  )

# shortcut
lrtest(poisson_model, nb_model)

# ============================================================
# 9. MAXIMUM LIKELIHOOD ESTIMATION ----
# ============================================================
# theta here is the vector of all parameters, not NegBinAlt's theta
x_beta <- function(theta, age) {
  pluck(theta, 1) + pluck(theta, 2) * age
}

l <- function(theta, y, age, log = FALSE) {
  dpois(y, lambda = g(x_beta(theta, age)), log = log)
}

ll <- function(theta, y, age) {
  sum(l(theta, y, age, log = TRUE))
}

rebellion_counts <- rebellions |>
  pull(rebellions)
age_values <- rebellions |>
  pull(age)

mean_count <- rebellions |>
  summarize(mean(rebellions)) |>
  pull()

start_poisson <- c(log(mean_count), 0)
ll(start_poisson, rebellion_counts, age_values)

# convergence code 1: the optimizer didn't reach the top
optimizer <- Optimizer$new(which = "stats::nlminb")

fit_raw <- optimizer$maximize(
  objective = ll,
  initial   = start_poisson,
  y         = rebellion_counts,
  age       = age_values
)
fit_raw |>
  pluck("convergence")

# centering changes the intercept, not the slope
rebellions_centered <- rebellions |>
  mutate(age_centered = age - mean(age))

age_centered_values <- rebellions_centered |>
  pull(age_centered)

fit_poisson <- optimizer$maximize(
  objective = ll,
  initial   = start_poisson,
  y         = rebellion_counts,
  age       = age_centered_values
)
fit_poisson |>
  pluck("convergence")

theta_poisson <- fit_poisson |>
  pluck("parameter")
theta_poisson

# negative binomial: only l() changes; third entry of theta = log of NegBinAlt's theta
l <- function(theta, y, age, log = FALSE) {
  dnbinom(y, mu = g(x_beta(theta, age)), size = exp(pluck(theta, 3)), log = log)
}

fit_nb_ml <- optimizer$maximize(
  objective = ll,
  initial   = c(start_poisson, 0),
  y         = rebellion_counts,
  age       = age_centered_values
)
fit_nb_ml |>
  pluck("convergence")

theta_nb <- fit_nb_ml |>
  pluck("parameter")
theta_nb

poisson_age_only <- glm(
  rebellions ~ age_centered,
  family = poisson(link = "log"),
  data   = rebellions_centered
)
nb_age_only <- MASS::glm.nb(
  rebellions ~ age_centered,
  data = rebellions_centered
)

poisson_coefs <- poisson_age_only |>
  tidy() |>
  pull(estimate)
nb_coefs <- nb_age_only |>
  tidy() |>
  pull(estimate)
nb_theta <- nb_age_only |>
  pluck("theta")

tibble(
  parameter   = c("beta_0", "beta_age", "theta"),
  poisson_ml  = c(theta_poisson, NA),
  poisson_glm = c(poisson_coefs, NA),
  nb_ml       = c(pluck(theta_nb, 1), pluck(theta_nb, 2), exp(pluck(theta_nb, 3))),
  nb_glm      = c(nb_coefs, nb_theta)
)

modelsummary(
  list("Poisson" = poisson_age_only, "Negative binomial" = nb_age_only),
  output   = "tinytable",
  coef_map = c(
    "age_centered" = "Age (centered)",
    "(Intercept)"  = "Intercept"
  ),
  stars    = TRUE,
  gof_map  = c("nobs", "logLik", "aic"),
  fmt      = 4
)

# ============================================================
# 10. EXPECTED VS. PREDICTED VALUES ----
# ============================================================
# expected value: lambda_i; predicted value: a draw from the distribution
count_values <- rebellions |>
  mutate(
    expected          = predict(nb_model, type = "response"),
    predicted_nb      = rnbinom(n(), size = theta_hat, mu = expected),
    predicted_poisson = rpois(n(), lambda = expected)
  )
count_values |>
  select(rebellions, expected, predicted_nb, predicted_poisson)

count_values |>
  select(
    Observed            = rebellions,
    `Negative binomial` = predicted_nb,
    Poisson             = predicted_poisson
  ) |>
  pivot_longer(everything(), names_to = "source", values_to = "count") |>
  mutate(source = fct_relevel(source, "Poisson", "Negative binomial", "Observed")) |>
  ggplot(aes(x = count, y = source, fill = source)) +
  geom_density_ridges(alpha = 0.7, bandwidth = 8, scale = 0.95, from = 0) +
  scale_fill_manual(values = c(
    "Observed"          = "grey50",
    "Negative binomial" = "#2f7f93",
    "Poisson"           = "darkorange"
  )) +
  labs(
    title = "Observed and predicted rebellions",
    x     = "Rebellions",
    y     = NULL,
    fill  = NULL
  ) +
  theme(legend.position = "none")

# ============================================================
# 11. QUANTITIES OF INTEREST ----
# ============================================================
n_boot <- 1000

age_mean <- rebellions |>
  summarize(mean(age)) |>
  pull()
age_mean

age_young <- 40
age_old <- 70

# expected values with bootstrap intervals
ev_boot <- predictions(
  nb_model,
  newdata = datagrid(
    ep_group = c("EPP", "SD", "PFE"),
    age      = age_mean
  )
) |>
  inferences(method = "boot", R = n_boot)
ev_boot

# FD = E(Y | EPP, 70) - E(Y | EPP, 40)
fd_boot <- comparisons(
  nb_model,
  variables = list(age = c(age_young, age_old)),
  newdata   = datagrid(ep_group = "EPP")
) |>
  inferences(method = "boot", R = n_boot)
fd_boot

ev_draws <- ev_boot |>
  get_draws() |>
  as_tibble() |>
  left_join(group_labels, by = "ep_group") |>
  select(group_label, draw)
ev_draws

ev_draws |>
  mutate(group_label = fct_relevel(group_label, "S&D", "EPP", "PfE")) |>
  ggplot(aes(x = draw, y = group_label, fill = group_label)) +
  geom_density_ridges(alpha = 0.7, bandwidth = 2) +
  scale_fill_manual(values = c(
    "S&D" = "#2f7f93",
    "EPP" = "grey50",
    "PfE" = "darkorange"
  )) +
  labs(
    title = "Expected rebellions by group",
    x     = "Expected rebellions",
    y     = NULL,
    fill  = NULL
  )

# ============================================================
# 12. PREDICTED VALUES ----
# ============================================================
# Y^(r) ~ NegBinAlt(E(Y | X)^(r), theta_hat), one draw per bootstrap sample
boot_pvs <- ev_draws |>
  filter(group_label == "EPP") |>
  mutate(predicted = rnbinom(n(), size = theta_hat, mu = draw))

ev_pv <- boot_pvs |>
  pivot_longer(c(draw, predicted), names_to = "type", values_to = "value") |>
  mutate(
    type = if_else(
      type == "draw",
      "Expected value: estimation uncertainty",
      "Predicted value: estimation + fundamental uncertainty"
    )
  )

ev_pv |>
  group_by(type) |>
  summarize(
    estimate = mean(value),
    lower    = quantile(value, 0.025),
    upper    = quantile(value, 0.975)
  )

ev_pv |>
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 5, fill = "#2f7f93") +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  labs(
    title = "Expected and predicted rebellions, EPP",
    x     = "Rebellions",
    y     = "Bootstrap samples"
  )

# ============================================================
# 13. TRY IT: NEWCOMERS ----
# ============================================================
# Do MEPs who are new to Parliament follow their group more closely, or are
# they less socialized into group discipline? newcomer is 1 for MEPs
# who weren't part of the 2019–2024 Parliament.
#
# Your task: add newcomer to the negative binomial model. Report its rate
# ratio with a 95% confidence interval, and what happens to the age coefficient.

# ============================================================
# 14. TRY IT: RENEW AND THE ECR ----
# ============================================================
# Renew sits in the centre, the ECR on the right. Does that go along with
# different levels of group discipline?
#
# Your task: report the first difference in expected rebellions between the
# ECR and Renew, for an MEP of average age, with a
# 95% bootstrap interval.
