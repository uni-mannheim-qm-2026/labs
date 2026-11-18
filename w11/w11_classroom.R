# ============================================================
# WEEK 11 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, scales, broom, modelsummary, marginaleffects, patchwork, optimizeR)
set.seed(1111)

# ============================================================
# 1. DATA: ROLL-CALL VOTES IN THE EUROPEAN PARLIAMENT ----
# ============================================================
# HowTheyVote.eu, one row per MEP and vote (Open Database License)
ep_votes <- read_csv("data/ep_votes.csv")
ep_votes

ep_votes |>
  filter(vote == "nature_restoration") |>
  count(position)

# political groups on a left (1) to right (7) scale
left_right_scale <- tribble(
  ~ep_group,   ~group_label, ~left_right,
  "GUE_NGL",   "The Left",   1,
  "GREEN_EFA", "Greens/EFA", 2,
  "SD",        "S&D",        3,
  "RENEW",     "Renew",      4,
  "EPP",       "EPP",        5,
  "ECR",       "ECR",        6,
  "ID",        "ID",         7
)

# yes/no votes only; non-attached MEPs and missing ages drop out
nature <- ep_votes |>
  filter(
    vote == "nature_restoration",
    position %in% c("FOR", "AGAINST")
  ) |>
  inner_join(left_right_scale, by = "ep_group") |>
  drop_na(age) |>
  mutate(yes = if_else(position == "FOR", 1, 0))
nature

group_shares <- nature |>
  group_by(left_right, group_label) |>
  summarize(
    share_yes = mean(yes),
    meps      = n(),
    .groups   = "drop"
  ) |>
  mutate(
    group_label = fct_inorder(group_label),
    right       = cumsum(meps),
    left        = right - meps,
    center      = (left + right) / 2
  )
group_shares

# column width = number of MEPs
group_centers <- group_shares |>
  pull(center)
group_names <- group_shares |>
  pull(group_label)

group_shares |>
  ggplot() +
  geom_rect(
    aes(xmin = left, xmax = right, ymin = 0, ymax = share_yes, fill = "Yes"),
    color = "white"
  ) +
  geom_rect(
    aes(xmin = left, xmax = right, ymin = share_yes, ymax = 1, fill = "No"),
    color = "white"
  ) +
  scale_x_continuous(
    breaks = group_centers,
    labels = group_names,
    guide  = guide_axis(n.dodge = 2)
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("Yes" = "#2f7f93", "No" = "grey85")) +
  labs(
    title = "Votes by group, Nature Restoration Law",
    x     = "Column width: number of MEPs",
    y     = "Share of the group's votes",
    fill  = "Vote"
  )

# ============================================================
# 2. A BINARY OUTCOME: OLS ----
# ============================================================
# Yes_i = beta_0 + beta_1 * LeftRight_i + epsilon_i
ols_model <- lm(yes ~ left_right, data = nature)
ols_model |>
  tidy()

ols_values <- ols_model |>
  augment()

# fitted "probabilities" outside [0, 1]
ols_values |>
  summarize(
    lowest  = min(.fitted),
    highest = max(.fitted),
    outside = mean(.fitted < 0 | .fitted > 1)
  )

ols_values |>
  ggplot(aes(x = left_right)) +
  geom_jitter(aes(y = yes, color = "Observed votes"), width = 0.2, height = 0.03, alpha = 0.3) +
  geom_line(aes(y = .fitted, color = "OLS line"), linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Observed votes" = "grey50",
    "OLS line"       = "#2f7f93"
  )) +
  labs(
    title = "OLS on a 0/1 outcome",
    x     = "Left-right position",
    y     = "Yes vote",
    color = NULL
  )

# ============================================================
# 3. A BINARY OUTCOME: BERNOULLI ----
# ============================================================
# Y_i ~ Bernoulli(pi), pi_hat_ML = mean(y)
pi_hat <- nature |>
  summarize(pi_hat = mean(yes)) |>
  pull(pi_hat)
pi_hat

rbinom(10, size = 1, prob = pi_hat)

# one Bernoulli vs. one per group: Y_i ~ Bernoulli(pi_g)
one_bernoulli <- group_shares |>
  mutate(
    panel    = "One Bernoulli",
    estimate = pi_hat
  )
group_bernoullis <- group_shares |>
  mutate(
    panel    = "One Bernoulli per group",
    estimate = share_yes
  )

bind_rows(one_bernoulli, group_bernoullis) |>
  ggplot(aes(x = left_right)) +
  geom_point(aes(y = share_yes, color = "Observed share per group"), size = 3) +
  geom_segment(
    aes(x = left_right - 0.5, xend = left_right + 0.5, y = estimate, yend = estimate, color = "Estimated probability"),
    linewidth = 1
  ) +
  facet_wrap(~panel) +
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = c(
    "Observed share per group" = "grey50",
    "Estimated probability"    = "#2f7f93"
  )) +
  labs(
    title = "One Bernoulli vs. one Bernoulli per group",
    x     = "Left-right position",
    y     = "Share voting yes",
    color = NULL
  )

# ============================================================
# 4. THE LOGIT MODEL ----
# ============================================================
# Y_i ~ Bernoulli(pi_i), pi_i = g(eta_i) = exp(eta_i) / (1 + exp(eta_i))
g <- function(eta) {
  exp(eta) / (1 + exp(eta))
}

g(c(-5, 0, 5))
plogis(c(-5, 0, 5))

logit_simple <- glm(yes ~ left_right, family = binomial(link = "logit"), data = nature)
logit_simple |>
  tidy()

# eta_i = beta_0 + beta_1 * LeftRight_i + beta_2 * Age_i
logit_model <- glm(yes ~ left_right + age, family = binomial(link = "logit"), data = nature)
logit_model |>
  tidy()

# coefficients on different scales: not comparable
ols_full <- lm(yes ~ left_right + age, data = nature)

modelsummary(
  list("OLS" = ols_full, "Logit" = logit_model),
  output   = "tinytable",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "left_right"  = "Left-right",
    "age"         = "Age"
  ),
  stars    = TRUE,
  gof_map  = c("nobs", "logLik"),
  fmt      = 3
)

# geometric mean probability per observed vote: exp(log L / N)
logit_model |>
  glance() |>
  mutate(probability_per_vote = exp(as.numeric(logLik) / nobs)) |>
  select(logLik, nobs, probability_per_vote)

# ============================================================
# 5. MAXIMUM LIKELIHOOD ESTIMATION ----
# ============================================================
# log L(beta | y, X) = sum_i log f_Bern(y_i; g(X_i beta))
x_beta <- function(theta, left_right, age) {
  pluck(theta, 1) + pluck(theta, 2) * left_right + pluck(theta, 3) * age
}

l <- function(theta, y, left_right, age, log = FALSE) {
  dbinom(y, size = 1, prob = g(x_beta(theta, left_right, age)), log = log)
}

ll <- function(theta, y, left_right, age) {
  sum(l(theta, y, left_right, age, log = TRUE))
}

yes_votes <- nature |>
  pull(yes)
left_right_values <- nature |>
  pull(left_right)
age_values <- nature |>
  pull(age)

# start at 0: pi_i = 0.5 for everybody
optimizer <- Optimizer$new(which = "stats::nlminb")

theta_logit <- optimizer$maximize(
  objective  = ll,
  initial    = c(0, 0, 0),
  y          = yes_votes,
  left_right = left_right_values,
  age        = age_values
) |>
  pluck("parameter")
theta_logit

logit_model |>
  tidy() |>
  select(term, glm = estimate) |>
  mutate(ml = theta_logit)

# ============================================================
# 6. MODEL FIT ----
# ============================================================
# classification table, cutoff 0.5
classified <- logit_model |>
  augment(type.predict = "response") |>
  mutate(classified = if_else(.fitted > 0.5, 1, 0))

classified |>
  count(yes, classified) |>
  pivot_wider(names_from = classified, values_from = n, names_prefix = "classified_")

# PCP = share on the diagonal, PMC = max(mean(y), 1 - mean(y))
classified |>
  summarize(
    pcp = mean(yes == classified),
    pmc = max(mean(yes), 1 - mean(yes))
  )

# n_op: observed o, classified p; yes votes are the positives
# precision = n_11 / (n_01 + n_11), recall = n_11 / (n_10 + n_11)
# specificity = n_00 / (n_00 + n_01), F1 = 2 * prec * rec / (prec + rec)
classified |>
  summarize(
    accuracy    = mean(yes == classified),
    precision   = sum(yes == 1 & classified == 1) / sum(classified == 1),
    recall      = sum(yes == 1 & classified == 1) / sum(yes == 1),
    specificity = sum(yes == 0 & classified == 0) / sum(yes == 0),
    f1          = 2 * precision * recall / (precision + recall)
  )

# separation plot
separation <- classified |>
  arrange(.fitted) |>
  mutate(
    rank = row_number(),
    vote = if_else(yes == 1, "Yes", "No")
  )

expected_noes <- separation |>
  summarize(n() - sum(.fitted)) |>
  pull()

separation |>
  ggplot(aes(x = rank)) +
  geom_tile(aes(y = 0.5, height = 1, fill = vote)) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  annotate("point", x = expected_noes, y = -0.05, shape = 17, size = 3) +
  scale_fill_manual(values = c("Yes" = "#2f7f93", "No" = "grey85")) +
  labs(
    title = "Separation plot, Nature Restoration Law",
    x     = "MEPs, sorted by predicted probability",
    y     = "Predicted probability",
    fill  = "Observed vote"
  )

# ============================================================
# 7. EXPECTED VS. PREDICTED VALUES ----
# ============================================================
# expected: pi_hat_i = g(X_i beta_hat), predicted: Y_i ~ Bernoulli(pi_hat_i)
logit_values <- logit_model |>
  augment(type.predict = "response") |>
  mutate(predicted = rbinom(n(), size = 1, prob = .fitted))
logit_values |>
  select(yes, left_right, age, .fitted, predicted)

logit_values |>
  ggplot(aes(x = left_right)) +
  geom_jitter(aes(y = yes, color = "Observed votes"), width = 0.2, height = 0.03, alpha = 0.3) +
  geom_jitter(aes(y = predicted, color = "Predicted: Bernoulli draw"), width = 0.2, height = 0.03, alpha = 0.3) +
  geom_point(aes(y = .fitted, color = "Expected: augment()")) +
  scale_color_manual(values = c(
    "Observed votes"            = "grey60",
    "Predicted: Bernoulli draw" = "darkorange",
    "Expected: augment()"       = "#2f7f93"
  )) +
  labs(
    title = "Expected and predicted values",
    x     = "Left-right position",
    y     = "Yes vote",
    color = NULL
  )

# ============================================================
# 8. QUANTITIES OF INTEREST: RENEW VS. EPP ----
# ============================================================
# scenario: Renew and EPP positions at average age
n_boot <- 1000

age_mean <- nature |>
  summarize(mean(age)) |>
  pull()
age_mean

renew <- left_right_scale |>
  filter(ep_group == "RENEW") |>
  pull(left_right)
epp <- left_right_scale |>
  filter(ep_group == "EPP") |>
  pull(left_right)

scenarios <- tibble(
  left_right = c(renew, epp),
  age        = age_mean
)

# E(Yes | LeftRight, mean age) = g(beta_0 + beta_1 * LeftRight + beta_2 * mean age)
ev_boot <- predictions(logit_model, newdata = scenarios) |>
  inferences(method = "boot", R = n_boot)
ev_boot

# FD = E(Yes | EPP, mean age) - E(Yes | Renew, mean age)
fd_boot <- comparisons(
  logit_model,
  variables = list(left_right = c(renew, epp)),
  newdata   = datagrid(age = age_mean, left_right = renew)
) |>
  inferences(method = "boot", R = n_boot)
fd_boot

# one row per bootstrap sample
ev_draws <- ev_boot |>
  get_draws() |>
  as_tibble() |>
  mutate(quantity = if_else(left_right == renew, "EV, Renew", "EV, EPP")) |>
  select(quantity, draw)
fd_draws <- fd_boot |>
  get_draws() |>
  as_tibble() |>
  mutate(quantity = "First difference") |>
  select(quantity, draw)

qoi_draws <- bind_rows(ev_draws, fd_draws)
qoi_draws

qoi_draws |>
  ggplot(aes(x = draw, fill = quantity)) +
  geom_histogram(binwidth = 0.01, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c(
    "EV, Renew"        = "#2f7f93",
    "EV, EPP"          = "darkorange",
    "First difference" = "grey40"
  )) +
  labs(
    title = "Bootstrap distributions of three quantities",
    x     = "Probability of a yes vote, or difference",
    y     = "Bootstrap samples",
    fill  = NULL
  )

# ============================================================
# 9. EXPECTED VALUES OVER A RANGE ----
# ============================================================
# young and old: 10th and 90th age percentile
age_levels <- nature |>
  summarize(
    young = quantile(age, 0.1),
    old   = quantile(age, 0.9)
  ) |>
  pivot_longer(everything(), names_to = "mep", values_to = "age")
age_levels

age_values_range <- age_levels |>
  pull(age)

range_evs <- predictions(
  logit_model,
  newdata = datagrid(left_right = seq(1, 7, by = 0.25), age = age_values_range)
) |>
  inferences(method = "boot", R = n_boot) |>
  as_tibble() |>
  left_join(age_levels, by = "age") |>
  select(left_right, mep, estimate, conf.low, conf.high)
range_evs

# FD = E(Yes | LeftRight, Age_old) - E(Yes | LeftRight, Age_young)
range_fds <- comparisons(
  logit_model,
  variables = list(age = age_values_range),
  newdata   = datagrid(left_right = seq(1, 7, by = 0.25), age = age_values_range[1])
) |>
  inferences(method = "boot", R = n_boot) |>
  as_tibble() |>
  select(left_right, estimate, conf.low, conf.high)
range_fds

range_evs_plot <- range_evs |>
  ggplot(aes(x = left_right, y = estimate, color = mep, fill = mep)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(young = "#2f7f93", old = "darkorange")) +
  scale_fill_manual(values = c(young = "#2f7f93", old = "darkorange")) +
  scale_x_continuous(breaks = 1:7) +
  labs(
    title = "Expected votes with 95% intervals",
    x     = NULL,
    y     = "Probability of a yes vote",
    color = NULL,
    fill  = NULL
  )

range_fds_plot <- range_fds |>
  ggplot(aes(x = left_right, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, fill = "grey40") +
  geom_line(linewidth = 1, color = "grey40") +
  scale_x_continuous(breaks = 1:7) +
  labs(
    title = "First difference, old minus young",
    x     = "Left-right position",
    y     = "Difference in probability"
  )

# overlapping EV bands don't settle it: the FD's own interval does
range_evs_plot / range_fds_plot +
  plot_layout(heights = c(2, 1))

# ============================================================
# 10. PREDICTED VALUES ----
# ============================================================
# Y^(r) ~ Bernoulli(E(Yes | X)^(r)), one draw per bootstrap sample
boot_pvs <- ev_draws |>
  mutate(predicted = rbinom(n(), size = 1, prob = draw))

boot_pvs |>
  count(quantity, predicted)

# the share of yes draws is again the expected value
boot_pvs |>
  group_by(quantity) |>
  summarize(
    share_yes = mean(predicted),
    expected  = mean(draw)
  )

# ============================================================
# 11. TRY IT: THE COMBUSTION ENGINE VOTE ----
# ============================================================
# In February 2023, Parliament voted on CO2 standards for cars, the
# phase-out of new combustion engines by 2035. ep_votes holds this vote
# as co2_cars.
# Your task: fit the same logit model for this vote and report, with 95%
# bootstrap intervals, the expected probability of a yes vote for Renew
# and the EPP at average age, and the first difference between them. How
# does the first difference compare with the Nature Restoration Law?

# ============================================================
# 12. TRY IT: LOGIT VS. PROBIT ----
# ============================================================
# The logit and the probit differ only in their response function.
# Your task: fit the Nature Restoration model as a probit. Compare its
# coefficients with the logit's, and plot both models' predicted
# probabilities along the left-right scale at average age. Do the two
# models disagree about any MEP?
