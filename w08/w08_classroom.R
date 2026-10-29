# ============================================================
# WEEK 8 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, broom, marginaleffects, infer, ggdag, dagitty, patchwork)
set.seed(808)

# ============================================================
# 1. QUANTITIES OF INTEREST: WEEK 7'S AGE MODEL ----
# ============================================================
# GERDA 2021 without Berlin, plus the INKAR median age of the same year
gerda <- read_csv("data/federal_cty_harm.csv")
covariates <- read_csv("data/cty_covariates.csv")

covariates_2021 <- covariates |>
  filter(year == 2021) |>
  select(county_code, median_age)

counties <- gerda |>
  filter(
    election_year == 2021,
    state != "11"
  ) |>
  select(county_code, state, afd, turnout) |>
  mutate(across(c(afd, turnout), ~ .x * 100)) |>
  left_join(covariates_2021, by = "county_code")

# AfD_i = beta_0 + beta_1 * Turnout_i + beta_2 * Age_i + beta_3 * Turnout_i * Age_i + epsilon_i
age_model <- lm(afd ~ turnout * median_age, data = counties)

age_beta_1 <- age_model |>
  tidy() |>
  filter(term == "turnout") |>
  pull(estimate)
age_beta_3 <- age_model |>
  tidy() |>
  filter(term == "turnout:median_age") |>
  pull(estimate)

age_levels <- counties |>
  summarize(
    p10 = quantile(median_age, 0.1),
    p90 = quantile(median_age, 0.9)
  ) |>
  pivot_longer(
    everything(),
    names_to        = "percentile",
    names_prefix    = "p",
    names_transform = as.numeric,
    values_to       = "median_age"
  )

turnout_min <- counties |>
  summarize(min(turnout)) |>
  pull()
turnout_max <- counties |>
  summarize(max(turnout)) |>
  pull()
turnout_grid <- seq(turnout_min, turnout_max, length.out = 50)

# ============================================================
# 2. BOOTSTRAPPING THE COEFFICIENTS ----
# ============================================================
# step 1: bootstrap samples, 399 counties drawn with replacement
n_boot <- 1000

boot_samples <- expand_grid(
  sample_id = 1:n_boot,
  counties
) |>
  group_by(sample_id) |>
  slice_sample(prop = 1, replace = TRUE) |>
  ungroup()
boot_samples

# step 2: fit the model on each sample
boot_coefs <- tibble(replicate = 1:n_boot) |>
  rowwise() |>
  mutate(
    boot_samples |>
      filter(sample_id == replicate) |>
      lm(afd ~ turnout * median_age, data = _) |>
      tidy() |>
      select(term, estimate) |>
      pivot_wider(names_from = term, values_from = estimate)
  ) |>
  ungroup() |>
  rename(intercept = `(Intercept)`) |>
  pivot_longer(-replicate, names_to = "term", values_to = "estimate")
boot_coefs

# the same with infer
boot_coefs_infer <- counties |>
  specify(afd ~ turnout * median_age) |>
  generate(reps = n_boot, type = "bootstrap") |>
  fit()
boot_coefs_infer

# the same with marginaleffects
boot_coefs_marginaleffects <- hypotheses(age_model) |>
  inferences(method = "boot", R = n_boot) |>
  get_draws() |>
  as_tibble() |>
  select(replicate = drawid, term, estimate = draw) |>
  mutate(term = if_else(term == "(Intercept)", "intercept", term))
boot_coefs_marginaleffects

# SE_boot(beta_j) = sd of the bootstrap coefficients
boot_se <- boot_coefs |>
  group_by(term) |>
  summarize(se_boot = sd(estimate))

infer_se <- boot_coefs_infer |>
  group_by(term) |>
  summarize(se_infer = sd(estimate))

marginaleffects_se <- boot_coefs_marginaleffects |>
  group_by(term) |>
  summarize(se_marginaleffects = sd(estimate))

age_se <- age_model |>
  tidy() |>
  select(term, se_ols = std.error) |>
  mutate(term = if_else(term == "(Intercept)", "intercept", term))

age_se |>
  left_join(boot_se, by = "term") |>
  left_join(infer_se, by = "term") |>
  left_join(marginaleffects_se, by = "term")

boot_betas <- boot_coefs |>
  pivot_wider(names_from = term, values_from = estimate) |>
  rename(
    beta_0 = intercept,
    beta_1 = turnout,
    beta_2 = median_age,
    beta_3 = `turnout:median_age`
  )
boot_betas

lm_estimate <- tibble(
  beta_1 = age_beta_1,
  beta_3 = age_beta_3
)

boot_betas_plot <- boot_betas |>
  ggplot(aes(x = beta_1, y = beta_3)) +
  geom_point(alpha = 0.3, color = "#2f7f93") +
  geom_point(data = lm_estimate, aes(shape = "Estimate from lm()"), size = 4) +
  scale_shape_manual(values = c("Estimate from lm()" = 4)) +
  labs(
    title = "Bootstrap samples of two coefficients",
    x     = "Turnout coefficient",
    y     = "Interaction coefficient",
    shape = NULL
  )
boot_betas_plot

# step 3: the scenario, mean turnout in a young and an old county
turnout_mean <- counties |>
  summarize(mean(turnout)) |>
  pull()
age_young <- age_levels |>
  filter(percentile == 10) |>
  pull(median_age)
age_old <- age_levels |>
  filter(percentile == 90) |>
  pull(median_age)

# ============================================================
# 3. EXPECTED VALUES AND FIRST DIFFERENCES ----
# ============================================================
# step 4: E(AfD | Turnout, Age)^(r) per bootstrap sample, FD = EV_old - EV_young
boot_qoi <- boot_betas |>
  mutate(
    ev_young = beta_0 + beta_1 * turnout_mean + beta_2 * age_young + beta_3 * turnout_mean * age_young,
    ev_old   = beta_0 + beta_1 * turnout_mean + beta_2 * age_old + beta_3 * turnout_mean * age_old,
    fd       = ev_old - ev_young
  )
boot_qoi

# step 5: mean and 2.5%/97.5% quantiles
boot_qoi_long <- boot_qoi |>
  pivot_longer(c(ev_young, ev_old, fd), names_to = "quantity", values_to = "value") |>
  mutate(quantity = fct_inorder(quantity))
boot_qoi_long

qoi_summary <- boot_qoi_long |>
  group_by(quantity) |>
  summarize(
    estimate = mean(value),
    lower    = quantile(value, 0.025),
    upper    = quantile(value, 0.975)
  )
qoi_summary

qoi_colors <- c(
  ev_young = "#2f7f93",
  ev_old   = "darkorange",
  fd       = "grey40"
)
qoi_labels <- c(
  ev_young = "EV, young county",
  ev_old   = "EV, old county",
  fd       = expression(atop("First difference", E(AfD ~ "|" ~ Age[old]) - E(AfD ~ "|" ~ Age[young])))
)

qoi_plot <- boot_qoi_long |>
  ggplot(aes(x = value, fill = quantity)) +
  geom_histogram(binwidth = 0.05, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = qoi_colors, labels = qoi_labels) +
  labs(
    title = "Bootstrap distributions of three quantities",
    x     = "AfD vote share (%) or difference (points)",
    y     = "Bootstrap samples",
    fill  = NULL
  )
qoi_plot

# ============================================================
# 4. COMPARISON WITH MARGINALEFFECTS ----
# ============================================================
evs_delta <- predictions(
  age_model,
  newdata = datagrid(turnout = turnout_mean, median_age = c(age_young, age_old)),
  df      = "residual"
)
evs_boot <- evs_delta |>
  inferences(method = "boot", R = n_boot)
evs_simulation <- evs_delta |>
  inferences(method = "simulation", R = n_boot)
evs_boot
evs_simulation

fds_delta <- comparisons(
  age_model,
  variables = list(median_age = c(age_young, age_old)),
  newdata   = datagrid(turnout = turnout_mean),
  df        = "residual"
)
fds_boot <- fds_delta |>
  inferences(method = "boot", R = n_boot)
fds_simulation <- fds_delta |>
  inferences(method = "simulation", R = n_boot)
fds_boot
fds_simulation

# ============================================================
# 5. EXPECTED VALUES OVER A RANGE ----
# ============================================================
range_scenarios <- expand_grid(
  turnout    = turnout_grid,
  median_age = c(age_young, age_old)
)

range_evs <- expand_grid(boot_betas, range_scenarios) |>
  mutate(ev = beta_0 + beta_1 * turnout + beta_2 * median_age + beta_3 * turnout * median_age) |>
  group_by(turnout, median_age) |>
  summarize(
    estimate = mean(ev),
    lower    = quantile(ev, 0.025),
    upper    = quantile(ev, 0.975),
    .groups  = "drop"
  ) |>
  mutate(scenario = if_else(median_age == age_young, "ev_young", "ev_old"))
range_evs

range_labels <- c(
  ev_young = str_glue("Young county: {round(age_young, 1)} years"),
  ev_old   = str_glue("Old county: {round(age_old, 1)} years")
)

range_evs_plot <- range_evs |>
  ggplot(aes(x = turnout, color = scenario, fill = scenario)) +
  geom_point(data = counties, aes(y = afd), color = "grey60", fill = NA, alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
  geom_line(aes(y = estimate), linewidth = 1) +
  scale_color_manual(values = qoi_colors, labels = range_labels, aesthetics = c("color", "fill")) +
  labs(
    title = "Expected AfD support with 95% intervals",
    x     = "Turnout (%)",
    y     = "AfD vote share (%)",
    color = NULL,
    fill  = NULL
  )
range_evs_plot

# the same with marginaleffects
predictions(
  age_model,
  newdata = datagrid(turnout = turnout_grid, median_age = c(age_young, age_old))
) |>
  inferences(method = "boot", R = n_boot)

# ============================================================
# 6. PREDICTED VALUES ----
# ============================================================
sigma_hat <- age_model |>
  glance() |>
  pull(sigma)
sigma_hat

# PV^(r) = EV^(r) + epsilon^(r), epsilon ~ N(0, sigma_hat^2)
boot_pvs <- boot_qoi |>
  mutate(
    pv_young = ev_young + rnorm(n(), 0, sigma_hat),
    pv_old   = ev_old + rnorm(n(), 0, sigma_hat)
  )

pv_summary <- boot_pvs |>
  pivot_longer(c(pv_young, pv_old), names_to = "quantity", values_to = "value") |>
  group_by(quantity) |>
  summarize(
    estimate = mean(value),
    lower    = quantile(value, 0.025),
    upper    = quantile(value, 0.975)
  )
pv_summary

# analytical prediction interval
scenarios <- tibble(
  scenario   = c("young", "old"),
  turnout    = turnout_mean,
  median_age = c(age_young, age_old)
)

age_model |>
  augment(newdata = scenarios, interval = "prediction") |>
  select(scenario, .fitted, .lower, .upper)

ev_pv_draws <- boot_pvs |>
  pivot_longer(c(ev_young, ev_old, pv_young, pv_old), names_to = c("type", "scenario"), names_sep = "_", values_to = "value") |>
  mutate(
    type = if_else(
      type == "ev",
      "Expected value: estimation uncertainty",
      "Predicted value: estimation + fundamental uncertainty"
    ),
    scenario = str_glue("{scenario} county")
  )

ev_pv_draws |>
  ggplot(aes(x = value, fill = scenario)) +
  geom_histogram(binwidth = 0.25, alpha = 0.6, position = "identity") +
  facet_wrap(~type, ncol = 1) +
  scale_fill_manual(values = c("young county" = "#2f7f93", "old county" = "darkorange")) +
  labs(
    title = "Expected and predicted AfD shares",
    x     = "AfD vote share (%)",
    y     = "Bootstrap samples",
    fill  = NULL
  )

range_pvs <- expand_grid(boot_betas, range_scenarios) |>
  mutate(
    ev = beta_0 + beta_1 * turnout + beta_2 * median_age + beta_3 * turnout * median_age,
    pv = ev + rnorm(n(), 0, sigma_hat)
  ) |>
  group_by(turnout, median_age) |>
  summarize(
    lower   = quantile(pv, 0.025),
    upper   = quantile(pv, 0.975),
    .groups = "drop"
  ) |>
  mutate(scenario = if_else(median_age == age_young, "ev_young", "ev_old"))

range_evs_plot +
  geom_ribbon(data = range_pvs, aes(ymin = lower, ymax = upper), alpha = 0.08, linetype = "dashed", linewidth = 0.4) +
  labs(
    title    = "Expected and predicted AfD support with 95% intervals",
    subtitle = "Narrow bands: expected values. Wide dashed bands: predicted values."
  )

# ============================================================
# 7. MONTE CARLO: THE POPULATION ----
# ============================================================
# Knowledge_i = beta_0 + beta_1 * Educ_i + beta_2 * Interest_i + epsilon_i
source("include/plot_dag.R")

knowledge_labels <- tribble(
  ~name,       ~label,
  "education", "Education",
  "interest",  "Political interest",
  "knowledge", "Political knowledge"
)

dagitty('dag {
  education [exposure, pos="0,0"]
  knowledge [outcome, pos="2,0"]
  interest [pos="1,-1"]
  education -> knowledge
  interest -> education
  interest -> knowledge
}') |>
  plot_dag(labels = knowledge_labels)

# step 1: the true values
n_population   <- 100000
# interest
interest_mean  <- 5
interest_sd    <- 1.8
interest_min   <- 0
interest_max   <- 10
# education
delta_0        <- 9
delta_interest <- 0.6
sigma_nu       <- 2
education_min  <- 8
# knowledge
beta_0         <- 5
beta_education <- 2.5
beta_interest  <- 3
sigma_epsilon  <- 8

# Interest_i  ~ N(5, 1.8^2)
# Educ_i      = delta_0 + delta_1 * Interest_i + nu_i,                    nu_i      ~ N(0, sigma_nu^2)
# Knowledge_i = beta_0 + beta_1 * Educ_i + beta_2 * Interest_i + epsilon_i, epsilon_i ~ N(0, sigma_epsilon^2)
# truncation: keep 0 <= Interest_i <= 10 and Educ_i >= 8
population <- tibble(interest = rnorm(n_population, interest_mean, interest_sd)) |>
  mutate(
    education = delta_0 + delta_interest * interest + rnorm(n(), 0, sigma_nu),
    knowledge = beta_0 + beta_education * education + beta_interest * interest + rnorm(n(), 0, sigma_epsilon)
  ) |>
  filter(
    between(interest, interest_min, interest_max),
    education >= education_min
  ) |>
  mutate(voter_id = row_number(), .before = 1)
population

population |>
  slice_sample(n = 2000) |>
  ggplot(aes(x = education, y = knowledge, color = interest)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Education, interest and political knowledge",
    x     = "Education (years)",
    y     = "Political knowledge (score)",
    color = "Political interest (0–10)"
  )

truth <- tibble(
  term       = c("education", "interest"),
  true_value = c(beta_education, beta_interest)
)
truth

# ============================================================
# 8. NO VIOLATION: THE SAMPLING DISTRIBUTION ----
# ============================================================
# step 2: 1,000 surveys of 500 voters
n_sample <- 500
n_sims   <- 1000

mc_draws <- population |>
  slice_sample(n = n_sample * n_sims, replace = TRUE) |>
  mutate(sample_id = rep(1:n_sims, each = n_sample)) |>
  select(sample_id, voter_id)

mc_samples <- mc_draws |>
  left_join(population, by = "voter_id")
mc_samples

# steps 3 and 4: estimate on every sample
simulate_round <- function(samples, formula, round_name) {
  samples |>
    distinct(sample = sample_id) |>
    rowwise() |>
    mutate(
      samples |>
        filter(sample_id == sample) |>
        lm(formula, data = _) |>
        tidy() |>
        select(term, estimate, std.error) |>
        pivot_wider(names_from = term, values_from = c(estimate, std.error))
    ) |>
    ungroup() |>
    pivot_longer(
      -sample,
      names_to      = c(".value", "term"),
      names_pattern = "(estimate|std.error)_(.*)"
    ) |>
    mutate(
      term  = str_remove(term, "_[a-z]+$"),
      round = round_name
    ) |>
    rename(sample_id = sample)
}

baseline <- mc_samples |>
  simulate_round(knowledge ~ education + interest, "No violation")
baseline

# step 5: bias = mean(beta_hat) - beta, sd(beta_hat) vs. mean(SE)
summarize_round <- function(results, truth) {
  results |>
    inner_join(truth, by = "term") |>
    group_by(round, term, true_value) |>
    summarize(
      mean_estimate = mean(estimate),
      sd_estimate   = sd(estimate),
      mean_se       = mean(std.error),
      .groups       = "drop"
    ) |>
    mutate(bias = mean_estimate - true_value, .after = mean_estimate)
}

baseline_summary <- baseline |>
  summarize_round(truth)
baseline_summary

plot_rounds <- function(results, truth, title) {
  results |>
    filter(term == "education") |>
    inner_join(truth, by = "term") |>
    mutate(round = fct_inorder(round)) |>
    ggplot(aes(x = estimate, fill = round)) +
    geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
    geom_vline(aes(xintercept = true_value, linetype = "True value")) +
    scale_fill_manual(values = c("#2f7f93", "grey60")) +
    scale_linetype_manual(values = c("True value" = "dashed")) +
    labs(
      title    = title,
      x        = "Estimate",
      y        = "Samples",
      fill     = NULL,
      linetype = NULL
    )
}

# ============================================================
# 9. OMITTED VARIABLE ----
# ============================================================
# E(beta_1_hat*) = beta_1 + beta_2 * delta, Interest_i = delta_0 + delta * Educ_i + u_i
ovb_delta <- population |>
  lm(interest ~ education, data = _) |>
  tidy() |>
  filter(term == "education") |>
  pull(estimate)
ovb_expected <- beta_education + beta_interest * ovb_delta
ovb_expected

omitted <- mc_samples |>
  simulate_round(knowledge ~ education, "Interest omitted")

omitted_summary <- omitted |>
  summarize_round(truth)
omitted_summary

omitted |>
  bind_rows(baseline) |>
  plot_rounds(truth, "Education coefficient with and without interest")

# ============================================================
# 10. MEASUREMENT ERROR ----
# ============================================================
# Interest*_i = Interest_i + u_i, u_i ~ N(0, sigma_u^2), rounded and capped
sigma_report <- 1.5

population <- population |>
  mutate(
    interest_reported = round(interest + rnorm(n(), 0, sigma_report)),
    interest_reported = pmin(pmax(interest_reported, interest_min), interest_max)
  )

mc_samples <- mc_draws |>
  left_join(population, by = "voter_id")

population |>
  slice_sample(n = 2000) |>
  ggplot(aes(x = interest, y = interest_reported)) +
  geom_jitter(height = 0.2, alpha = 0.3, color = "#2f7f93") +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Reported = true")) +
  scale_linetype_manual(values = c("Reported = true" = "dashed")) +
  labs(
    title    = "True and reported political interest",
    x        = "True interest",
    y        = "Reported interest",
    linetype = NULL
  )

# lambda = Var(Interest) / Var(Interest*), E(beta_2_hat) ~ beta_2 * (lambda - R^2) / (1 - R^2)
reliability <- population |>
  summarize(var(interest) / var(interest_reported)) |>
  pull()
reported_r2 <- population |>
  lm(interest_reported ~ education, data = _) |>
  glance() |>
  pull(r.squared)
attenuation_expected <- beta_interest * (reliability - reported_r2) / (1 - reported_r2)
attenuation_expected

measurement <- mc_samples |>
  simulate_round(knowledge ~ education + interest_reported, "Interest self-reported")

measurement_summary <- measurement |>
  summarize_round(truth)
measurement_summary

measurement |>
  bind_rows(baseline) |>
  plot_rounds(truth, "Education coefficient, true vs. reported interest")

# ============================================================
# 11. HETEROSKEDASTICITY ----
# ============================================================
# epsilon_i ~ N(0, sigma_i^2), sigma_i = kappa * (Educ_i - shift)^2
hetero_kappa <- 0.12
hetero_shift <- 7

population <- population |>
  mutate(
    knowledge_hetero = beta_0 + beta_education * education + beta_interest * interest +
      rnorm(n(), 0, hetero_kappa * (education - hetero_shift)^2)
  )

mc_samples <- mc_draws |>
  left_join(population, by = "voter_id")

population |>
  slice_sample(n = 2000) |>
  ggplot(aes(x = education, y = knowledge_hetero)) +
  geom_point(alpha = 0.4, color = "#2f7f93") +
  labs(
    title = "Knowledge scatters more among the highly educated",
    x     = "Education (years)",
    y     = "Political knowledge (score)"
  )

hetero <- mc_samples |>
  simulate_round(knowledge_hetero ~ education + interest, "Heteroskedastic errors")

hetero_summary <- hetero |>
  summarize_round(truth)
hetero_summary

hetero |>
  bind_rows(baseline) |>
  plot_rounds(truth, "Education coefficient with heteroskedastic errors")

# the bootstrap as a fix, for the first 200 surveys
n_sims_boot  <- 200
n_boot_inner <- 200

hetero_boot_se <- mc_samples |>
  distinct(sample = sample_id) |>
  filter(sample <= n_sims_boot) |>
  rowwise() |>
  mutate(
    mc_samples |>
      filter(sample_id == sample) |>
      specify(knowledge_hetero ~ education + interest) |>
      generate(reps = n_boot_inner, type = "bootstrap") |>
      fit() |>
      ungroup() |>
      filter(term == "education") |>
      summarize(se_boot = sd(estimate))
  ) |>
  ungroup() |>
  rename(sample_id = sample)
hetero_boot_se

hetero_se_compare <- hetero |>
  filter(sample_id <= n_sims_boot, term == "education") |>
  left_join(hetero_boot_se, by = "sample_id") |>
  summarize(
    sd_estimate  = sd(estimate),
    mean_se_ols  = mean(std.error),
    mean_se_boot = mean(se_boot)
  )
hetero_se_compare

# robust (HC3) standard errors for one survey
mc_samples |>
  filter(sample_id == 1) |>
  lm(knowledge_hetero ~ education + interest, data = _) |>
  hypotheses(vcov = "HC3")

# ============================================================
# 12. MULTICOLLINEARITY ----
# ============================================================
# second society: Educ_col_i = delta_0_col + delta_col * Interest_i + nu_col_i, Educ_col_i >= 8
delta_0_collinear  <- 6
delta_collinear    <- 1.2
sigma_nu_collinear <- 0.6

population_collinear <- population |>
  mutate(
    education_collinear = delta_0_collinear + delta_collinear * interest + rnorm(n(), 0, sigma_nu_collinear),
    knowledge_collinear = beta_0 + beta_education * education_collinear + beta_interest * interest +
      rnorm(n(), 0, sigma_epsilon)
  ) |>
  filter(education_collinear >= education_min)

# its own surveys, drawn as in step 2
mc_samples_collinear <- population_collinear |>
  slice_sample(n = n_sample * n_sims, replace = TRUE) |>
  mutate(sample_id = rep(1:n_sims, each = n_sample))

original_pairs <- population |>
  slice_sample(n = 2000) |>
  select(interest, years = education) |>
  mutate(society = "Original population")
collinear_pairs <- population_collinear |>
  slice_sample(n = 2000) |>
  select(interest, years = education_collinear) |>
  mutate(society = "Collinear society")
predictor_pairs <- bind_rows(original_pairs, collinear_pairs)

predictor_r2 <- predictor_pairs |>
  group_by(society) |>
  summarize(r2 = cor(years, interest)^2) |>
  mutate(label = str_glue("R² = {format(round(r2, 2), nsmall = 2)}"))
predictor_r2

predictor_pairs |>
  ggplot(aes(x = interest, y = years)) +
  geom_point(alpha = 0.3, color = "#2f7f93") +
  geom_smooth(aes(linetype = "OLS line"), method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  geom_label(data = predictor_r2, aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.3) +
  facet_wrap(~ fct_inorder(society)) +
  scale_linetype_manual(values = c("OLS line" = "solid")) +
  labs(
    title    = "Education and interest in the two societies",
    x        = "Political interest",
    y        = "Education (years)",
    linetype = NULL
  )

# VIF_1 = 1 / (1 - R_1^2)
vif_values <- predictor_r2 |>
  mutate(vif = 1 / (1 - r2))
vif_values

# SE ratio ~ sqrt(VIF_col / VIF_original)
vif_original <- vif_values |>
  filter(society == "Original population") |>
  pull(vif)
vif_collinear <- vif_values |>
  filter(society == "Collinear society") |>
  pull(vif)
se_ratio_expected <- sqrt(vif_collinear / vif_original)
se_ratio_expected

collinear <- mc_samples_collinear |>
  simulate_round(knowledge_collinear ~ education_collinear + interest, "Collinear predictors")

collinear_summary <- collinear |>
  summarize_round(truth)
collinear_summary

collinear |>
  bind_rows(baseline) |>
  plot_rounds(truth, "Education coefficient, weak vs. strong collinearity")

# ============================================================
# 13. ALL ROUNDS COMPARED ----
# ============================================================
all_rounds <- bind_rows(baseline, omitted, measurement, hetero, collinear) |>
  mutate(round = fct_inorder(round))

all_rounds_summary <- all_rounds |>
  summarize_round(truth) |>
  filter(term == "education") |>
  mutate(se_ratio = mean_se / sd_estimate)
all_rounds_summary

plot_all_rounds <- function(results, truth) {
  round_intervals <- results |>
    filter(term == "education") |>
    group_by(round = fct_inorder(round)) |>
    summarize(
      mean_estimate = mean(estimate),
      lower         = quantile(estimate, 0.025),
      upper         = quantile(estimate, 0.975)
    )
  benchmark_interval <- round_intervals |>
    filter(round == "No violation")
  true_education <- truth |>
    filter(term == "education")

  round_intervals |>
    ggplot(aes(x = mean_estimate, y = fct_rev(round))) +
    geom_rect(
      data        = benchmark_interval,
      aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, fill = "Middle 95%, no violation"),
      inherit.aes = FALSE,
      alpha       = 0.2
    ) +
    geom_vline(data = true_education, aes(xintercept = true_value, linetype = "True value")) +
    geom_pointrange(aes(xmin = lower, xmax = upper), color = "#2f7f93") +
    scale_fill_manual(values = c("Middle 95%, no violation" = "#2f7f93")) +
    scale_linetype_manual(values = c("True value" = "dashed")) +
    labs(
      title    = "Education coefficient across all rounds",
      subtitle = "Mean and middle 95% of 1,000 estimates",
      x        = "Estimate",
      y        = NULL,
      fill     = NULL,
      linetype = NULL
    )
}

all_rounds |>
  plot_all_rounds(truth)

# ============================================================
# 14. TRY IT: SURVEY SIZE ----
# ============================================================
# Your task: report the standard deviation of the education estimates
# and the mean reported standard error with 2,000 respondents per survey,
# and how much smaller both are than with 500.

# ============================================================
# 15. TRY IT: STRONGER MEASUREMENT ERROR ----
# ============================================================
# Suppose respondents report their interest with sigma_u = 3 instead of 1.5.
# Your task: report the mean estimates of both coefficients with this
# noisier self-report, and compare their bias with the measurement-error
# round above.

# ============================================================
# 16. TRY IT: CURVED EDUCATION EFFECT ----
# ============================================================
# Assumption 1, linearity, with gamma = -0.2:
# Knowledge_curved_i = beta_0 + beta_1 * Educ_i + gamma * (Educ_i - 10)^2 + beta_2 * Interest_i + epsilon_i
# The effect of one more year, beta_1 + 2 * gamma * (Educ_i - 10), equals beta_1 at 10 years.
# A squared term in a formula: I((education - 10)^2)
# Your task: estimate the effect of education at 10 years, once with
# the linear model and once with the squared term added. Report both mean
# estimates, and add both rounds to the plot of all rounds.

# ============================================================
# 17. TRY IT: CLUSTERED SURVEYS ----
# ============================================================
# Assumption 4, independence. 2,000 municipalities, each with a local shift:
# Educ_clust_i      = Educ_i + a_m, a_m ~ N(0, 1.5^2)
# Knowledge_clust_i = beta_0 + beta_1 * Educ_clust_i + beta_2 * Interest_i + b_m + epsilon_i, b_m ~ N(0, 6^2)
# Each survey: 25 municipalities, 20 voters in each.
# Your task: simulate 1,000 such surveys. Report the mean education
# estimate, the standard deviation of the estimates and the mean reported
# standard error, and add the round to the plot of all rounds.

# ============================================================
# 18. TRY IT: SKEWED ERRORS ----
# ============================================================
# Assumption 6, normality:
# epsilon_i = sigma_epsilon * (g_i - k) / sqrt(k), g_i ~ Gamma(k, 1), k = 1
# Your task: report the mean education estimate, the standard
# deviation of the estimates and the mean reported standard error with
# skewed errors, and add the round to the plot of all rounds.
