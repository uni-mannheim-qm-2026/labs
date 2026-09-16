# ============================================================
# WEEK 2 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse)


# ============================================================
# 1. REPRODUCIBILITY: SET.SEED() ----
# ============================================================
# r-prefixed functions (rnorm(), rbinom(), rgamma(), sample(), ...) draw
# an actual random sample -- different every call. d/p/q-prefixed
# functions (dnorm(), pbinom(), qgamma(), ...) compute a value straight
# from the formula instead -- deterministic, no variance in the output

a <- rnorm(5)
b <- rnorm(5)
identical(a, b)

set.seed(1234)
a <- rnorm(5)
set.seed(1234)
b <- rnorm(5)
identical(a, b)

# in practice, set.seed() is usually called once, at the very top of a
# script, rather than before every draw


# ============================================================
# 2. DISCRETE DISTRIBUTIONS: THE BERNOULLI ----
# ============================================================
# playground: probabilityplayground.com/bernoulli
# one legislator voting on a bill, P(yes) = 0.8

dbinom(x = 1, size = 1, prob = 0.8) # yes vote
dbinom(x = 0, size = 1, prob = 0.8) # no vote

# size = 1 is exactly a Bernoulli trial -- named wrapper, same math
dbern <- function(x, prob) {
  dbinom(x, size = 1, prob = prob)
}

dbern(x = 1, prob = 0.8)
dbern(x = 0, prob = 0.8)

# rbern() wraps rbinom() the same way
rbern <- function(n, prob) {
  rbinom(n, size = 1, prob = prob)
}

rbern(n = 1, prob = 0.5)

votes <- tibble(vote = rbern(n = 1000, prob = 0.5))
votes |> count(vote)


# ============================================================
# 3. THE BINOMIAL DISTRIBUTION ----
# ============================================================
# playground: probabilityplayground.com/binomial
# d/p/q/r naming pattern:
#   d: P(K=k)      -- x=k, size=n, prob=p
#   p: P(K<=k)     -- q=k, size=n, prob=p
#   q: min k s.t. P(K<=k) >= a -- p=a (not prob!), size=n, prob=p
#   r: iid draws   -- n=m (not size!), size=n, prob=p

chamber_votes <- tibble(
  yes_votes = 0:100,
  pmf       = dbinom(yes_votes, size = 100, prob = 0.5),
  cdf       = pbinom(yes_votes, size = 100, prob = 0.5)
)

# a small, hand-checkable case -- two legislators, size = 2
dbinom(x = c(0, 1, 2), size = 2, prob = 0.5) # exactly 0, 1, 2 yes votes
pbinom(q = 1, size = 2, prob = 0.5)          # 0 or 1 yes votes
1 - pbinom(q = 1, size = 2, prob = 0.5)      # more than 1 yes vote
pbinom(q = 1, size = 2, prob = 0.5, lower.tail = FALSE) # same, lower.tail
qbinom(p = 0.75, size = 2, prob = 0.5) # smallest k with P(K<=k) >= .75
rbinom(n = 5, size = 2, prob = 0.5)    # 5 simulated two-legislator outcomes


# ============================================================
# 4. EXAMPLE: WILL THE SUMMIT VENUE OVERFLOW? ----
# ============================================================
# 120 delegates invited, 85% show-up chance each, venue seats 100

1 - pbinom(q = 100, size = 120, prob = 0.85)

summit_cdf <- tibble(
  attendees = 0:120,
  cdf       = pbinom(attendees, size = 120, prob = 0.85)
)

summit_cdf |>
  ggplot(aes(x = attendees, y = cdf)) +
  geom_point(size = 0.8) +
  labs(
    title = "CDF of attendance (n = 120 invited, p = 0.85)",
    x = "Delegates showing up", y = "Probability"
  ) +
  theme_intror()


# ============================================================
# 5. CONTINUOUS DISTRIBUTIONS: THE NORMAL ----
# ============================================================
# playground: probabilityplayground.com/normal
# same d/p/q/r pattern, continuous x instead of discrete k

tibble(x = seq(-4, 4, by = 0.01), density = dnorm(x)) |>
  ggplot(aes(x = x, y = density)) +
  geom_line() +
  labs(title = "PDF of the standard Normal N(0, 1)", x = "x", y = "f(x)") +
  theme_intror()

# q(0.975): which x has 97.5% of the mass below it? worth deriving and
# storing rather than memorizing "1.96"
crit_975 <- qnorm(p = 0.975, mean = 0, sd = 1)
crit_975

pnorm(q = crit_975, mean = 0, sd = 1)     # P(X <= 1.96)
1 - pnorm(q = crit_975, mean = 0, sd = 1) # P(X > 1.96)
qnorm(p = 0.3, mean = 0, sd = 1) # value below which 30% of the mass lies

rnorm(n = 20, mean = 0, sd = 1) # 20 actual random draws

normal_curve <- tibble(
  x   = seq(-5, 5, by = 0.1),
  pdf = dnorm(x),
  cdf = pnorm(x)
)

normal_curve |>
  ggplot(aes(x = x, y = cdf)) +
  geom_line() +
  labs(title = "CDF of N(0, 1)", x = "x", y = "F(x)") +
  theme_intror()

# a different mean and sd shifts and stretches the same curve
normal_curve |>
  mutate(pdf_shifted = dnorm(x, mean = 3, sd = 2)) |>
  ggplot(aes(x = x, y = pdf_shifted)) +
  geom_line() +
  labs(title = "PDF of N(3, 4)", x = "x", y = "f(x)") +
  theme_intror()

# ============================================================
# 6. CENTRAL LIMIT THEOREM ----
# ============================================================
# sampling distribution of the mean approaches N(mu, sigma^2/n) as n
# grows, regardless of the population's own shape
#   Trial 1 -- delegate age (Normal population)
#     1.a: 100 surveys of 150 delegates each
#     1.b: 100 surveys of 400 delegates each
#   Trial 2 -- travel distance to the conference (skewed population)
#     2.a: 100 surveys of 100 delegates each
#     2.b: 100 surveys of 400 delegates each

population_size <- 1000
n_batches <- 100
delegate_ages <- rnorm(population_size, mean = 32, sd = 5)

tibble(age = delegate_ages) |>
  summarize(mean_age = mean(age), var_age = var(age))

tibble(age = delegate_ages) |>
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = course_primary, alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  labs(title = "Population distribution of delegate age", x = "Age", y = "Density") +
  theme_intror()

tibble(age = delegate_ages) |>
  slice_sample(n = 30, replace = TRUE)

# delegate_ages is a plain vector, so the trials below sample it with
# base R's sample() -- slice_sample() draws data-frame rows, sample()
# draws vector elements

sample_size <- 150
trial_1a <- expand_grid(batch = 1:n_batches, participant = 1:sample_size) |>
  group_by(batch) |>
  mutate(age = sample(delegate_ages, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("1.a: n = {sample_size} per survey"))
trial_1a |> head(100)

sample_size <- 400
trial_1b <- expand_grid(batch = 1:n_batches, participant = 1:sample_size) |>
  group_by(batch) |>
  mutate(age = sample(delegate_ages, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("1.b: n = {sample_size} per survey"))
trial_1b |> head(100)

trials_1 <- bind_rows(trial_1a, trial_1b) |>
  group_by(batch, label) |>
  summarize(sample_mean = mean(age), .groups = "drop")

trials_1 |>
  ggplot(aes(x = sample_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = course_primary, alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~label, ncol = 1) +
  labs(
    title = "Sampling distribution of the mean, by survey size",
    x = "Sample mean", y = "Density"
  ) +
  theme_intror()

trials_1 |>
  group_by(label) |>
  summarize(mean_sm = mean(sample_mean), var_sm = var(sample_mean))

tibble(population_variance = var(delegate_ages)) |>
  mutate(theoretical_1a = population_variance / 150, theoretical_1b = population_variance / 400)

# larger samples per survey (400 vs. 150) tighten the sampling
# distribution around the true population mean

travel_distances <- rgamma(population_size, shape = 1.1, scale = 2000)

tibble(distance_km = travel_distances) |>
  ggplot(aes(x = distance_km)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = course_primary, alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  labs(title = "Population distribution of travel distance", x = "Distance traveled (km)", y = "Density") +
  theme_intror()
# clearly right-skewed, nothing like a Normal

sample_size <- 100
trial_2a <- expand_grid(batch = 1:n_batches, participant = 1:sample_size) |>
  group_by(batch) |>
  mutate(distance_km = sample(travel_distances, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("2.a: n = {sample_size} per survey"))
trial_2a |> head(100)

sample_size <- 400
trial_2b <- expand_grid(batch = 1:n_batches, participant = 1:sample_size) |>
  group_by(batch) |>
  mutate(distance_km = sample(travel_distances, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("2.b: n = {sample_size} per survey"))
trial_2b |> head(100)

trials_2 <- bind_rows(trial_2a, trial_2b) |>
  group_by(batch, label) |>
  summarize(sample_mean = mean(distance_km), .groups = "drop")

trials_2 |>
  ggplot(aes(x = sample_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = course_primary, alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~label, ncol = 1) +
  labs(
    title = "Sampling distribution of the mean, skewed population",
    x = "Sample mean", y = "Density"
  ) +
  theme_intror()

trials_2 |>
  group_by(label) |>
  summarize(mean_sm = mean(sample_mean), var_sm = var(sample_mean))

# sampling distribution of the mean approaches Normal regardless of the
# population's own shape -- that's the whole point of the theorem

# sampling several variables together (not independently) preserves
# whatever relationship holds between them within a batch
delegates <- tibble(
  age         = rnorm(population_size, mean = 32, sd = 5),
  distance_km = rgamma(population_size, shape = 1.1, scale = 2000)
)

sample_size <- 100
trial_combined <- expand_grid(batch = 1:n_batches, participant = 1:sample_size) |>
  group_by(batch) |>
  mutate(sample = slice_sample(delegates, n = sample_size, replace = TRUE)) |>
  ungroup() |>
  unnest(sample) |>
  mutate(label = str_glue("Joint age + distance, n = {sample_size} per survey"))
trial_combined |> head(100)


# ============================================================
# 7. NORMAL APPROXIMATION OF THE BINOMIAL ----
# ============================================================
# a Binomial count is a sum of n Bernoulli trials -- by the CLT, that
# sum looks increasingly Normal as n grows, so its probabilities can be
# approximated with a matched Normal instead. Back to the summit:

n_invited   <- 120
p_attend    <- 0.85
mean_attend <- n_invited * p_attend
sd_attend   <- sqrt(n_invited * p_attend * (1 - p_attend))

1 - pnorm(q = 100, mean = mean_attend, sd = sd_attend)
1 - pbinom(q = 100, size = n_invited, prob = p_attend) # close to the normal approx above!

tibble(
  attendees = seq(0, 120, by = 0.01),
  density   = dnorm(attendees, mean = mean_attend, sd = sd_attend)
) |>
  ggplot(aes(x = attendees, y = density)) +
  geom_line() +
  labs(
    title = str_glue("PDF of N({mean_attend}, {round(sd_attend^2, 1)})"),
    x = "Delegates showing up", y = "Density"
  ) +
  theme_intror()


# ============================================================
# 8. VARIABLE STANDARDIZATION ----
# ============================================================
# comparing two conferences' attendance only makes sense relative to
# each conference series' own typical turnout. z_i = (x_i - mean) / sd

set.seed(1234)
series_a <- rnorm(1000, mean = 60, sd = 20)
series_b <- rnorm(1000, mean = 120, sd = 40)

mean_a <- mean(series_a)
sd_a   <- sd(series_a)
conf_a_attendance <- 90

z_a <- (conf_a_attendance - mean_a) / sd_a
z_a

mean_b <- mean(series_b)
sd_b   <- sd(series_b)
conf_b_attendance <- 160

z_b <- (conf_b_attendance - mean_b) / sd_b
z_b
# A had the stronger year relative to its own history, despite drawing
# fewer delegates in absolute terms

conf_a_curve <- tibble(
  x       = seq(mean_a - 3 * sd_a, mean_a + 3 * sd_a, length.out = 200),
  density = dnorm(x, mean = mean_a, sd = sd_a),
  conference = "A", attendance = conf_a_attendance
)

conf_b_curve <- tibble(
  x       = seq(mean_b - 3 * sd_b, mean_b + 3 * sd_b, length.out = 200),
  density = dnorm(x, mean = mean_b, sd = sd_b),
  conference = "B", attendance = conf_b_attendance
)

standardize_curves <- bind_rows(conf_a_curve, conf_b_curve)
standardize_curves

standardize_curves |>
  ggplot(aes(x = x, y = density)) +
  geom_line() +
  geom_vline(aes(xintercept = attendance), color = course_primary, linetype = "dashed") +
  facet_wrap(~conference, scales = "free", labeller = label_both) +
  labs(
    title = "Each conference vs. its own series",
    x = "Attendance (delegates)", y = "Density"
  ) +
  theme_intror()


# ============================================================
# 9. TRY IT: A TIGHTER INVITATION CAP ----
# ============================================================
# Your task: what's the probability of overflowing a 100-seat venue if
# the summit chair caps invitations at 110 instead of 120 (same 85%
# attendance rate)?


# ============================================================
# 10. TRY IT: STANDARDIZING A SMALL CONFERENCES TIBBLE ----
# ============================================================
# Your task: two more conferences, two more series -- using a tibble and
# mutate(), compute z-scores to find which one had the stronger year
# relative to its own history: Conference C (45 delegates, series
# average 30, sd 10) or Conference D (210 delegates, series average
# 150, sd 50)?


# ============================================================
# 11. TRY IT: CLT AT A LARGER SAMPLE SIZE ----
# ============================================================
# Your task: repeat trial 1.b (the Normal delegate-age population,
# n = 400 per survey) at n = 900 per survey. Does the sampling
# distribution get tighter still, and does var(trial) land close to
# the theoretical var(pop)/900?
