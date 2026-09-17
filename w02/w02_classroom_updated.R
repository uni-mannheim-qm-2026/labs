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
    x     = "Delegates showing up",
    y     = "Probability"
  )


# ============================================================
# 5. CONTINUOUS DISTRIBUTIONS: THE NORMAL ----
# ============================================================
# playground: probabilityplayground.com/normal
# same d/p/q/r pattern, continuous x instead of discrete k

tibble(
  x       = seq(-4, 4, by = 0.01),
  density = dnorm(x)
) |>
  ggplot(aes(x = x, y = density)) +
  geom_line() +
  labs(
    title = "PDF of the standard Normal N(0, 1)",
    x     = "x",
    y     = "f(x)"
  )

# q(0.975): which x has 97.5% of the mass below it? worth deriving and
# storing rather than memorizing "1.96"
crit_975 <- qnorm(p = 0.975, mean = 0, sd = 1)
crit_975

pnorm(q = crit_975, mean = 0, sd = 1)     # P(X <= 1.96)
1 - pnorm(q = crit_975, mean = 0, sd = 1) # P(X > 1.96)
qnorm(p = 0.3, mean = 0, sd = 1) # value below which 30% of the mass lies

rnorm(n = 20, mean = 0, sd = 1) # 20 actual random draws

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
  summarize(
    mean_age = mean(age),
    var_age  = var(age)
  )

tibble(age = delegate_ages) |>
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  labs(
    title = "Population distribution of delegate age",
    x     = "Age",
    y     = "Density"
  )

tibble(age = delegate_ages) |>
  slice_sample(n = 30, replace = TRUE)

# delegate_ages is a plain vector, so the trials below sample it with
# base R's sample() -- slice_sample() draws data-frame rows, sample()
# draws vector elements

sample_size <- 150
trial_1a <- expand_grid(
  batch       = 1:n_batches,
  participant = 1:sample_size
) |>
  group_by(batch) |>
  mutate(age = sample(delegate_ages, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("1.a: n = {sample_size} per survey"))
trial_1a |> head(100)

sample_size <- 400
trial_1b <- expand_grid(
  batch       = 1:n_batches,
  participant = 1:sample_size
) |>
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
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~label, ncol = 1) +
  labs(
    title = "Sampling distribution of the mean, by survey size",
    x     = "Sample mean",
    y     = "Density"
  )

trials_1 |>
  group_by(label) |>
  summarize(
    mean_sm = mean(sample_mean),
    var_sm  = var(sample_mean)
  )

tibble(population_variance = var(delegate_ages)) |>
  mutate(
    theoretical_1a = population_variance / 150,
    theoretical_1b = population_variance / 400
  )

# larger samples per survey (400 vs. 150) tighten the sampling
# distribution around the true population mean

travel_distances <- rgamma(population_size, shape = 1.1, scale = 2000)

tibble(distance_km = travel_distances) |>
  ggplot(aes(x = distance_km)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  labs(
    title = "Population distribution of travel distance",
    x     = "Distance traveled (km)",
    y     = "Density"
  )
# clearly right-skewed, nothing like a Normal

sample_size <- 100
trial_2a <- expand_grid(
  batch       = 1:n_batches,
  participant = 1:sample_size
) |>
  group_by(batch) |>
  mutate(distance_km = sample(travel_distances, size = sample_size, replace = TRUE)) |>
  ungroup() |>
  mutate(label = str_glue("2.a: n = {sample_size} per survey"))
trial_2a |> head(100)

sample_size <- 400
trial_2b <- expand_grid(
  batch       = 1:n_batches,
  participant = 1:sample_size
) |>
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
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#2f7f93", alpha = 0.4, color = NA) +
  geom_density(linewidth = 0.8) +
  facet_wrap(~label, ncol = 1) +
  labs(
    title = "Sampling distribution of the mean, skewed population",
    x     = "Sample mean",
    y     = "Density"
  )

trials_2 |>
  group_by(label) |>
  summarize(
    mean_sm = mean(sample_mean),
    var_sm  = var(sample_mean)
  )

# sampling distribution of the mean approaches Normal regardless of the
# population's own shape -- that's the whole point of the theorem

# sampling several variables together (not independently) preserves
# whatever relationship holds between them within a batch
delegates <- tibble(
  age         = rnorm(population_size, mean = 32, sd = 5),
  distance_km = rgamma(population_size, shape = 1.1, scale = 2000)
)

sample_size <- 100
trial_combined <- expand_grid(
  batch       = 1:n_batches,
  participant = 1:sample_size
) |>
  group_by(batch) |>
  mutate(sample = slice_sample(delegates, n = sample_size, replace = TRUE)) |>
  ungroup() |>
  unnest(sample) |>
  mutate(label = str_glue("Joint age + distance, n = {sample_size} per survey"))
trial_combined |> head(100)


# ============================================================
# 7. NORMAL APPROXIMATION OF THE BINOMIAL ----
# ============================================================
# NOTE: we didn't get to cover this section live -- the comments below
# carry the full explanation from the .qmd, for self-study.
#
# A Binomial count is itself a sum of n independent Bernoulli trials --
# by the CLT we just saw (section 6 above), that sum's own distribution
# should look increasingly Normal as n grows, the same logic as a
# sampling distribution of a mean. That means a Binomial's probabilities
# can be approximated with a Normal distribution matched on mean and
# variance -- useful whenever an exact pbinom() calculation would be
# impractical, and the reason this approximation is worth knowing rather
# than always just calling pbinom() directly. Back to the summit
# scenario (120 delegates invited, 85% show-up chance each, venue seats
# 100):
#
#   mean of K = n * p
#   variance of K = n * p * (1 - p)

n_invited   <- 120
p_attend    <- 0.85
mean_attend <- n_invited * p_attend
sd_attend   <- sqrt(n_invited * p_attend * (1 - p_attend))

# In general, the Normal approximation replaces an exact Binomial tail
# probability with the matching Normal's:
#
#   P(K > 100) is approximately P(X > 100), where X ~ Normal(mean = n*p, variance = n*p*(1-p))
#
# Concretely, pnorm() computes that Normal probability directly off the
# Normal CDF itself -- the same "F_Normal" from this session's
# Normal-CDF formula earlier (section 5 above):
#
#   P(X > 100) = 1 - F_Normal(100; mean = n*p, variance = n*p*(1-p))

1 - pnorm(q = 100, mean = mean_attend, sd = sd_attend)

# pbinom() computes that same right-tail probability exactly instead --
# straight off the Binomial CDF itself, no Normal approximation
# involved:
#
#   P(K > 100) = 1 - F_Binomial(100; n, p)

1 - pbinom(q = 100, size = n_invited, prob = p_attend) # close to the normal approx above!

# The two match closely -- closely enough that overlaying the exact
# Binomial pmf (dbinom() at every integer k) directly on top of its
# fitted Normal curve -- Normal(mean = n*p, variance = n*p*(1-p)),
# evaluated at this scenario's own mean_attend/sd_attend -- makes the
# approximation's quality visible at a glance: the same curve whose
# right tail beyond 100 pnorm() just integrated above.

binom_pmf <- tibble(
  attendees = 0:n_invited,
  pmf       = dbinom(attendees, n_invited, p_attend)
)

normal_pdf <- tibble(
  attendees = seq(0, n_invited, by = 0.01),
  density   = dnorm(attendees, mean_attend, sd_attend)
)

binom_label  <- str_glue("Binomial({n_invited}, {p_attend}) pmf -- dbinom()")
normal_label <- str_glue("Fitted N({mean_attend}, {round(sd_attend^2, 1)}) -- dnorm()")

ggplot() +
  geom_segment(
    data = binom_pmf, aes(x = attendees, xend = attendees, y = 0, yend = pmf),
    color = "grey75", linewidth = 0.3
  ) +
  geom_point(data = binom_pmf, aes(x = attendees, y = pmf, color = binom_label), size = 1.2) +
  geom_line(data = normal_pdf, aes(x = attendees, y = density, color = normal_label), linewidth = 0.8) +
  scale_color_manual(name = NULL, values = set_names(c("#B8860B", "#2f7f93"), c(binom_label, normal_label))) +
  labs(
    title = "Binomial pmf vs. its Normal approximation",
    x     = "Delegates showing up",
    y     = "Density / probability"
  )


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
  x          = seq(mean_a - 3 * sd_a, mean_a + 3 * sd_a, length.out = 200),
  density    = dnorm(x, mean = mean_a, sd = sd_a),
  conference = "A",
  attendance = conf_a_attendance
)

conf_b_curve <- tibble(
  x          = seq(mean_b - 3 * sd_b, mean_b + 3 * sd_b, length.out = 200),
  density    = dnorm(x, mean = mean_b, sd = sd_b),
  conference = "B",
  attendance = conf_b_attendance
)

standardize_curves <- bind_rows(conf_a_curve, conf_b_curve)
standardize_curves

standardize_curves |>
  ggplot(aes(x = x, y = density)) +
  geom_line() +
  geom_vline(aes(xintercept = attendance), color = "#2f7f93", linetype = "dashed") +
  facet_wrap(~conference, scales = "free", labeller = label_both) +
  labs(
    title = "Each conference vs. its own series",
    x     = "Attendance (delegates)",
    y     = "Density"
  )


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
