# ============================================================
# WEEK 1 -- CLASSROOM QUICK-REFERENCE
# ============================================================

# Setup

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse)


# ============================================================
# 1. FROM WIDE TO LONG ----
# ============================================================
# same Bundestag polling data from the Intro R course, still wide

polls <- read_csv("data/polls.csv")
polls |> head(100)

polls_long <- polls |>
  pivot_longer(CDUCSU:BSW, names_to = "party", values_to = "share")
polls_long


# ============================================================
# 2. GROUPING AND SUMMARIZING ----
# ============================================================
# grouping by two variables at once, same idea as Day 3's cat1/cat2 tibble

polls_long |>
  mutate(year = year(Date)) |>
  group_by(party, year) |>
  summarize(avg_share = mean(share, na.rm = TRUE)) |>
  ungroup() |>
  arrange(party, year)

# collapsing to one row per party only

party_avg <- polls_long |>
  group_by(party) |>
  summarize(avg_share = mean(share, na.rm = TRUE)) |>
  arrange(desc(avg_share))
party_avg


# ============================================================
# 3. PLOTTING THE RESULT ----
# ============================================================
# real party colors, same vector as Intro R Day 2/3

party_colors <- c(
  CDUCSU = "#000000",
  SPD    = "#E3000F",
  Gruene = "#46962b",
  FDP    = "#ffed00",
  Linke  = "#be3075",
  AfD    = "#009ee0",
  BSW    = "#7d3f98"
)

# a bar chart of the summarize() result

party_avg |>
  ggplot(aes(x = party, y = avg_share, fill = party)) +
  geom_col() +
  scale_fill_manual(values = party_colors) +
  labs(title = "Average Bundestag polling share, 2017-today", x = NULL, y = "Share (%)")

# the full trend line, Intro R Day 2/3's central plot

polls_long |>
  ggplot(aes(x = Date, y = share, color = party)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = party_colors) +
  labs(title = "Bundestag polling trend", x = NULL, y = "Share (%)", color = "Party")


# ============================================================
# 4. MEASURES OF CENTRAL TENDENCY: THE MEAN ----
# ============================================================

x <- 1:5
x

mean(x)
sum(x) / length(x)

# only makes sense for numeric vectors
x_char <- c("a", "b", "c", "d", "e")
mean(x_char)

# but it does work for logicals -- TRUE counts as 1, FALSE as 0
x_logical <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
mean(x_logical)
sum(x_logical)

# the same mean again, now via a tibble and summarize() --
# from here on every aggregation goes through a tibble

x_tbl <- tibble(xcol = x)
x_tbl |> summarize(mean_x = mean(xcol))


# ============================================================
# 5. CENTRAL TENDENCY AND VARIABILITY, THE REST OF THE TOOLBOX ----
# ============================================================
# mean(), median(), var(), sd(), range(), IQR() -- pooling every party's
# share into one number is illustrative only, see the Try it below

polls_long |>
  summarize(
    mean_share   = mean(share, na.rm = TRUE),
    median_share = median(share, na.rm = TRUE),
    var_share    = var(share, na.rm = TRUE),
    sd_share     = sd(share, na.rm = TRUE),
    min_share    = min(share, na.rm = TRUE),
    max_share    = max(share, na.rm = TRUE),
    iqr_share    = IQR(share, na.rm = TRUE)
  )


# ============================================================
# 6. THE MODE: THREE DEFINITIONS ----
# ============================================================
# no dedicated mode() function in R. One small tibble: shares (numeric)
# has 3 and 7 tied at 3 occurrences each; parties (character), same
# length, SPD the single clear winner, no tie

mode_data <- tibble(
  shares  = c(3, 7, 3, 9, 7, 2, 3, 7, 5, 1),
  parties = c("SPD", "CDUCSU", "SPD", "Gruene", "SPD", "AfD", "SPD", "Linke", "FDP", "CDUCSU")
)
mode_data

# select() the column we need, then group_by() + summarize(n()), the way
# we already know

shares_counts <- mode_data |>
  select(shares) |>
  group_by(shares) |>
  summarize(n = n()) |>
  ungroup()
shares_counts

# count() is a shortcut for that same group_by()+summarize(n())+ungroup()
# combination, in one step

parties_counts <- mode_data |>
  select(parties) |>
  count(parties)
parties_counts

# Definition 1: every value tied for the highest count (multimodal)
shares_counts |>
  filter(n == max(n))

# Definition 2: a single "the" mode. filter()-ing for the max count alone
# isn't tie-safe -- arrange() + slice(1) always returns exactly one row
parties_counts |>
  arrange(desc(n)) |>
  slice(1)

# Definition 3: for numeric data, average the tied values into one number
shares_counts |>
  filter(n == max(n)) |>
  summarize(mean_mode = mean(shares))


# ============================================================
# 7. PLOTTING DATA, REVISITED IN GGPLOT2 ----
# ============================================================
# same ideas as Intro R Day 2, plus one new geom

polls_long |>
  ggplot(aes(x = share)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of polled party shares", x = "Share (%)")

polls_long |>
  ggplot(aes(x = share)) +
  geom_density() +
  labs(title = "Density of polled party shares", x = "Share (%)")

polls_long |>
  ggplot(aes(x = "", y = share)) +
  geom_boxplot() +
  labs(title = "Boxplot of polled party shares", x = NULL, y = "Share (%)")

# a violin plot shows the full shape of the distribution, not just its quartiles
polls_long |>
  ggplot(aes(x = "", y = share)) +
  geom_violin() +
  labs(title = "Violin plot of polled party shares", x = NULL, y = "Share (%)")


# ============================================================
# 8. TRY IT: AfD'S HISTORIC HIGH AND LOW ----
# ============================================================
# Your task: populist parties' support can swing dramatically over time --
# looking at AfD's entire historic polling record (not just recent
# numbers), what's the highest and lowest share it has ever been polled
# at?
#
# Your task: different pollsters can paint quite different pictures --
# repeat this, but separately for each pollster: find AfD's highest and
# lowest recorded share (and the date) per pollster.


# ============================================================
# 9. TRY IT: HOW MUCH DID THE POLLS AGREE RIGHT BEFORE THE 2025 ELECTION? ----
# ============================================================
# Your task: the 2025 federal election was held 2025-02-23. Calculate the
# mean, variance, and standard deviation of the Green party's polled
# share in the four weeks leading up to it, then plot the distribution of
# those final polls against the Greens' actual result (11.6%).


# ============================================================
# 10. TRY IT: BSW'S RISE ABOVE -- AND FALL BACK BELOW -- THE 5% THRESHOLD ----
# ============================================================
# Your task: BSW is a newly founded party -- founded 2024-01-08 -- that
# had to clear the 5% electoral threshold to enter parliament at all.
# Look at the remainder of that year (2024-01-08 to 2024-12-31):
# mutate() a cleared_threshold column. Compute the average share per
# pollster for each above and below the threshold. Plot the distribution
# of BSW's share by pollster.
