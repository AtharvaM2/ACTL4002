library(dplyr)
library(ggplot2)
library(MASS)       # glm.nb for negative binomial frequency model
library(scales)
library(tidyr)
library(readr)

theme_set(theme_minimal(base_size = 12))
COL_AGRI    <- "#2D6A4F"
COL_CLIMATE <- "#BA7517"
COL_SEVERE  <- "#A32D2D"
COL_WATER   <- "#185FA5"
COL_GAP     <- "#993C1D"

dir.create("outputs", showWarnings = FALSE)

philippines_master <- read_csv("data/philippines_master.csv", show_col_types = FALSE)

# =============================================================
# PART 1 - FREQUENCY MODEL
# How often does a triggering event happen?
# =============================================================
# Primary trigger per your dual-index design = typhoon wind speed (storm events).
# Restrict to years where the disaster panel actually has data (EM-DAT coverage
# window) - fitting a rate over years where the field is NA would silently drop
# those rows anyway, but being explicit here avoids confusion later.

freq_data <- philippines_master %>% filter(!is.na(n_storm_events))

cat("Frequency model fitted on", nrow(freq_data), "years:",
    min(freq_data$year), "-", max(freq_data$year), "\n\n")

freq_stats <- freq_data %>%
  summarise(mean_events = mean(n_storm_events), var_events = var(n_storm_events))
cat("Mean storm events/year:", round(freq_stats$mean_events, 2),
    "| Variance:", round(freq_stats$var_events, 2), "\n")
cat("Variance/mean ratio:", round(freq_stats$var_events / freq_stats$mean_events, 2),
    "(>1 suggests overdispersion -> negative binomial preferred over Poisson)\n\n")

pois_fit <- glm(n_storm_events ~ 1, data = freq_data, family = poisson())
nb_fit   <- tryCatch(MASS::glm.nb(n_storm_events ~ 1, data = freq_data),
                      error = function(e) NULL)

if (!is.null(nb_fit)) {
  aic_compare <- AIC(pois_fit, nb_fit)
  print(aic_compare)
  use_nb <- aic_compare["nb_fit", "AIC"] < aic_compare["pois_fit", "AIC"]
} else {
  use_nb <- FALSE
}

lambda_hat <- exp(coef(pois_fit)[1])
cat("\nPoisson rate (lambda):", round(lambda_hat, 2), "events/year\n")
if (!is.null(nb_fit)) {
  cat("Negative binomial mean:", round(exp(coef(nb_fit)[1]), 2),
      "| dispersion (theta):", round(nb_fit$theta, 2), "\n")
  cat("Preferred model:", ifelse(use_nb, "Negative Binomial", "Poisson"), "(lower AIC)\n")
}

# =============================================================
# PART 2 - SEVERITY MODEL
# Given a trigger fires, how big is the payout?
# =============================================================
# Tied to YOUR product's own tiered payout structure (mild/moderate/severe based on
# yield shortfall vs trend) rather than generic EM-DAT economy-wide damage figures -
# this way the simulation result plugs directly into the loss-ratio / pricing checks
# from your Step 1-6 pricing work (Sum Insured = PHP 60,000/ha, Office Premium =
# PHP 16,170/ha), instead of producing a number you'd then have to translate.
#
# This ALSO replaces the "35% trigger frequency" placeholder assumption from your
# pricing notes with an empirically-derived figure from real yield data - exactly
# the kind of evidence-based refinement the GAIP rubric rewards.

yield_data <- philippines_master %>% filter(!is.na(cereal_yield_kg_per_hectare))
yield_trend <- lm(cereal_yield_kg_per_hectare ~ year, data = yield_data)
yield_data$yield_resid_pct <- residuals(yield_trend) / fitted(yield_trend)
resid_sd <- sd(yield_data$yield_resid_pct)

cat("\nYield residual SD (% deviation from trend):", round(resid_sd * 100, 2), "%\n")

SI <- 60000  # PHP/ha - Sum Insured, from your pricing Step 1

classify_tier <- function(z) {
  case_when(
    z <= -2.5 ~ "severe",
    z <= -1.5 ~ "moderate",
    z <= -1.0 ~ "mild",
    TRUE      ~ "none"
  )
}
payout_table <- c(none = 0, mild = 0.30 * SI, moderate = 0.60 * SI, severe = 1.00 * SI)

yield_data <- yield_data %>%
  mutate(z = yield_resid_pct / resid_sd, tier = classify_tier(z))

tier_probs <- yield_data %>%
  count(tier) %>%
  mutate(p = n / sum(n)) %>%
  complete(tier = names(payout_table), fill = list(n = 0, p = 0))  # keep all 4 tiers even if unseen historically

cat("\nEmpirical trigger-tier probabilities (from", nrow(yield_data), "years of yield data):\n")
print(tier_probs)

p_any_trigger <- 1 - tier_probs$p[tier_probs$tier == "none"]
cat("\nImplied overall trigger frequency:", percent(p_any_trigger, accuracy = 0.1),
    "(compare to the 35% assumption in your pricing notes)\n")

# =============================================================
# PART 3 - MONTE CARLO LOSS SIMULATION
# =============================================================
# IMPORTANT design choice: we draw a CONTINUOUS standardised yield shock each
# simulated year (from a fitted distribution) and THEN bucket it into your tiers,
# rather than resampling directly from the ~63 historical years' tier labels.
#
# Why this matters: your "severe" tier has only 2 observations in 63 years of
# history. Resampling those 2 rows directly makes the tail of the simulation
# exactly as noisy as a 2-data-point estimate - any one unusual year would swing
# the whole tail. Fitting a distribution (Normal here; Student-t offered below
# for a fatter-tailed alternative) borrows strength from the WHOLE distribution
# shape to estimate tail probability, which is the standard actuarial approach
# for thin tail data (this is literally what Loss Models / EVT methods exist for).
#
# z is already standardised (mean 0, sd 1 by construction from Part 2), so the
# base case is simply z ~ Normal(0,1). Stress tests in Part 4 shift the mean
# and/or scale the sd of this same distribution - far more interpretable than an
# ad hoc "frequency multiplier" knob.

set.seed(42)
n_sim <- 10000
portfolio_ha <- 10000          # adjust to your actual target portfolio size
office_premium_per_ha <- 16170 # PHP/ha, from your pricing Step 3
total_premium_pool <- office_premium_per_ha * portfolio_ha

simulate_years <- function(n_sim, mean_shift = 0, sd_scale = 1,
                            dist = c("normal", "t"), t_df = 5) {
  dist <- match.arg(dist)
  z <- if (dist == "normal") {
    rnorm(n_sim, mean = mean_shift, sd = sd_scale)
  } else {
    mean_shift + sd_scale * rt(n_sim, df = t_df)   # fatter tails than Normal
  }
  tier <- classify_tier(z)
  payout_per_ha <- unname(payout_table[tier])
  tibble(
    sim_id = 1:n_sim, z = z, tier = tier,
    payout_per_ha = payout_per_ha,
    total_claims_php = payout_per_ha * portfolio_ha,
    loss_ratio = (payout_per_ha * portfolio_ha) / total_premium_pool
  )
}

base_results <- simulate_years(n_sim)

# Sense-check: does the fitted-Normal trigger frequency roughly match the
# empirical historical frequency from Part 2? Large gaps are worth investigating
# (e.g. historical shocks may be skewed, not symmetric - common for droughts).
cat("\nFitted-distribution trigger frequency:", percent(mean(base_results$tier != "none"), accuracy = 0.1),
    "| Historical empirical frequency:", percent(p_any_trigger, accuracy = 0.1), "\n")

base_summary <- base_results %>% summarise(
  mean_payout_per_ha          = mean(payout_per_ha),
  mean_loss_ratio             = mean(loss_ratio),
  VaR_95                      = quantile(loss_ratio, 0.95),
  VaR_99                      = quantile(loss_ratio, 0.99),
  TVaR_95                     = mean(loss_ratio[loss_ratio >= quantile(loss_ratio, 0.95)]),
  prob_loss_ratio_over_90pct  = mean(loss_ratio > 0.90)
)
cat("\n=== BASE CASE: 10,000-year Monte Carlo simulation ===\n")
print(base_summary)

p_loss_dist <- ggplot(base_results, aes(loss_ratio)) +
  geom_histogram(bins = 50, fill = COL_AGRI, alpha = 0.85) +
  geom_vline(xintercept = 0.628, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_vline(xintercept = base_summary$VaR_95, linetype = "dashed", color = COL_SEVERE, linewidth = 0.8) +
  annotate("text", x = 0.628, y = Inf, label = "PCIC long-run avg (62.8%)",
           hjust = -0.05, vjust = 2, size = 3, color = "grey40") +
  annotate("text", x = base_summary$VaR_95, y = Inf, label = "95% VaR",
           hjust = -0.1, vjust = 4, size = 3, color = COL_SEVERE) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Simulated annual loss ratio distribution (10,000 years)",
       subtitle = "Base case - no stress applied",
       x = "Loss ratio (claims / premium pool)", y = "Simulated years")

ggsave("outputs/05_loss_ratio_distribution.png", p_loss_dist, width = 8, height = 5, dpi = 120)

# =============================================================
# PART 4 - STRESS TESTING
# =============================================================
# Three climate-narrative-aligned levers, each mapped onto the SAME fitted
# distribution from Part 3 (so results stay comparable to the base case):
#   1. Chronic drying  - mean_shift < 0: a permanent downward drift in typical
#      yields (the "new normal is worse" scenario), holding volatility fixed.
#   2. More erratic seasons - sd_scale > 1: same average outcome, but wider
#      year-to-year swings (the "less predictable, not necessarily worse on
#      average" scenario).
#   3. Fatter tail - dist = "t": same mean and spread, but more extreme outlier
#      years than a Normal distribution would predict (the "rare disasters are
#      less rare than they used to be" scenario - arguably the most realistic
#      reading of climate change's effect on tail risk specifically).

scenarios <- list(
  base             = list(mean_shift = 0,    sd_scale = 1.0, dist = "normal"),
  chronic_drying   = list(mean_shift = -0.3, sd_scale = 1.0, dist = "normal"),
  more_erratic     = list(mean_shift = 0,    sd_scale = 1.3, dist = "normal"),
  fatter_tail      = list(mean_shift = 0,    sd_scale = 1.0, dist = "t"),
  combined_stress  = list(mean_shift = -0.3, sd_scale = 1.3, dist = "t")
)

stress_results <- lapply(names(scenarios), function(nm) {
  s <- scenarios[[nm]]
  res <- simulate_years(n_sim, mean_shift = s$mean_shift, sd_scale = s$sd_scale, dist = s$dist)
  res %>% summarise(
    scenario        = nm,
    trigger_freq    = mean(tier != "none"),
    mean_loss_ratio = mean(loss_ratio),
    VaR_95          = quantile(loss_ratio, 0.95),
    VaR_99          = quantile(loss_ratio, 0.99),
    prob_over_90pct = mean(loss_ratio > 0.90)
  )
}) %>% bind_rows()

cat("\n=== STRESS TEST COMPARISON ===\n")
print(stress_results)

p_stress <- stress_results %>%
  pivot_longer(c(mean_loss_ratio, VaR_95, VaR_99), names_to = "metric", values_to = "value") %>%
  ggplot(aes(scenario, value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(mean_loss_ratio = COL_AGRI, VaR_95 = COL_CLIMATE, VaR_99 = COL_SEVERE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Loss ratio under stress scenarios", y = "Loss ratio", x = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/06_stress_test_comparison.png", p_stress, width = 8, height = 5, dpi = 120)

# =============================================================
# PART 5 - RAINFALL/YIELD THRESHOLD SENSITIVITY
# =============================================================
# Your task list calls for "different rain thresholds" - this is the yield-
# residual analogue until PAGASA rainfall data is merged in directly. Shows how
# moving the tier cut-points (a PRODUCT DESIGN choice, distinct from the climate
# stress scenarios above, which are about the WORLD getting worse) changes
# trigger frequency and average payout under the same base-case distribution.

threshold_sets <- list(
  tight = c(mild = -1.25, moderate = -1.75, severe = -2.75),
  base  = c(mild = -1.00, moderate = -1.50, severe = -2.50),
  loose = c(mild = -0.75, moderate = -1.25, severe = -2.25)
)

z_base <- simulate_years(n_sim)$z   # reuse one fixed set of simulated shocks across thresholds

threshold_sensitivity <- lapply(names(threshold_sets), function(nm) {
  cuts <- threshold_sets[[nm]]
  tier_alt <- case_when(
    z_base <= cuts["severe"]   ~ "severe",
    z_base <= cuts["moderate"] ~ "moderate",
    z_base <= cuts["mild"]     ~ "mild",
    TRUE ~ "none"
  )
  payout_alt <- unname(payout_table[tier_alt])
  tibble(threshold_set = nm,
         trigger_frequency  = mean(tier_alt != "none"),
         mean_payout_per_ha = mean(payout_alt),
         mean_loss_ratio    = mean(payout_alt * portfolio_ha / total_premium_pool))
}) %>% bind_rows()

cat("\n=== THRESHOLD SENSITIVITY (product design choice) ===\n")
print(threshold_sensitivity)

# =============================================================
# SAVE OUTPUTS
# =============================================================
write_csv(base_results, "outputs/monte_carlo_base_results.csv")
write_csv(stress_results, "outputs/stress_test_summary.csv")
write_csv(threshold_sensitivity, "outputs/threshold_sensitivity.csv")

cat("\nAll modeling outputs saved to outputs/.\n")
