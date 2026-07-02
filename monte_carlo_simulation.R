# =============================================================================
# FILE 2: 02_monte_carlo_simulation.R  (v2 - vectorised)
# PURPOSE: Monte Carlo aggregate-loss simulation and stress testing, built
#          directly on the fitted model objects from 01_models.R.
#
# DEPENDENCIES: Run 01_models.R first (produces model_objects_V5.RData).
#
# v2 NOTE: v1 called predict.gam() one row at a time inside nested loops -
# with 20,000 simulated years that's 100,000+ individual predict() calls,
# each paying GAM's matrix-construction overhead, and it never finished.
# This version batches every prediction into ONE vectorised call per stage
# (all years at once for frequency, all events at once for severity) and
# is the difference between not finishing and running in a few seconds.
#
# SIMULATION DESIGN:
#
#   Frequency draw (per simulated year):
#     freq_zinb gives, for any covariate set, a zero-inflation probability
#     pi (structural "no storm regime" probability) and a Negative Binomial
#     mean mu + dispersion theta for the count component. To simulate:
#     draw a structural-zero indicator ~ Bernoulli(pi); if 0, draw
#     N ~ NegBinom(mu, theta) (this can ALSO land on 0 by chance - that's
#     the "sampling zero" path, distinct from the structural zero above).
#     This is the standard ZINB simulation recipe, not a simplification.
#
#   Severity draw (per simulated event):
#     sev_event_gam (Gamma GAM, log link) gives a fitted mean mu_event for
#     any covariate set. The Gamma dispersion phi is summary(model)$scale,
#     where Var(Y) = phi * mu^2 (mgcv's parametrisation). Shape = 1/phi,
#     rate = shape/mu_event reproduces a Gamma draw with the right mean
#     AND the right variance - drawing from a point estimate alone (no
#     dispersion) would silently understate tail risk.
#
#   Covariate source for each simulated year:
#     Rather than drawing precip/ENSO/employment/etc. as independent
#     Normals (which would erase their real joint correlation - e.g. El
#     Nino years and rainfall deficit move together), we BOOTSTRAP RESAMPLE
#     entire historical year-rows from model_data. This preserves the real
#     joint covariate structure the model was actually fitted on. Per-event
#     severity drivers (event_magnitude, log_affected) are resampled
#     similarly from event_data, since these vary storm-to-storm even
#     within the same year.
#
#   Process risk vs parameter risk (these are NOT the same thing):
#     The simulation captures PROCESS risk - randomness in outcomes given
#     the fitted model is exactly correct. It does NOT capture PARAMETER
#     risk - uncertainty in whether the fitted coefficients themselves are
#     correct, which 01_models.R already quantified via block bootstrap
#     (boot_expected_loss, parameter_risk_factor = 2.97). Section 3 below
#     combines both into a TOTAL risk distribution, the way Klugman/Panjer/
#     Willmot "Loss Models" recommends: each process-risk draw is rescaled
#     by a parameter-risk multiplier drawn from the existing bootstrap
#     distribution. Reporting process risk alone would understate how
#     uncertain this estimate really is with only 44 years of data.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(pscl)
library(mgcv)

load("model_objects_V5.RData")

COL_GREEN <- "#1D9E75"
COL_RED   <- "#E24B4A"
COL_BLUE  <- "#185FA5"
COL_AMBER <- "#BA7517"
theme_set(theme_minimal(base_size = 12))

set.seed(2026)
dir.create("outputs_mc", showWarnings = FALSE)

sev_phi <- summary(sev_event_gam)$scale  # Gamma dispersion, Var(Y) = phi * mu^2

# --- Extrapolation guard rail -----------------------------------------------
# Resampling marginal covariates independently (event severity-drivers from
# event_data, year-level exposure/climate from model_data) means every
# INDIVIDUAL value fed to the GAM is a real historical value - but the
# COMBINATION may never have co-occurred. For an additive model on a log
# link, summing several smooths' tail values that never appeared together
# historically can still produce a back-transformed mean far beyond
# anything the model ever actually fitted (verified: up to ~75,000 M USD
# for a single event vs a real historical max of ~13,820 M USD total damage
# for an entire YEAR). This is a genuine, known hazard of flexible smooths +
# log link + thin data (141 events), not a simulation coding error - and it
# would silently wreck every result below if left unguarded.
#
# Fix: cap the predicted MEAN (mu_evt) at the model's own maximum in-sample
# fitted value. Individual Gamma draws can still exceed this (preserving
# genuine tail variability) - only the MEAN the model is allowed to assert
# for a never-seen covariate combination is bounded by what it actually
# produced for real, observed events.
sev_mu_cap <- max(predict(sev_event_gam, type = "response"))
cat("Severity mean prediction capped at in-sample max: M USD", round(sev_mu_cap, 1), "\n")

# =============================================================================
# SECTION 1 - VECTORISED SIMULATION ENGINE
# =============================================================================

#' Simulate n_sim independent years in one batch.
#' year_overrides: named list of covariate values to FIX across every
#' simulated year (used by the stress scenarios in Section 4) - any
#' covariate not named here is bootstrap-resampled from source_data.
run_monte_carlo <- function(n_sim, year_overrides = list(), source_data = model_data) {

  # --- Stage 1: resample n_sim year-rows, apply any stress overrides ---
  yr_idx  <- sample(seq_len(nrow(source_data)), n_sim, replace = TRUE)
  yrs     <- source_data[yr_idx, ]
  for (nm in names(year_overrides)) yrs[[nm]] <- year_overrides[[nm]]
  yrs$sim_id <- seq_len(n_sim)

  # --- Stage 2: ONE vectorised frequency prediction for all n_sim years ---
  pi_hat <- predict(freq_zinb, newdata = yrs, type = "zero")
  mu_hat <- predict(freq_zinb, newdata = yrs, type = "count")

  is_struct_zero <- rbinom(n_sim, 1, pi_hat)
  n_events <- ifelse(is_struct_zero == 1, 0L,
                      rnbinom(n_sim, mu = mu_hat, size = freq_zinb$theta))
  yrs$n_events <- n_events

  total_events <- sum(n_events)
  if (total_events == 0) {
    return(tibble(sim_id = yrs$sim_id, n_events = n_events, loss = 0))
  }

  # --- Stage 3: build ONE big event-level dataframe for every event across
  # every simulated year, then predict severity in a single vectorised call ---
  evt_year_idx <- rep(seq_len(n_sim), times = n_events)
  evt_draw_idx <- sample(seq_len(nrow(event_data)), total_events, replace = TRUE)

  evt_covs <- event_data[evt_draw_idx, c("event_magnitude", "log_affected")]
  evt_covs$days_precip_over_20mm <- yrs$days_precip_over_20mm[evt_year_idx]
  evt_covs$year_scaled           <- yrs$year_scaled[evt_year_idx]
  evt_covs$agri_emp_scaled       <- yrs$agri_emp_scaled[evt_year_idx]
  evt_covs$agri_va_scaled        <- yrs$agri_va_scaled[evt_year_idx]

  mu_evt <- as.numeric(predict(sev_event_gam, newdata = evt_covs, type = "response"))
  mu_evt <- pmin(mu_evt, sev_mu_cap)
  shape  <- 1 / sev_phi
  severities <- rgamma(total_events, shape = shape, rate = shape / mu_evt)

  # --- Stage 4: aggregate event severities back up to annual loss ---
  loss_vec <- numeric(n_sim)
  agg <- tapply(severities, evt_year_idx, sum)
  loss_vec[as.integer(names(agg))] <- as.numeric(agg)

  tibble(sim_id = yrs$sim_id, n_events = n_events, loss = loss_vec)
}

risk_metrics <- function(loss_vec, label = "scenario") {
  tibble(
    scenario        = label,
    mean_loss       = mean(loss_vec),
    sd_loss         = sd(loss_vec),
    prob_zero_loss  = mean(loss_vec == 0),
    VaR_90          = quantile(loss_vec, 0.90),
    VaR_95          = quantile(loss_vec, 0.95),
    VaR_99          = quantile(loss_vec, 0.99),
    TVaR_95         = mean(loss_vec[loss_vec >= quantile(loss_vec, 0.95)]),
    TVaR_99         = mean(loss_vec[loss_vec >= quantile(loss_vec, 0.99)])
  )
}

# =============================================================================
# SECTION 2 - BASE CASE MONTE CARLO (PROCESS RISK ONLY)
# =============================================================================

n_sim <- 20000
t0 <- Sys.time()
base_sim <- run_monte_carlo(n_sim)
cat("Base case simulation runtime:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

base_metrics <- risk_metrics(base_sim$loss, "Base case (process risk only)")

cat("\n=== BASE CASE: PROCESS RISK ONLY (", n_sim, "simulated years) ===\n")
print(base_metrics)

cat("\nSense check against 01_models.R's own burn cost:\n")
cat("  Simulated mean annual loss: M USD", round(base_metrics$mean_loss, 1), "\n")
cat("  Historical burn cost:       M USD", round(burn_cost_annual$burn_cost_m_usd_yr, 1), "\n")
cat("  Ratio (sim/historical):     ", round(base_metrics$mean_loss / burn_cost_annual$burn_cost_m_usd_yr, 2), "\n")
cat("  (Close to 1.0 = the simulation engine reproduces the model's own\n")
cat("   historical burn cost, as it should before trusting anything downstream.)\n")

# =============================================================================
# SECTION 3 - TOTAL RISK: PROCESS + PARAMETER RISK COMBINED
# =============================================================================

param_multiplier <- boot_expected_loss / mean(boot_expected_loss)
total_risk_sim <- base_sim %>%
  mutate(param_mult = sample(param_multiplier, n(), replace = TRUE),
         loss_total = loss * param_mult)

total_metrics <- risk_metrics(total_risk_sim$loss_total, "Total risk (process + parameter)")

cat("\n=== TOTAL RISK: PROCESS + PARAMETER (same", n_sim, "years, rescaled) ===\n")
print(total_metrics)
cat("\nVaR_95 inflation from parameter risk alone:",
    percent(total_metrics$VaR_95 / base_metrics$VaR_95 - 1, accuracy = 0.1), "\n")
cat("This is the cost of having only", nrow(model_data), "years of data to estimate from -\n")
cat("not a flaw in the simulation, worth a sentence in your limitations slide.\n")

p_loss_dist <- ggplot(base_sim, aes(loss)) +
  geom_histogram(bins = 60, fill = COL_GREEN, alpha = 0.5) +
  geom_histogram(data = total_risk_sim, aes(loss_total), bins = 60,
                 fill = COL_RED, alpha = 0.35) +
  geom_vline(xintercept = base_metrics$VaR_95, color = COL_GREEN, linetype = "dashed") +
  geom_vline(xintercept = total_metrics$VaR_95, color = COL_RED, linetype = "dashed") +
  scale_x_continuous(labels = comma_format(),
                      limits = c(0, quantile(total_risk_sim$loss_total, 0.995))) +
  labs(title = "Simulated annual loss distribution",
       subtitle = "Green = process risk only | Red = process + parameter risk | dashed = 95% VaR",
       x = "Annual loss (M USD)", y = "Simulated years")

ggsave("outputs_mc/01_loss_distribution.png", p_loss_dist, width = 9, height = 5.5, dpi = 120)

# =============================================================================
# SECTION 4 - STRESS TESTING
# =============================================================================
# Four probabilistic scenarios, each grounded in a specific covariate the
# fitted models actually use, plus one deterministic historical replay -
# not generic "+20% to everything" multipliers.

hist_precip_p90 <- quantile(model_data$precip_scaled, 0.90, na.rm = TRUE)
hist_enso_max   <- max(model_data$enso_scaled, na.rm = TRUE)
hist_year_max   <- max(model_data$year_scaled, na.rm = TRUE)
year_step       <- diff(range(model_data$year_scaled)) / diff(range(model_data$year))

stress_scenarios <- list(
  base                = list(),
  el_nino_regime      = list(enso_scaled = hist_enso_max),
  heavy_rain_regime   = list(precip_scaled = hist_precip_p90),
  climate_trend_10yr  = list(year_scaled = hist_year_max + 10 * year_step)
)

cat("\n=== STRESS SCENARIO 4 (climate_trend_10yr) - CAUTION ===\n")
cat("This shifts year_scaled 10 years beyond your data's actual range (2023).\n")
cat("GAM smooth terms are not validated outside their fitted range - this number\n")
cat("shows DIRECTION, not a figure to quote with confidence in front of judges.\n")
cat("Say so explicitly if you present it.\n")

stress_results <- lapply(names(stress_scenarios), function(nm) {
  sim <- run_monte_carlo(n_sim, year_overrides = stress_scenarios[[nm]])
  risk_metrics(sim$loss, nm)
}) %>% bind_rows()

# Scenario 5: historical worst-case replay (deterministic, not probabilistic).
worst_year_events <- max(model_data$n_storm_events, na.rm = TRUE)
cat_events        <- event_data %>% filter(is_catastrophic)
worst_case_covs   <- model_data[which.max(model_data$n_storm_events), ]

worst_case_loss <- if (nrow(cat_events) > 0) {
  evt_covs <- cat_events %>%
    dplyr::select(event_magnitude, log_affected) %>%
    slice(rep(1, worst_year_events))
  evt_covs$days_precip_over_20mm <- worst_case_covs$days_precip_over_20mm
  evt_covs$year_scaled           <- worst_case_covs$year_scaled
  evt_covs$agri_emp_scaled       <- worst_case_covs$agri_emp_scaled
  evt_covs$agri_va_scaled        <- worst_case_covs$agri_va_scaled
  sum(pmin(predict(sev_event_gam, newdata = evt_covs, type = "response"), sev_mu_cap))
} else NA

cat("\n=== STRESS SCENARIO 5: HISTORICAL WORST-CASE REPLAY (deterministic) ===\n")
cat("Worst observed storm count in a single year:", worst_year_events, "\n")
cat("If that many events all hit at catastrophic-tier severity:\n")
cat("  Expected loss: M USD", round(worst_case_loss, 1), "\n")
cat("  vs. base case mean: M USD", round(base_metrics$mean_loss, 1),
    "(", round(worst_case_loss / base_metrics$mean_loss, 1), "x )\n")

cat("\n=== STRESS TEST COMPARISON (probabilistic scenarios 1-4) ===\n")
print(stress_results %>% dplyr::select(scenario, mean_loss, VaR_95, VaR_99))

p_stress <- stress_results %>%
  dplyr::select(scenario, mean_loss, VaR_95, VaR_99) %>%
  pivot_longer(-scenario, names_to = "metric", values_to = "value") %>%
  mutate(scenario = factor(scenario, levels = names(stress_scenarios))) %>%
  ggplot(aes(scenario, value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = worst_case_loss, linetype = "dotted", color = COL_RED) +
  annotate("text", x = 1, y = worst_case_loss, label = "historical worst-case replay",
           vjust = -0.5, hjust = 0, size = 3, color = COL_RED) +
  scale_fill_manual(values = c(mean_loss = COL_GREEN, VaR_95 = COL_AMBER, VaR_99 = COL_RED)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Loss under stress scenarios", y = "M USD", x = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs_mc/02_stress_test_comparison.png", p_stress, width = 9, height = 5.5, dpi = 120)

# =============================================================================
# SECTION 5 - EXPORT
# =============================================================================
write.csv(base_sim,       "outputs_mc/monte_carlo_base_case.csv", row.names = FALSE)
write.csv(total_risk_sim, "outputs_mc/monte_carlo_total_risk.csv", row.names = FALSE)
write.csv(stress_results, "outputs_mc/stress_test_summary.csv", row.names = FALSE)
write.csv(bind_rows(base_metrics, total_metrics), "outputs_mc/risk_metrics_summary.csv", row.names = FALSE)

cat("\n=== MONTE CARLO + STRESS TESTING COMPLETE ===\n")
cat("All outputs saved to outputs_mc/\n")
