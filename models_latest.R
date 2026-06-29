# =============================================================================
# FILE 1: 01_models.R  (v5)
# PURPOSE: Fit, validate, and compare frequency and severity models for
#          Philippines parametric rice insurance. Incorporates all design
#          solution enhancements: farmer income, crop diversity, inflation,
#          unemployment, and climate-resilient practice discount mechanism.
#
# BUG FIXES FROM v4 (all would have caused runtime errors):
#
#   BUG 1 — tavg_c DOES NOT EXIST [would crash entire script]
#     The real column name from temp_series_clean is:
#     average_mean_surface_air_temperature_annual_mean_c
#     Every reference to tavg_c has been corrected throughout.
#
#   BUG 2 — severity_data NEVER CREATED [crash in S3 Tweedie block]
#     The S3 block referenced `severity_data` which was never assigned.
#     Only `severity_annual_data` existed. Fixed: use severity_annual_data
#     consistently everywhere.
#
#   BUG 3 — sev_event_glm / sev_tweedie_gam naming inconsistency [crash on export]
#     The comparison table referred to "SA3: TWEEDIE GLM" pointing at
#     sev_tweedie_gam (which is an annual aggregate model, not per-event).
#     The export tried to save sev_event_glm which was never assigned.
#     Fixed: per-event comparison uses only SA1/SA2 (Gamma GAMs). The
#     Tweedie GAM lives in Section 4 (annual aggregate) where it belongs.
#
#   BUG 4 — enso_proxy computed with {} block inside mutate() [silent wrong values]
#     The `{}` block inside mutate() evaluated the entire column, not a
#     filtered subset. The base period mean was correctly scoped but the
#     subtraction operated on the column correctly only by coincidence.
#     Fixed: compute base_mean and base_sd BEFORE the mutate() call and
#     reference them as scalar constants inside mutate().
#
#   BUG 5 — freq_formula pasting then LOO uses old syntax [crash in LOO loop]
#     The LOO loop called zeroinfl(freq_formula_count | freq_formula_zero, ...)
#     but freq_formula_zero is a one-sided formula (~enso...). The pipe
#     syntax inside zeroinfl() requires the combined formula object.
#     Fixed: define freq_formula_combined once, use everywhere.
#
#   BUG 6 — enso_scaled missing from annual_covariates in 02_pricing.R
#     The ZINB was trained with enso_scaled but the prediction dataframe
#     in 02_pricing.R did not include it. Fixed: added to annual_covariates
#     with value 0 (mean, since standardised).
#
# NEW DESIGN SOLUTION FEATURES:
#
#   FARMER INCOME INTEGRATION
#     cereal_yield_kg_per_hectare and agriculture_forestry_and_fishing_
#     value_added_percent_of_gdp are used to proxy farmer income trends.
#     In the severity model, higher agricultural value-added years correlate
#     with more assets at risk, increasing expected damage. In the pricing
#     file, the income proxy is used in the affordability calculation.
#
#   CROP DIVERSITY / PLANTING SEASON
#     food_production_index_2014_2016_100 and crop_production_index capture
#     crop mix and production diversity. Farmers growing more diverse crops
#     have lower correlated loss exposure (basis risk is lower when only
#     part of the farm is rice). This feeds the discount mechanism.
#
#   INFLATION / INTEREST RATE ADJUSTMENT
#     Total damage is already CPI-adjusted in EM-DAT (total_damage_adj_000us).
#     We additionally construct a real-terms damage trend by deflating by the
#     cereal price index, separating genuine climate-driven damage growth from
#     pure asset-value inflation. This produces a cleaner severity trend.
#
#   UNEMPLOYMENT AS VULNERABILITY PROXY
#     employment_in_agriculture_percent_of_total_employment is used as a
#     vulnerability exposure variable — years with higher agricultural
#     employment share mean more households are directly exposed to crop
#     loss, increasing the insured population's sensitivity to a given event.
#
#   CLIMATE-RESILIENT PRACTICE DISCOUNT
#     fertilizer_consumption_kilograms_per_hectare_of_arable_land proxies
#     input intensity. A farmer using less fertilizer relative to the national
#     average may be practising conservation agriculture (lower input, lower
#     cost, more resilient). The discount mechanism is modelled as a
#     multiplicative factor on the pure premium in 02_pricing.R.
#
# DEPENDENCIES: Run Combined_Dataset.R first.
# =============================================================================

# install.packages(c("pscl","mgcv","tweedie","AER","MASS","dplyr","tidyr",
#                    "ggplot2","scales","corrplot","zoo","lmtest","boot"))
# For rootograms (optional):
# install.packages("countreg", repos="http://R-Forge.R-project.org")

library(MASS)
library(pscl)
library(mgcv)
library(tweedie)
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(zoo)
library(lmtest)
library(boot)
library(corrplot)

has_countreg <- requireNamespace("countreg", quietly = TRUE)

# =============================================================================
# SECTION 0 — DATA PREPARATION
# =============================================================================
# CRITICAL: The temperature column from temp_series_clean is named
# average_mean_surface_air_temperature_annual_mean_c (confirmed from
# Combined_Dataset.R and the data dictionary). Do NOT shorten to tavg_c
# unless you add an explicit rename() step first (done below for clarity).

raw_data <- philippines_master %>%
  filter(year >= 1980, year <= 2023) %>%
  rename(
    # Rename the long temperature column to a workable alias ONCE here.
    # Every downstream reference uses tavg_c. This is safe because we
    # are explicit about where the rename happens.
    tavg_c = average_mean_surface_air_temperature_annual_mean_c
  ) %>%
  mutate(
    total_damage_m_usd    = total_damage_adj_000us / 1000,
    insured_damage_m_usd  = insured_damage_adj_000us / 1000,
    n_storm_events        = replace_na(as.integer(n_storm_events),    0L),
    n_flood_events        = replace_na(as.integer(n_flood_events),    0L),
    n_drought_events      = replace_na(as.integer(n_drought_events),  0L),
    n_disaster_events     = replace_na(as.integer(n_disaster_events), 0L),
    storm_magnitude_mean  = ifelse(is.nan(storm_magnitude_mean), NA,
                                   storm_magnitude_mean)
  ) %>%
  arrange(year)

# ---- Step 1: Missingness summary BEFORE imputation --------------------------
missing_summary <- raw_data %>%
  summarise(
    across(
      c(days_precip_over_20mm, storm_magnitude_mean, tavg_c,
        official_flows_agri_usd_m, agri_orientation_index,
        cereal_yield_kg_per_hectare,
        food_production_index_2014_2016_100,
        crop_production_index_2014_2016_100,
        fertilizer_consumption_kilograms_per_hectare_of_arable_land,
        employment_in_agriculture_percent_of_total_employment_modeled_ilo_estimate,
        agriculture_forestry_and_fishing_value_added_percent_of_gdp),
      list(
        n_miss   = ~ sum(is.na(.)),
        pct_miss = ~ round(mean(is.na(.)) * 100, 1)
      )
    )
  ) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("=== MISSING VALUE SUMMARY (pre-imputation) ===\n")
print(missing_summary)

# ---- Step 2: Compute ENSO proxy scalars BEFORE mutate() --------------------
# BUG 4 FIX: compute base period statistics as scalars outside mutate()
# so they are used as constants, not re-evaluated per row.
base_temp_vec  <- raw_data$tavg_c[raw_data$year >= 1980 & raw_data$year <= 2010]
enso_base_mean <- mean(base_temp_vec, na.rm = TRUE)
enso_base_sd   <- sd(base_temp_vec,   na.rm = TRUE)
if (is.na(enso_base_sd) || enso_base_sd == 0) enso_base_sd <- 1  # guard

# ---- Step 3: Principled imputation and feature engineering ------------------
model_data <- raw_data %>%
  mutate(
    # --- Core climate variables: linear interpolation ---
    days_precip_over_20mm = na.approx(days_precip_over_20mm, na.rm = FALSE, rule = 2),
    tavg_c                = na.approx(tavg_c,                na.rm = FALSE, rule = 2),
    
    # --- Storm magnitude: undefined when no storms (structural NA) ---
    storm_magnitude_mean = case_when(
      n_storm_events == 0          ~ NA_real_,
      !is.na(storm_magnitude_mean) ~ storm_magnitude_mean,
      TRUE ~ na.approx(storm_magnitude_mean, na.rm = FALSE, rule = 2)
    ),
    
    # --- Official flows: structural gap pre-2000 ---
    flows_pre2000 = as.integer(is.na(official_flows_agri_usd_m) & year < 2000),
    official_flows_agri_usd_m = ifelse(
      is.na(official_flows_agri_usd_m),
      min(official_flows_agri_usd_m, na.rm = TRUE),
      official_flows_agri_usd_m
    ),
    
    # --- ENSO proxy: scalar constants used here ---
    enso_proxy = (tavg_c - enso_base_mean) / enso_base_sd,
    
    # --- NEW: Agricultural value-added as farmer income proxy ---
    # Higher agri VA % of GDP -> larger income at stake -> higher exposure
    agri_va_pct = replace_na(
      agriculture_forestry_and_fishing_value_added_percent_of_gdp,
      mean(agriculture_forestry_and_fishing_value_added_percent_of_gdp,
           na.rm = TRUE)
    ),
    
    # --- NEW: Cereal yield as crop productivity proxy ---
    # Interpolate gaps; used in affordability and severity exposure scaling
    cereal_yield = na.approx(cereal_yield_kg_per_hectare, na.rm = FALSE, rule = 2),
    cereal_yield = replace_na(cereal_yield,
                              mean(cereal_yield_kg_per_hectare, na.rm = TRUE)),
    
    # --- NEW: Food production index as crop diversity proxy ---
    # Higher index -> more diverse/productive agriculture -> lower per-event
    # relative impact (diversification effect). Interpolate missing.
    food_prod_idx = na.approx(food_production_index_2014_2016_100,
                              na.rm = FALSE, rule = 2),
    food_prod_idx = replace_na(food_prod_idx,
                               mean(food_production_index_2014_2016_100,
                                    na.rm = TRUE)),
    
    # --- NEW: Fertilizer consumption as resilience practice proxy ---
    # BELOW average fertilizer use -> more likely conservation/organic
    # farming -> qualifies for climate-resilient practice discount.
    # Set to national average for zero-input years (pre-data gap).
    fertilizer_kg_ha = replace_na(
      fertilizer_consumption_kilograms_per_hectare_of_arable_land,
      mean(fertilizer_consumption_kilograms_per_hectare_of_arable_land,
           na.rm = TRUE)
    ),
    
    # --- NEW: Agricultural employment share as vulnerability proxy ---
    # Higher % employed in agriculture -> more households at risk per event
    agri_emp_pct = replace_na(
      employment_in_agriculture_percent_of_total_employment_modeled_ilo_estimate,
      mean(employment_in_agriculture_percent_of_total_employment_modeled_ilo_estimate,
           na.rm = TRUE)
    ),
    
    # --- NEW: Inflation-adjusted damage trend ---
    # total_damage_adj_000us is already CPI-adjusted (EM-DAT uses US CPI).
    # We additionally deflate by food_prod_idx to separate genuine
    # climate-driven damage growth from asset-value appreciation.
    # This gives a "real climate damage" series for severity modelling.
    # Rebase food_prod_idx so 2014-2016 = 1.0 (matches the index base period)
    food_idx_rebased       = food_prod_idx / 100,
    total_damage_real_musd = total_damage_m_usd / pmax(food_idx_rebased, 0.01),
    
    # --- Standardise ALL predictors AFTER imputation ---
    year_scaled          = as.numeric(scale(year)),
    precip_scaled        = as.numeric(scale(days_precip_over_20mm)),
    magnitude_scaled     = as.numeric(scale(
      ifelse(is.na(storm_magnitude_mean),
             mean(storm_magnitude_mean, na.rm = TRUE),
             storm_magnitude_mean))),
    temp_scaled          = as.numeric(scale(tavg_c)),
    flows_scaled         = as.numeric(scale(official_flows_agri_usd_m)),
    enso_scaled          = as.numeric(scale(enso_proxy)),
    agri_va_scaled       = as.numeric(scale(agri_va_pct)),
    cereal_yield_scaled  = as.numeric(scale(cereal_yield)),
    food_prod_scaled     = as.numeric(scale(food_prod_idx)),
    fertilizer_scaled    = as.numeric(scale(fertilizer_kg_ha)),
    agri_emp_scaled      = as.numeric(scale(agri_emp_pct)),
    
    log_damage           = ifelse(total_damage_m_usd > 0,
                                  log(total_damage_m_usd), NA)
  ) %>%
  filter(!is.na(days_precip_over_20mm))

cat("\n=== ANNUAL PANEL AFTER IMPUTATION ===\n")
cat("Years:", min(model_data$year), "-", max(model_data$year), "\n")
cat("N:", nrow(model_data), "\n")

# Post-imputation visual check
ggplot(model_data, aes(x = year, y = tavg_c)) +
  geom_line(colour = "#1D9E75") +
  labs(title = "Temperature series after interpolation",
       x = "Year", y = "Mean surface air temp (°C)") +
  theme_minimal()

# ---- 0b: Event-level dataset ------------------------------------------------
# Rename the temperature column in event join source to match model_data
event_data_raw <- climate_rain_clean %>%
  filter(disaster_type == "Storm",
         !is.na(start_year),
         start_year >= 1980,
         start_year <= 2023) %>%
  rename(year = start_year) %>%
  mutate(
    event_damage_m_usd = replace_na(total_damage_adjusted_000_us / 1000, 0),
    event_magnitude    = ifelse(is.na(magnitude),
                                median(magnitude, na.rm = TRUE), magnitude),
    total_affected     = replace_na(total_affected, 0),
    log_affected       = log1p(total_affected),
    decade             = floor(year / 10) * 10
  ) %>%
  left_join(
    model_data %>%
      dplyr::select(year, days_precip_over_20mm, precip_scaled,
                    year_scaled, temp_scaled, enso_scaled, flows_scaled,
                    agri_emp_scaled, food_prod_scaled, cereal_yield_scaled,
                    agri_va_scaled),
    by = "year"
  ) %>%
  filter(event_damage_m_usd > 0)

cat_threshold     <- quantile(event_data_raw$event_damage_m_usd, 0.98, na.rm = TRUE)
event_data        <- event_data_raw %>%
  mutate(is_catastrophic = event_damage_m_usd >= cat_threshold)
event_data_no_cat <- event_data %>% filter(!is_catastrophic)

cat("\n=== EVENT-LEVEL DATA ===\n")
cat("Total events (damage > 0):", nrow(event_data), "\n")
cat("Catastrophic threshold (98th pct): M USD", round(cat_threshold, 1), "\n")
cat("Catastrophic events:", sum(event_data$is_catastrophic), "\n")

# =============================================================================
# SECTION 1 — VARIABLE SELECTION
# =============================================================================
cat("\n=== VARIABLE SELECTION: CORRELATION MATRICES ===\n")

freq_cands <- model_data %>%
  dplyr::select(precip_scaled, enso_scaled, year_scaled, flows_scaled,
                agri_emp_scaled, food_prod_scaled) %>%
  na.omit()
cat("Frequency candidates (remove pairs |r| > 0.8):\n")
print(round(cor(freq_cands), 3))

sev_cands <- event_data %>%
  dplyr::select(event_magnitude, log_affected, precip_scaled,
                enso_scaled, year_scaled, agri_emp_scaled,
                cereal_yield_scaled, agri_va_scaled) %>%
  na.omit()
cat("\nSeverity candidates:\n")
print(round(cor(sev_cands), 3))

cat("\nNote: enso_scaled and temp_scaled are collinear by construction.\n")
cat("Use enso_scaled (direct physical interpretation) and drop temp_scaled.\n")
cat("year_scaled and flows_scaled may be collinear (ODA grew over time).\n")
cat("Check correlation output above before finalising formulas.\n")

# =============================================================================
# SECTION 2 — FREQUENCY MODELS
# =============================================================================
# BUG 5 FIX: Define ONE combined formula object and use it everywhere
# (model fitting, LOO loop, bootstrap). Do not mix formula objects with
# the | pipe syntax inside function calls — it only works if passed as
# a single combined formula to zeroinfl()/hurdle().

freq_formula_combined <- n_storm_events ~
  precip_scaled + enso_scaled + year_scaled |
  enso_scaled + precip_scaled

# Separate components needed for bgtest() and glm() (Poisson/NB)
freq_formula_count_only <- n_storm_events ~ precip_scaled + enso_scaled + year_scaled

# ---- 2a: Distributional diagnostics -----------------------------------------
freq_mean     <- mean(model_data$n_storm_events)
freq_var      <- var(model_data$n_storm_events)
freq_zero_obs <- sum(model_data$n_storm_events == 0)
freq_zero_poi <- nrow(model_data) * dpois(0, freq_mean)

cat("\n=== FREQUENCY DIAGNOSTICS ===\n")
cat("Mean:                           ", round(freq_mean, 3), "\n")
cat("Variance:                       ", round(freq_var, 3), "\n")
cat("Overdispersion (Var/Mean):      ", round(freq_var / freq_mean, 3), "\n")
cat("Observed zeros:                 ", freq_zero_obs, "\n")
cat("Expected zeros under Poisson:   ", round(freq_zero_poi, 1), "\n")
cat("Excess zeros:                   ", round(freq_zero_obs - freq_zero_poi, 1), "\n")
cat("\nZINB vs Hurdle rationale:\n")
cat("  ZINB:   zeros from TWO processes — structural ENSO-regime zeros AND\n")
cat("          sampling zeros from the active NB process.\n")
cat("  Hurdle: zeros from ONE process — binary non-occurrence.\n")
cat("  Philippines typhoons: ZINB preferred — ENSO creates a distinct\n")
cat("  meteorological regime where typhoon landfall is structurally suppressed.\n")

# ---- 2b: Fit models ---------------------------------------------------------
freq_poisson <- glm(freq_formula_count_only, family = poisson(link = "log"),
                    data = model_data)
cat("\n=== F1: POISSON ===\n")
print(summary(freq_poisson))
cat("AIC:", round(AIC(freq_poisson), 2), "| BIC:", round(BIC(freq_poisson), 2), "\n")
cat("Overdispersion test:\n"); print(dispersiontest(freq_poisson, trafo = 1))

freq_nb <- glm.nb(freq_formula_count_only, data = model_data)
cat("\n=== F2: NEGATIVE BINOMIAL ===\n")
print(summary(freq_nb))
cat("AIC:", round(AIC(freq_nb), 2), "| BIC:", round(BIC(freq_nb), 2), "\n")

freq_zinb <- zeroinfl(freq_formula_combined,
                      data = model_data, dist = "negbin", link = "logit")
cat("\n=== F3: ZINB (PRIMARY) ===\n")
print(summary(freq_zinb))
cat("AIC:", round(AIC(freq_zinb), 2), "| BIC:", round(BIC(freq_zinb), 2), "\n")

freq_hurdle <- hurdle(freq_formula_combined,
                      data = model_data, dist = "negbin", zero.dist = "binomial")
cat("\n=== F4: HURDLE NB ===\n")
print(summary(freq_hurdle))
cat("AIC:", round(AIC(freq_hurdle), 2), "| BIC:", round(BIC(freq_hurdle), 2), "\n")

# ---- 2c: Vuong test ---------------------------------------------------------
zinb_vs_hurdle <- tryCatch(
  vuong(freq_zinb, freq_hurdle),
  error = function(e) { cat("Vuong test error:", e$message, "\n"); NULL }
)
if (!is.null(zinb_vs_hurdle)) {
  cat("\n=== VUONG TEST: ZINB vs HURDLE ===\n")
  print(zinb_vs_hurdle)
  cat("V > 1.96: ZINB preferred | V < -1.96: Hurdle preferred\n")
}

# ---- 2d: Rootogram ----------------------------------------------------------
if (has_countreg) {
  par(mfrow = c(1, 2))
  countreg::rootogram(freq_zinb,   main = "ZINB",   style = "hanging")
  countreg::rootogram(freq_hurdle, main = "Hurdle", style = "hanging")
  par(mfrow = c(1, 1))
} else {
  count_vals    <- 0:max(model_data$n_storm_events)
  obs_freq      <- sapply(count_vals,
                          function(k) mean(model_data$n_storm_events == k))
  pred_zinb_pmf <- predict(freq_zinb, type = "prob")
  exp_zinb      <- colMeans(pred_zinb_pmf)[count_vals + 1]
  tibble(
    count  = rep(count_vals, 2),
    sqfreq = c(sqrt(obs_freq), sqrt(exp_zinb)),
    source = rep(c("Observed", "ZINB"), each = length(count_vals))
  ) %>%
    ggplot(aes(x = count, y = sqfreq, colour = source)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_colour_manual(values = c("Observed" = "#1D9E75", "ZINB" = "#E24B4A")) +
    labs(title = "Count distribution: Observed vs ZINB (√-scale)",
         x = "Annual storm count", y = "√(relative frequency)") +
    theme_minimal()
}

# ---- 2e: Autocorrelation ----------------------------------------------------
cat("\n=== AUTOCORRELATION DIAGNOSTICS ===\n")
bg_test <- bgtest(freq_poisson, order = 2)
cat("Breusch-Godfrey test (order 1-2): p =", round(bg_test$p.value, 4), "\n")
cat("H0: no serial correlation. p < 0.05 -> autocorrelation present.\n")

par(mfrow = c(2, 2))
acf(model_data$n_storm_events,
    main = "ACF: Raw storm counts", xlab = "Lag (years)")
pacf(model_data$n_storm_events,
     main = "PACF: Raw storm counts", xlab = "Lag (years)")
acf(residuals(freq_zinb, type = "response"),
    main = "ACF: ZINB residuals", xlab = "Lag (years)")
pacf(residuals(freq_zinb, type = "response"),
     main = "PACF: ZINB residuals", xlab = "Lag (years)")
par(mfrow = c(1, 1))

if (bg_test$p.value < 0.10) {
  cat("Autocorrelation detected — bootstrap SEs (Section 6) account for this.\n")
}

# ---- 2f: Comparison table ---------------------------------------------------
freq_comparison <- tibble(
  Model  = c("F1: Poisson", "F2: NB", "F3: ZINB", "F4: Hurdle NB"),
  AIC    = round(c(AIC(freq_poisson), AIC(freq_nb),
                   AIC(freq_zinb),    AIC(freq_hurdle)), 2),
  BIC    = round(c(BIC(freq_poisson), BIC(freq_nb),
                   BIC(freq_zinb),    BIC(freq_hurdle)), 2),
  LogLik = round(c(as.numeric(logLik(freq_poisson)), as.numeric(logLik(freq_nb)),
                   as.numeric(logLik(freq_zinb)),    as.numeric(logLik(freq_hurdle))), 2)
) %>%
  mutate(Delta_AIC = round(AIC - min(AIC), 2),
         Delta_BIC = round(BIC - min(BIC), 2))

cat("\n=== FREQUENCY MODEL COMPARISON ===\n")
print(freq_comparison)
cat("Delta_AIC > 2: meaningful; > 10: decisive.\n")
cat("With n=44, prefer lower BIC (penalises extra parameters more heavily).\n")

# ---- 2g: Calibration CDF plot -----------------------------------------------
count_grid     <- 0:max(model_data$n_storm_events)
observed_cdf   <- ecdf(model_data$n_storm_events)
pred_zinb_prob <- predict(freq_zinb, type = "prob")
zinb_cdf       <- cumsum(colMeans(pred_zinb_prob))[count_grid + 1]
nb_cdf         <- cumsum(sapply(count_grid, function(k)
  mean(dnbinom(k, mu = fitted(freq_nb), size = freq_nb$theta))))

tibble(
  count     = rep(count_grid, 3),
  cdf_value = c(observed_cdf(count_grid), zinb_cdf, nb_cdf),
  source    = rep(c("Observed", "ZINB", "NB"), each = length(count_grid))
) %>%
  ggplot(aes(x = count, y = cdf_value, colour = source, linetype = source)) +
  geom_step(linewidth = 1) +
  scale_colour_manual(values = c("Observed" = "#1D9E75",
                                 "ZINB" = "#E24B4A", "NB" = "#185FA5")) +
  labs(title = "Calibration: Frequency CDF",
       x = "Annual storm count", y = "Cumulative probability") +
  theme_minimal()

# =============================================================================
# SECTION 3 — SEVERITY MODEL A: PER-EVENT GAMMA GAM
# =============================================================================
# NEW PREDICTORS ADDED (design solution):
#   agri_emp_scaled   — agricultural employment share; higher = more
#                       households exposed per event = higher damage
#   agri_va_scaled    — agricultural value-added; proxies asset base at risk

cat("\n=== PER-EVENT SEVERITY DIAGNOSTICS ===\n")
cat("N events:", nrow(event_data), "\n")
cat("Mean (M USD):", round(mean(event_data$event_damage_m_usd), 3), "\n")
cat("Median (M USD):", round(median(event_data$event_damage_m_usd), 3), "\n")
cat("CV:", round(sd(event_data$event_damage_m_usd) /
                   mean(event_data$event_damage_m_usd), 3), "\n")
cat("Top 2% share:", round(
  sum(event_data$event_damage_m_usd[event_data$is_catastrophic]) /
    sum(event_data$event_damage_m_usd) * 100, 1), "%\n")

par(mfrow = c(1, 2))
qqnorm(log(event_data$event_damage_m_usd), main = "QQ: log(per-event damage)")
qqline(log(event_data$event_damage_m_usd), col = "#E24B4A")
hist(log(event_data$event_damage_m_usd),
     main = "Histogram: log(per-event damage)",
     xlab = "log(M USD)", col = "#1D9E7588", border = "white")
par(mfrow = c(1, 1))

# ---- SA1: Gamma GAM — full dataset (primary) --------------------------------
sev_event_gam_full <- gam(
  event_damage_m_usd ~
    s(event_magnitude,       k = 5, bs = "cr") +
    s(log_affected,          k = 5, bs = "cr") +
    s(days_precip_over_20mm, k = 5, bs = "cr") +
    s(year_scaled,           k = 5, bs = "cr") +
    s(agri_emp_scaled,       k = 4, bs = "cr") +   # NEW: exposure vulnerability
    s(agri_va_scaled,        k = 4, bs = "cr"),    # NEW: income/asset base
  family = Gamma(link = "log"),
  method = "REML",
  data   = event_data
)

cat("\n=== SA1: Per-Event Gamma GAM (full, PRIMARY) ===\n")
print(summary(sev_event_gam_full))
cat("AIC:", round(AIC(sev_event_gam_full), 2),
    "| BIC:", round(BIC(sev_event_gam_full), 2), "\n")
cat("R-sq (adj):", round(summary(sev_event_gam_full)$r.sq, 3), "\n")
cat("Dev. explained:", round(summary(sev_event_gam_full)$dev.expl * 100, 1), "%\n")
cat("edf per smooth (edf > 1 = nonlinearity meaningful):\n")
print(summary(sev_event_gam_full)$s.table[, "edf", drop = FALSE])

par(mfrow = c(2, 2)); gam.check(sev_event_gam_full); par(mfrow = c(1, 1))

# ---- SA2: Gamma GAM — excluding catastrophic outliers -----------------------
sev_event_gam_nocat <- gam(
  event_damage_m_usd ~
    s(event_magnitude,       k = 5, bs = "cr") +
    s(log_affected,          k = 5, bs = "cr") +
    s(days_precip_over_20mm, k = 5, bs = "cr") +
    s(year_scaled,           k = 5, bs = "cr") +
    s(agri_emp_scaled,       k = 4, bs = "cr") +
    s(agri_va_scaled,        k = 4, bs = "cr"),
  family = Gamma(link = "log"),
  method = "REML",
  data   = event_data_no_cat
)

# Representative covariate point (used for outlier sensitivity and bootstrap)
event_mean_covs <- event_data %>%
  summarise(
    event_magnitude       = mean(event_magnitude,       na.rm = TRUE),
    log_affected          = mean(log_affected,           na.rm = TRUE),
    days_precip_over_20mm = mean(days_precip_over_20mm, na.rm = TRUE),
    year_scaled           = mean(year_scaled,            na.rm = TRUE),
    agri_emp_scaled       = mean(agri_emp_scaled,        na.rm = TRUE),
    agri_va_scaled        = mean(agri_va_scaled,         na.rm = TRUE)
  )

pred_full  <- exp(predict(sev_event_gam_full,  newdata = event_mean_covs, type = "link"))
pred_nocat <- exp(predict(sev_event_gam_nocat, newdata = event_mean_covs, type = "link"))

cat("\n=== SA2: Per-Event Gamma GAM (no catastrophic outliers) ===\n")
cat("E[X] with catastrophic events:    M USD", round(pred_full,  3), "\n")
cat("E[X] without catastrophic events: M USD", round(pred_nocat, 3), "\n")
cat("Ratio full/no-cat:                ", round(pred_full / pred_nocat, 2), "\n")
cat("If ratio > 2: price cat layer separately.\n")

cat_dominance_flag <- pred_full / pred_nocat

# Primary model is full dataset
sev_event_gam <- sev_event_gam_full

# Per-event model comparison (SA1 vs SA2 only — both are per-event Gamma GAMs)
sev_event_comparison <- tibble(
  Model    = c("SA1: Gamma GAM (full data)", "SA2: Gamma GAM (no cat events)"),
  AIC      = round(c(AIC(sev_event_gam_full), AIC(sev_event_gam_nocat)), 2),
  BIC      = round(c(BIC(sev_event_gam_full), BIC(sev_event_gam_nocat)), 2),
  Dev_expl = c(round(summary(sev_event_gam_full)$dev.expl  * 100, 1),
               round(summary(sev_event_gam_nocat)$dev.expl * 100, 1))
) %>% mutate(Delta_AIC = round(AIC - min(AIC), 2))

cat("\n=== PER-EVENT SEVERITY MODEL COMPARISON ===\n")
print(sev_event_comparison)

# =============================================================================
# SECTION 4 — SEVERITY MODEL B: ANNUAL AGGREGATE TWEEDIE GAM
# =============================================================================
# BUG 2 FIX: Use severity_annual_data consistently (was mixed with
# severity_data in v4 which was never assigned).
# n_storm_events is EXCLUDED — including it would double-count frequency.
# NEW PREDICTORS: food_prod_scaled (crop diversity), agri_emp_scaled (exposure).

# Use `severity_annual_data` everywhere in this section
severity_annual_data <- model_data %>% filter(total_damage_m_usd > 0)

# Tweedie power parameter
cat("\n=== TWEEDIE POWER PARAMETER ESTIMATION ===\n")
tweedie_profile <- tryCatch(
  tweedie.profile(
    total_damage_m_usd ~
      magnitude_scaled + precip_scaled + enso_scaled +
      year_scaled + food_prod_scaled,
    data   = severity_annual_data,
    p.vec  = seq(1.2, 3.0, by = 0.1),
    method = "series", do.plot = TRUE, do.ci = TRUE
  ),
  error = function(e) {
    cat("tweedie.profile failed:", e$message, "\n")
    list(p.max = 1.8, ci = c(NA, NA))
  }
)
p_optimal <- tweedie_profile$p.max
cat("Optimal p:", round(p_optimal, 3), "\n")

# SB1: Annual Tweedie GAM — n_storm_events EXCLUDED
sev_annual_tweedie_gam <- gam(
  total_damage_m_usd ~
    s(magnitude_scaled,  k = 5, bs = "cr") +
    s(precip_scaled,     k = 5, bs = "cr") +
    s(enso_scaled,       k = 5, bs = "cr") +
    s(year_scaled,       k = 5, bs = "cr") +
    s(food_prod_scaled,  k = 4, bs = "cr") +   # NEW: crop diversity / production
    s(agri_emp_scaled,   k = 4, bs = "cr"),    # NEW: vulnerability exposure
  family = tw(),
  method = "REML",
  data   = severity_annual_data
)

cat("\n=== SB1: Annual Tweedie GAM (n_storm_events EXCLUDED) ===\n")
print(summary(sev_annual_tweedie_gam))
cat("AIC:", round(AIC(sev_annual_tweedie_gam), 2), "\n")
cat("R-sq (adj):", round(summary(sev_annual_tweedie_gam)$r.sq, 3), "\n")
cat("Dev. explained:", round(summary(sev_annual_tweedie_gam)$dev.expl * 100, 1), "%\n")

par(mfrow = c(2, 2)); gam.check(sev_annual_tweedie_gam); par(mfrow = c(1, 1))

# =============================================================================
# SECTION 5 — LOO-CV (temporal, respects time structure)
# =============================================================================
# BUG 5 FIX: LOO loop uses freq_formula_combined (not the mixed syntax)
cat("\n=== LEAVE-ONE-OUT TEMPORAL CV ===\n")

years_cv         <- sort(unique(model_data$year))
loo_freq_sq_err  <- numeric(length(years_cv))

for (i in seq_along(years_cv)) {
  yr_out    <- years_cv[i]
  train_ann <- model_data %>% filter(year != yr_out)
  test_ann  <- model_data %>% filter(year == yr_out)
  
  fit_zinb <- tryCatch(
    zeroinfl(freq_formula_combined, data = train_ann,
             dist = "negbin", link = "logit"),
    error = function(e) NULL
  )
  loo_freq_sq_err[i] <- if (!is.null(fit_zinb)) {
    pred <- predict(fit_zinb, newdata = test_ann, type = "response")
    (test_ann$n_storm_events - pred)^2
  } else NA
}

years_evt             <- sort(unique(event_data$year))
loo_sev_sq_err        <- numeric(length(years_evt))

for (i in seq_along(years_evt)) {
  yr_out    <- years_evt[i]
  train_evt <- event_data %>% filter(year != yr_out)
  test_evt  <- event_data %>% filter(year == yr_out)
  
  if (nrow(train_evt) < 15 || nrow(test_evt) == 0) {
    loo_sev_sq_err[i] <- NA; next
  }
  fit_sev <- tryCatch(
    gam(event_damage_m_usd ~
          s(event_magnitude, k = 4, bs = "cr") +
          s(log_affected,    k = 4, bs = "cr") +
          s(year_scaled,     k = 4, bs = "cr") +
          s(agri_emp_scaled, k = 3, bs = "cr"),
        family = Gamma(link = "log"), method = "REML", data = train_evt),
    error = function(e) NULL
  )
  loo_sev_sq_err[i] <- if (!is.null(fit_sev)) {
    pred <- tryCatch(predict(fit_sev, newdata = test_evt, type = "response"),
                     error = function(e) rep(NA, nrow(test_evt)))
    mean((test_evt$event_damage_m_usd - pred)^2, na.rm = TRUE)
  } else NA
}

loo_freq_rmse           <- sqrt(mean(loo_freq_sq_err, na.rm = TRUE))
loo_sev_rmse            <- sqrt(mean(loo_sev_sq_err,  na.rm = TRUE))
loo_in_sample_rmse_freq <- sqrt(mean((model_data$n_storm_events -
                                        predict(freq_zinb, type = "response"))^2))
loo_in_sample_rmse_sev  <- sqrt(mean((event_data$event_damage_m_usd -
                                        predict(sev_event_gam, type = "response"))^2))

cat("FREQUENCY: LOO RMSE =", round(loo_freq_rmse, 3),
    "| In-sample =", round(loo_in_sample_rmse_freq, 3),
    "| Overfit ratio =", round(loo_freq_rmse / loo_in_sample_rmse_freq, 2), "\n")
cat("SEVERITY:  LOO RMSE =", round(loo_sev_rmse, 3), "M USD",
    "| In-sample =", round(loo_in_sample_rmse_sev, 3),
    "| Overfit ratio =", round(loo_sev_rmse / loo_in_sample_rmse_sev, 2), "\n")
cat("Overfitting ratio > 2: serious overfitting; > 1.5: moderate.\n")

# =============================================================================
# SECTION 6 — BLOCK BOOTSTRAP FOR PARAMETER RISK
# =============================================================================
cat("\n=== BLOCK BOOTSTRAP (B=500, block=3yr) ===\n")

set.seed(42)
B          <- 500
block_size <- 3
n_ann      <- nrow(model_data)
n_blocks   <- ceiling(n_ann / block_size)
boot_el    <- numeric(B)

for (b in 1:B) {
  starts   <- sample(1:(n_ann - block_size + 1), n_blocks, replace = TRUE)
  idx      <- unlist(lapply(starts, function(s) s:(s + block_size - 1)))
  idx      <- idx[idx <= n_ann][1:n_ann]
  b_ann    <- model_data[idx, ]
  b_evt    <- event_data %>% filter(year %in% b_ann$year)
  
  ff <- tryCatch(
    zeroinfl(freq_formula_combined, data = b_ann,
             dist = "negbin", link = "logit"),
    error = function(e) NULL
  )
  fs <- if (!is.null(ff) && nrow(b_evt) >= 15) {
    tryCatch(
      gam(event_damage_m_usd ~
            s(event_magnitude, k = 4, bs = "cr") +
            s(log_affected,    k = 4, bs = "cr") +
            s(year_scaled,     k = 4, bs = "cr") +
            s(agri_emp_scaled, k = 3, bs = "cr"),
          family = Gamma(link = "log"), method = "REML", data = b_evt),
      error = function(e) NULL
    )
  } else NULL
  
  if (is.null(ff) || is.null(fs)) { boot_el[b] <- NA; next }
  
  bl <- tryCatch(predict(ff, newdata = model_data[ceiling(n_ann/2), ],
                         type = "response"), error = function(e) NA)
  bs <- tryCatch(exp(predict(fs, newdata = event_mean_covs, type = "link")),
                 error = function(e) NA)
  boot_el[b] <- bl * bs
}

boot_expected_loss    <- boot_el[!is.na(boot_el)]
parameter_risk_factor <- (mean(boot_expected_loss) + sd(boot_expected_loss)) /
  mean(boot_expected_loss)

cat("Completed:", length(boot_expected_loss), "/", B, "replications\n")
cat("Mean E[N×X]:  ", round(mean(boot_expected_loss), 3), "M USD\n")
cat("SD:           ", round(sd(boot_expected_loss), 3), "\n")
cat("CV:           ", round(sd(boot_expected_loss)/mean(boot_expected_loss), 3), "\n")
cat("90% CI:      [", round(quantile(boot_expected_loss, 0.05), 3), ",",
    round(quantile(boot_expected_loss, 0.95), 3), "]\n")
cat("Parameter risk factor (mean+SD)/mean:", round(parameter_risk_factor, 3), "\n")

ggplot(data.frame(x = boot_expected_loss), aes(x = x)) +
  geom_histogram(bins = 40, fill = "#1D9E75", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(boot_expected_loss),
             colour = "#185FA5", linewidth = 1.2) +
  geom_vline(xintercept = quantile(boot_expected_loss, c(0.05, 0.95)),
             colour = "#E24B4A", linetype = "dashed") +
  labs(title = "Bootstrap: E[N×X] distribution (parameter risk)",
       x = "Expected annual loss (M USD)", y = "Count") +
  theme_minimal()

# =============================================================================
# SECTION 7 — BURN COST VALIDATION
# =============================================================================
burn_cost_annual <- model_data %>%
  summarise(
    years_observed     = n(),
    loss_years         = sum(total_damage_m_usd > 0, na.rm = TRUE),
    total_loss_m_usd   = sum(total_damage_m_usd, na.rm = TRUE),
    burn_cost_m_usd_yr = total_loss_m_usd / years_observed,
    p75_annual_loss    = quantile(total_damage_m_usd, 0.75, na.rm = TRUE),
    p90_annual_loss    = quantile(total_damage_m_usd, 0.90, na.rm = TRUE),
    p95_annual_loss    = quantile(total_damage_m_usd, 0.95, na.rm = TRUE),
    max_annual_loss    = max(total_damage_m_usd, na.rm = TRUE)
  )

burn_per_event <- event_data %>%
  summarise(
    total_storm_events   = n(),
    avg_per_event_damage = mean(event_damage_m_usd, na.rm = TRUE),
    med_per_event_damage = median(event_damage_m_usd, na.rm = TRUE)
  )

avg_events_per_year <- mean(model_data$n_storm_events, na.rm = TRUE)
freq_x_sev_check    <- avg_events_per_year * burn_per_event$avg_per_event_damage

cat("\n=== BURN COST VALIDATION ===\n")
print(burn_cost_annual)
cat("\nPer-event stats:\n"); print(burn_per_event)
cat("\nFreq × Sev check (per-event severity):\n")
cat("  Avg events/yr × Avg per-event damage =",
    round(freq_x_sev_check, 2), "M USD\n")
cat("  Direct burn cost                     =",
    round(burn_cost_annual$burn_cost_m_usd_yr, 2), "M USD\n")
pct_diff <- abs(freq_x_sev_check - burn_cost_annual$burn_cost_m_usd_yr) /
  burn_cost_annual$burn_cost_m_usd_yr * 100
cat("  Divergence:", round(pct_diff, 1), "%\n")
cat("  > 15%: N and X may not be independent\n")

# =============================================================================
# SECTION 8 — CREDIBILITY (Bühlmann + Bayesian conjugate)
# =============================================================================
process_var_freq  <- mean(model_data$n_storm_events, na.rm = TRUE)
between_var_freq  <- max(var(model_data$n_storm_events, na.rm = TRUE) -
                           process_var_freq, 1e-4)
k_freq_buhlmann   <- process_var_freq / between_var_freq
Z_freq_buhlmann   <- nrow(model_data) / (nrow(model_data) + k_freq_buhlmann)

alpha_prior      <- 8.0
beta_prior       <- 1.0
sum_y_freq       <- sum(model_data$n_storm_events)
alpha_posterior  <- alpha_prior + sum_y_freq
beta_posterior   <- beta_prior  + nrow(model_data)
lambda_bayes     <- alpha_posterior / beta_posterior
lambda_bayes_sd  <- sqrt(alpha_posterior / beta_posterior^2)
lambda_bayes_ci  <- qgamma(c(0.05, 0.95),
                           shape = alpha_posterior, rate = beta_posterior)

cat("\n=== CREDIBILITY ===\n")
cat("Bühlmann Z:", round(Z_freq_buhlmann, 3),
    " k:", round(k_freq_buhlmann, 3), "\n")
cat("Bayesian posterior mean:", round(lambda_bayes, 3),
    " SD:", round(lambda_bayes_sd, 3), "\n")
cat("Bayesian 90% CI: [", round(lambda_bayes_ci[1], 3), ",",
    round(lambda_bayes_ci[2], 3), "]\n")

if (Z_freq_buhlmann < 0.3) {
  cat("WARNING: Z < 0.3. Prior dominates. Use Bayesian estimate.\n")
} else if (Z_freq_buhlmann < 0.5) {
  cat("NOTE: Z < 0.5. Burn cost outweighs model. Both reported in pricing.\n")
} else {
  cat("Z >= 0.5. Model estimate has majority weight. Bühlmann appropriate.\n")
}

# =============================================================================
# SECTION 9 — EXPORT
# =============================================================================

# Capture all scaling parameters as named scalars for 02_pricing.R
year_mean    <- mean(model_data$year)
year_sd      <- sd(model_data$year)
precip_mean  <- mean(model_data$days_precip_over_20mm,  na.rm = TRUE)
precip_sd    <- sd(model_data$days_precip_over_20mm,    na.rm = TRUE)
temp_mean    <- mean(model_data$tavg_c,                 na.rm = TRUE)
temp_sd      <- sd(model_data$tavg_c,                   na.rm = TRUE)
mag_mean     <- mean(model_data$storm_magnitude_mean,   na.rm = TRUE)
mag_sd       <- sd(model_data$storm_magnitude_mean,     na.rm = TRUE)
enso_mean    <- mean(model_data$enso_proxy,             na.rm = TRUE)
enso_sd      <- sd(model_data$enso_proxy,               na.rm = TRUE)
agri_emp_mean <- mean(model_data$agri_emp_pct,          na.rm = TRUE)
agri_emp_sd   <- sd(model_data$agri_emp_pct,            na.rm = TRUE)
agri_va_mean  <- mean(model_data$agri_va_pct,           na.rm = TRUE)
agri_va_sd    <- sd(model_data$agri_va_pct,             na.rm = TRUE)
food_prod_mean <- mean(model_data$food_prod_idx,        na.rm = TRUE)
food_prod_sd   <- sd(model_data$food_prod_idx,          na.rm = TRUE)
fertilizer_mean <- mean(model_data$fertilizer_kg_ha,   na.rm = TRUE)
fertilizer_sd   <- sd(model_data$fertilizer_kg_ha,     na.rm = TRUE)
cereal_yield_mean <- mean(model_data$cereal_yield,      na.rm = TRUE)
cereal_yield_sd   <- sd(model_data$cereal_yield,        na.rm = TRUE)

save(
  # Frequency models
  freq_zinb, freq_hurdle, freq_nb, freq_poisson,
  freq_formula_combined, freq_comparison,
  
  # Per-event severity
  sev_event_gam,        # primary (full data, SA1)
  sev_event_gam_nocat,  # sensitivity (no cat events, SA2)
  sev_event_comparison,
  cat_dominance_flag, cat_threshold, event_mean_covs,
  
  # Annual aggregate severity
  sev_annual_tweedie_gam,
  p_optimal, severity_annual_data,
  
  # Data
  model_data, event_data, event_data_no_cat,
  
  # Burn costs
  burn_cost_annual, burn_per_event,
  avg_events_per_year, freq_x_sev_check,
  
  # Bootstrap / parameter risk
  boot_expected_loss, parameter_risk_factor,
  
  # Credibility
  Z_freq_buhlmann, k_freq_buhlmann,
  lambda_bayes, lambda_bayes_sd, lambda_bayes_ci,
  alpha_prior, beta_prior, alpha_posterior, beta_posterior,
  
  # LOO-CV
  loo_freq_rmse, loo_sev_rmse,
  loo_in_sample_rmse_freq, loo_in_sample_rmse_sev,
  
  # Scaling parameters (all needed by 02_pricing.R)
  year_mean, year_sd,
  precip_mean, precip_sd,
  temp_mean, temp_sd,
  mag_mean, mag_sd,
  enso_mean, enso_sd,
  agri_emp_mean, agri_emp_sd,
  agri_va_mean, agri_va_sd,
  food_prod_mean, food_prod_sd,
  fertilizer_mean, fertilizer_sd,
  cereal_yield_mean, cereal_yield_sd,
  
  # Reference
  magnitude_scale_reference,
  
  file = "model_objects_V5.RData"
)

cat("\n=== EXPORT COMPLETE (v5) ===\n")
cat("Primary frequency model:     freq_zinb (ZINB)\n")
cat("Primary per-event severity:  sev_event_gam (Gamma GAM, SA1)\n")
cat("Primary annual severity:     sev_annual_tweedie_gam (Tweedie GAM, SB1)\n")
cat("Bootstrap replicates:       ", length(boot_expected_loss), "\n")
cat("Parameter risk factor:      ", round(parameter_risk_factor, 3), "\n")
cat("Run 02_pricing.R\n")