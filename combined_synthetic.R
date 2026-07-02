library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(janitor)
library(readr)

# Import datasets
agri_rural_raw <- read_excel("Data/Agriculture & Rural Development.xls")

#Data Clean
colnames(agri_rural_raw) <- as.character(unlist(agri_rural_raw[3, ]))
agri_rural_clean <- agri_rural_raw[-c(1:3), ]
agri_rural_clean <- agri_rural_clean %>%
  janitor::clean_names()

agri_philippines <- agri_rural_clean %>%
  filter(country_name == "Philippines")

climate_rain <- read_excel("Data/Natural disasters - climate and rainfall.xlsx")

#Data clean
climate_rain_philippines <- climate_rain %>%
  filter(Country == "Philippines")

temp_series <- read.csv("Data/Observed Weather Time Series.csv")

precip_20mm <- read.csv("Days_with_Precipitation_over_20mm.csv")
precip_20mm_philippines <- precip_20mm %>%
  filter(REF_AREA_LABEL == "Philippines")


agri_govt_exp <- read.csv("The agriculture orientation index for government expenditures.csv")
agri_govt_exp_phl <- agri_govt_exp %>%
  filter(REF_AREA_LABEL == "Philippines")

official_flows_agri <- read.csv("Total official flows official development assistance plus other official flows to the agriculture sector 2000-2023 Millions US dollars 2022 constant prices.csv")
official_flows_agri_phl <- official_flows_agri %>%
  filter(REF_AREA_LABEL == "Philippines")

###############################################################################
# =============================================================
# STEP 0b — INSPECT BEFORE RESHAPING (run this, read the output,
# THEN proceed to Step 1). The reshape code below is written for
# the standard shape these column names imply, but you must confirm.
# =============================================================

glimpse(agri_philippines)             # expect: country_name, indicator_name, x1960...x2025
glimpse(climate_rain_philippines)     # expect: one row PER DISASTER EVENT, not per year
glimpse(temp_series)                  # need to see this to confirm row/col layout
glimpse(precip_20mm_philippines)      # expect: REF_AREA_LABEL, TIME_PERIOD, OBS_VALUE (long, SDMX-style)
glimpse(agri_govt_exp_phl)            # same SDMX-style shape expected
glimpse(official_flows_agri_phl)      # same SDMX-style shape expected

# =============================================================
# STEP 1 — RESHAPE EACH DATASET TO: year | <value column(s)>
# =============================================================

## --- 1a. Agriculture & Rural Development (World Bank wide shape) ---
## agri_philippines has columns: country_name, country_code, indicator_name,
## indicator_code, then one column PER YEAR (e.g. x1960, x1961, ..., x2025).
## Pivot years to long, then pivot indicators to wide so each indicator
## becomes its own column with one row per year.

agri_rural_long <- agri_philippines %>%
  dplyr::select(indicator_name, matches("^x[12][0-9]{3}$")) %>%   # year columns only
  pivot_longer(
    cols = -indicator_name,
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.integer(str_remove(year, "^x")),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(year, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%  # dedupe if needed
  pivot_wider(names_from = indicator_name, values_from = value) %>%
  clean_names()

## --- 1b. Climate & rainfall (EM-DAT-style EVENT-LEVEL data) ---
## climate_rain_philippines is event-level: one row per disaster, with
## fields like Disaster Type, Start Year, Total Damage, Total Affected,
## Magnitude, Magnitude Scale, etc. This is NOT one-row-per-year, so it
## must be AGGREGATED to annual before it can join the rest of the panel.
## Adjust the exact column names below once glimpse() output confirms
## them (likely "Start Year" -> start_year after clean_names()).
climate_rain_clean <- climate_rain_philippines %>%
  clean_names()

## IMPORTANT - Magnitude needs care before aggregating, for two reasons:
## 1. The UNIT depends on Magnitude Scale, which varies by Disaster Type
##    (e.g. storms are often recorded in km/h wind speed, floods in
##    water depth, droughts frequently have no magnitude recorded at
##    all). Averaging magnitude across types mixes incompatible units
##    into a meaningless number - so we average WITHIN each type only.
## Solution: average magnitude PER DISASTER TYPE, so units stay
## comparable within each type.

## Check what magnitude scales actually appear, so you know which units
## you're working with before trusting any average:
climate_rain_clean %>%
  distinct(disaster_type, magnitude_scale) %>%
  arrange(disaster_type)

climate_rain_annual <- climate_rain_clean %>%
  filter(!is.na(start_year)) %>%
  group_by(year = start_year) %>%
  summarise(
    n_disaster_events           = n(),
    n_drought_events            = sum(disaster_type == "Drought", na.rm = TRUE),
    n_flood_events               = sum(disaster_type == "Flood", na.rm = TRUE),
    n_storm_events                = sum(disaster_type == "Storm", na.rm = TRUE),
    total_deaths                   = sum(total_deaths, na.rm = TRUE),
    total_affected                 = sum(total_affected, na.rm = TRUE),
    total_damage_adj_000us         = sum(total_damage_adjusted_000_us, na.rm = TRUE),
    insured_damage_adj_000us       = sum(insured_damage_adjusted_000_us, na.rm = TRUE),
    reconstruction_costs_adj_000us = sum(reconstruction_costs_adjusted_000_us, na.rm = TRUE),
    # Average magnitude split by disaster type, since units differ
    # across types (storm wind speed vs flood depth vs drought, which
    # is frequently unmeasured by magnitude in EM-DAT)
    storm_magnitude_mean   = mean(magnitude[disaster_type == "Storm"], na.rm = TRUE),
    flood_magnitude_mean   = mean(magnitude[disaster_type == "Flood"], na.rm = TRUE),
    drought_magnitude_mean = mean(magnitude[disaster_type == "Drought"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(year),
    # mean() on an empty filtered vector (no events of that type that
    # year) returns NaN, not NA - clean up so joins/models don't choke
    across(
      c(storm_magnitude_mean, flood_magnitude_mean, drought_magnitude_mean),
      ~ ifelse(is.nan(.x), NA, .x)
    )
  )

## Reference table: which Magnitude Scale applies to each Disaster Type,
## so you can label chart axes / report units correctly later. Keep this
## alongside the annual panel rather than folding it into the numbers.
magnitude_scale_reference <- climate_rain_clean %>%
  filter(!is.na(magnitude)) %>%
  count(disaster_type, magnitude_scale, sort = TRUE)


## --- 1c. Observed Weather Time Series ---
## UNCONFIRMED SHAPE - glimpse() first. Two likely cases:
##   CASE A: long format already (year, indicator, value columns) - if so,
##           just clean_names() and pivot_wider() on the indicator column.
##   CASE B: wide with year as column 1 and named met variables as the
##           other columns (e.g. year, tavg_c, precip_mm) - if so, this
##           is ALREADY in the right shape and needs almost no reshaping.
## Code below assumes CASE B (the more common shape for "time series" CSVs
## with year in column 1) - adjust if glimpse() shows otherwise.

temp_series_clean <- temp_series %>%
  clean_names() %>%
  rename(year = 1) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(across(everything(), ~ suppressWarnings(mean(as.numeric(.x), na.rm = TRUE))), .groups = "drop")

## --- Generic helper for World Bank Data360-style wide files: one row
## per country/indicator, a pile of metadata columns (REF_AREA_LABEL,
## INDICATOR_LABEL, UNIT_MEASURE_LABEL, etc.), then one column PER YEAR
## named X1950, X1951, ..., X2023. This is the SAME shape as the
## Agriculture & Rural Development file in Step 1a, just with extra
## metadata columns we don't need. Confirmed against the real
## precip_20mm_philippines structure - applies to agri_govt_exp_phl and
## official_flows_agri_phl too, since they share the filename/source style.

reshape_wb_wide <- function(df, value_col_name) {
  df %>%
    clean_names() %>%
    dplyr::select(matches("^x[12][0-9]{3}$")) %>%   # keep only year columns (x1950...x2023 etc)
    pivot_longer(
      cols = everything(),
      names_to = "year",
      values_to = value_col_name
    ) %>%
    mutate(
      year = as.integer(str_remove(year, "^x")),
      !!value_col_name := as.numeric(.data[[value_col_name]])
    ) %>%
    filter(!is.na(year)) %>%
    distinct(year, .keep_all = TRUE)   # guard in case the source had >1 row (e.g. multiple breakdowns)
}

## --- 1d. Days with precipitation over 20mm ---
precip_20mm_clean <- reshape_wb_wide(precip_20mm_philippines, "days_precip_over_20mm")

## --- 1e. Agriculture orientation index for government expenditure ---
agri_govt_exp_clean <- reshape_wb_wide(agri_govt_exp_phl, "agri_orientation_index")

## --- 1f. Official development flows to agriculture ---
official_flows_clean <- reshape_wb_wide(official_flows_agri_phl, "official_flows_agri_usd_m")

# =============================================================
# SYNTHETIC DATA GENERATION BLOCK
# Purpose : Fill gaps in each source dataset BEFORE the full_join
#           so that philippines_master has complete coverage from
#           1980 to 2023 (the modelling window chosen below).
#
# Competition justification (stated assumptions):
#   The competition brief explicitly permits synthetic or simulated
#   data where real data are unavailable, provided assumptions are
#   clearly stated. All synthetic values generated here are:
#     (a) derived from statistical properties of the OBSERVED data
#         (mean, variance, trend) — not hand-typed guesses
#     (b) labelled with a synthetic_flag column so they can be
#         filtered, weighted, or reported separately in any model
#     (c) accompanied by the assumption statement used to generate them
#
# Modelling window : 1980–2023
#   Chosen because:
#     - EM-DAT disaster records for the Philippines are considered
#       reasonably complete from ~1980 onward
#     - WDI agricultural indicators begin ~1961 but are sparse pre-1980
#     - Climate change acceleration in the literature is documented
#       from the 1980s, making pre-1980 climatology less representative
#       of the risk period your product covers
#
# SET THIS ONCE — all synthetic generation below respects it:
MODEL_START <- 1970L
MODEL_END   <- 2025L
FULL_YEARS  <- tibble(year = MODEL_START:MODEL_END)
set.seed(2024)   # reproducibility — state this in your appendix
# =============================================================


# =============================================================
# HELPER FUNCTIONS
# =============================================================

## add_synthetic_flag(): appends a logical column marking which rows
## are synthetic vs. observed. Use this on EVERY dataset before joining.
add_synthetic_flag <- function(df, observed_years) {
  df %>% mutate(synthetic = !(year %in% observed_years))
}

## fit_linear_trend(): fits lm(value ~ year) on observed data and
## returns predicted + noise for the full year range. noise_sd = NULL
## uses residual SD from the fitted model (recommended).
fill_linear_trend <- function(observed_df, year_col, value_col,
                              full_years = FULL_YEARS,
                              noise_sd = NULL,
                              floor_val = NULL,
                              cap_val = NULL) {
  obs <- observed_df %>%
    rename(year = !!year_col, value = !!value_col) %>%
    filter(!is.na(value))
  
  fit   <- lm(value ~ year, data = obs)
  resid_sd <- if (is.null(noise_sd)) sigma(fit) else noise_sd
  
  out <- full_years %>%
    mutate(
      pred  = predict(fit, newdata = .),
      noise = rnorm(n(), 0, resid_sd),
      value = pred + noise
    )
  
  # Apply floor/cap if supplied (e.g. counts can't go below 0)
  if (!is.null(floor_val)) out <- out %>% mutate(value = pmax(value, floor_val))
  if (!is.null(cap_val))   out <- out %>% mutate(value = pmin(value, cap_val))
  
  out %>% dplyr::select(year, value) %>%
    rename(!!value_col := value)
}


# =============================================================
# 1. TEMPERATURE & METEOROLOGICAL SERIES (temp_series_clean)
# =============================================================
# Assumption: surface air temperature follows a long-run linear
# warming trend. The trend and inter-annual variability are both
# estimated from the observed data (CASE B wide format, cleaned in
# Step 1c of main script). Synthetic values = trend prediction +
# noise drawn from N(0, observed_residual_sd).
#
# This is the standard approach in climate science for filling
# short-duration gaps in station records (WMO Guide to Climatological
# Practices, 2018 edition).

## Identify which numeric columns exist in temp_series_clean
## (the exact names depend on your file — adjust if needed)
temp_numeric_cols <- temp_series_clean %>%
  dplyr::select(-year) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

## For each meteorological variable: fit trend, fill gaps
temp_synthetic <- FULL_YEARS

for (col in temp_numeric_cols) {
  obs_temp <- temp_series_clean %>%
    dplyr::select(year, !!col) %>%
    filter(!is.na(.data[[col]]))
  
  if (nrow(obs_temp) < 5) {
    # Too few observations to fit a trend — leave as NA rather than
    # extrapolate wildly. Flag in assumption log below.
    temp_synthetic <- temp_synthetic %>% mutate(!!col := NA_real_)
    next
  }
  
  filled <- fill_linear_trend(obs_temp, "year", col,
                              floor_val = if (grepl("precip|rain|mm", col, ignore.case = TRUE)) 0 else NULL)
  temp_synthetic <- temp_synthetic %>% left_join(filled, by = "year")
}

## Preserve observed values — only fill where originally NA.
## Build the merged frame first, then coalesce column-by-column
## in a loop to avoid cur_column() / .data pronoun issues.
temp_merged <- FULL_YEARS %>%
  left_join(temp_series_clean, by = "year") %>%
  left_join(
    temp_synthetic %>% rename_with(~ paste0(.x, "_syn"), -year),
    by = "year"
  )

for (col in temp_numeric_cols) {
  syn_col <- paste0(col, "_syn")
  if (syn_col %in% names(temp_merged)) {
    temp_merged[[col]] <- dplyr::coalesce(temp_merged[[col]],
                                          temp_merged[[syn_col]])
  }
}

temp_synthetic_final <- temp_merged %>%
  dplyr::select(year, all_of(temp_numeric_cols)) %>%
  add_synthetic_flag(temp_series_clean$year)

cat("temp_synthetic_final: ", nrow(temp_synthetic_final), "rows,",
    sum(temp_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 2. PRECIPITATION OVER 20mm (precip_20mm_clean)
# =============================================================
# Assumption: annual count of days with precipitation > 20mm is
# modelled as a linear trend in the mean, with inter-annual
# variability drawn from a normal distribution with SD estimated
# from observed years. Floor at 0 (days can't be negative).
# Assumption log: "days_precip_over_20mm values for years not in
# the World Bank CCKP series are synthetic, generated by OLS trend
# extrapolation on observed 1980–2023 data."

precip_synthetic_final <- FULL_YEARS %>%
  left_join(precip_20mm_clean, by = "year") %>%
  {
    obs_years <- precip_20mm_clean$year
    obs_vals  <- precip_20mm_clean
    
    filled <- fill_linear_trend(obs_vals, "year", "days_precip_over_20mm",
                                floor_val = 0)
    
    FULL_YEARS %>%
      left_join(obs_vals,  by = "year") %>%
      left_join(filled %>% rename(days_syn = days_precip_over_20mm), by = "year") %>%
      mutate(days_precip_over_20mm = coalesce(days_precip_over_20mm, days_syn)) %>%
      dplyr::select(year, days_precip_over_20mm) %>%
      add_synthetic_flag(obs_years)
  }

cat("precip_synthetic_final: ", nrow(precip_synthetic_final), "rows,",
    sum(precip_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 3. EM-DAT DISASTER COUNTS (climate_rain_annual)
# =============================================================
# Assumption A — EVENT COUNTS (n_storm_events, n_flood_events,
#   n_drought_events): modelled as a Poisson process with a linear
#   trend in log(lambda) to reflect the documented increase in
#   climate-related disaster frequency from 1980–2023 (consistent
#   with IPCC AR6 findings on Philippine typhoon/flood frequency).
#   lambda estimated from observed years; synthetic counts drawn
#   from rpois(1, lambda_hat_for_that_year).
#
# Assumption B — DAMAGE & DEATHS (total_damage_adj_000us, total_deaths):
#   Modelled as log-normal. Log-normal is the actuarial standard for
#   property loss severity (Klugman, Panjer & Willmot, Loss Models,
#   4th ed.). Parameters fitted on observed non-zero values. Zero
#   probability preserved as the observed proportion of zero-damage years.
#
# Assumption C — INSURED DAMAGE (insured_damage_adj_000us):
#   Left as NA for synthetic years and flagged, because imputing
#   insured damage would directly contaminate the protection-gap
#   calculation which is a key output of the model.

## ---- 3a. Poisson trend model for event counts ----
fit_poisson_trend <- function(annual_df, count_col) {
  obs <- annual_df %>%
    rename(cnt = !!count_col) %>%
    filter(!is.na(cnt))
  
  if (sum(obs$cnt > 0) < 3) {
    # Not enough non-zero years — fall back to grand mean Poisson
    lambda_mean <- mean(obs$cnt, na.rm = TRUE)
    return(FULL_YEARS %>% mutate(!!count_col := rpois(n(), max(lambda_mean, 0.1))))
  }
  
  fit <- glm(cnt ~ year, data = obs, family = poisson(link = "log"))
  
  FULL_YEARS %>%
    mutate(
      lambda     = predict(fit, newdata = ., type = "response"),
      !!count_col := rpois(n(), lambda)
    ) %>%
    dplyr::select(year, !!count_col)
}

counts_storm   <- fit_poisson_trend(climate_rain_annual, "n_storm_events")
counts_flood   <- fit_poisson_trend(climate_rain_annual, "n_flood_events")
counts_drought <- fit_poisson_trend(climate_rain_annual, "n_drought_events")

## Derived: total events = sum of the three types
counts_all <- counts_storm %>%
  left_join(counts_flood,   by = "year") %>%
  left_join(counts_drought, by = "year") %>%
  mutate(n_disaster_events = n_storm_events + n_flood_events + n_drought_events)

## ---- 3b. Log-normal model for total damage ----
fit_lognormal_damage <- function(annual_df, damage_col) {
  obs <- annual_df %>%
    rename(dmg = !!damage_col) %>%
    filter(!is.na(dmg))
  
  # Proportion of years with zero reported damage
  p_zero <- mean(obs$dmg == 0, na.rm = TRUE)
  
  # Fit log-normal on non-zero years only
  nonzero <- obs %>% filter(dmg > 0) %>% pull(dmg)
  if (length(nonzero) < 3) {
    warning(paste("Too few non-zero observations for", damage_col, "— returning NA"))
    return(FULL_YEARS %>% mutate(!!damage_col := NA_real_))
  }
  
  mu_ln <- mean(log(nonzero))
  sd_ln <- sd(log(nonzero))
  
  FULL_YEARS %>%
    mutate(
      is_zero    = rbinom(n(), 1, p_zero),
      sev        = rlnorm(n(), mu_ln, sd_ln),
      !!damage_col := if_else(is_zero == 1, 0, sev)
    ) %>%
    dplyr::select(year, !!damage_col)
}

## Safety wrapper: if column missing or fewer than 3 non-zero values,
## return an NA frame rather than crashing or producing garbage.
safe_lognormal <- function(df, col) {
  if (!col %in% names(df)) {
    warning(paste("Column", col, "not found — returning NA frame"))
    return(FULL_YEARS %>% mutate(!!col := NA_real_))
  }
  nonzero_n <- sum(df[[col]] > 0, na.rm = TRUE)
  if (nonzero_n < 3) {
    warning(paste("Column", col, "has <3 non-zero values — returning NA frame"))
    return(FULL_YEARS %>% mutate(!!col := NA_real_))
  }
  fit_lognormal_damage(df, col)
}

damage_total   <- safe_lognormal(climate_rain_annual, "total_damage_adj_000us")
deaths_total   <- safe_lognormal(climate_rain_annual, "total_deaths")
affected_total <- safe_lognormal(climate_rain_annual, "total_affected")
recon_costs    <- safe_lognormal(climate_rain_annual, "reconstruction_costs_adj_000us")

## Diagnostic: confirm which EM-DAT columns exist and have data
cat("\nEM-DAT columns present:\n")
print(names(climate_rain_annual))
cat("Reconstruction non-zero rows:",
    sum(climate_rain_annual$reconstruction_costs_adj_000us > 0, na.rm = TRUE), "\n")

## ---- 3c. Assemble EM-DAT synthetic panel ----
## Strategy: rename EVERY synthetic column to *_syn BEFORE joining,
## so there are zero naming collisions and coalesce is unambiguous.
emdat_obs_years <- climate_rain_annual$year

## Rename synthetic severity frames explicitly
damage_syn   <- damage_total   %>% rename(total_damage_syn        = total_damage_adj_000us)
deaths_syn   <- deaths_total   %>% rename(total_deaths_syn        = total_deaths)
affected_syn <- affected_total %>% rename(total_affected_syn      = total_affected)
recon_syn    <- recon_costs    %>% rename(reconstruction_costs_syn = reconstruction_costs_adj_000us)

## Rename synthetic count columns (counts_all already has the right names
## but will collide with the observed join below — pre-rename them too)
counts_syn <- counts_all %>%
  rename(
    n_disaster_syn = n_disaster_events,
    n_storm_syn    = n_storm_events,
    n_flood_syn    = n_flood_events,
    n_drought_syn  = n_drought_events
  )

## Observed EM-DAT columns to overlay (observed wins over synthetic)
emdat_obs <- climate_rain_annual %>%
  dplyr::select(year, n_disaster_events, n_storm_events, n_flood_events,
                n_drought_events, total_damage_adj_000us, total_deaths,
                total_affected, reconstruction_costs_adj_000us,
                insured_damage_adj_000us)

## Join everything — no suffix collisions because every synthetic column
## has a unique _syn name before it enters the join chain
emdat_synthetic_final <- FULL_YEARS %>%
  left_join(counts_syn,    by = "year") %>%
  left_join(damage_syn,    by = "year") %>%
  left_join(deaths_syn,    by = "year") %>%
  left_join(affected_syn,  by = "year") %>%
  left_join(recon_syn,     by = "year") %>%
  left_join(emdat_obs,     by = "year") %>%
  mutate(
    n_disaster_events              = coalesce(n_disaster_events,              n_disaster_syn),
    n_storm_events                 = coalesce(n_storm_events,                 n_storm_syn),
    n_flood_events                 = coalesce(n_flood_events,                 n_flood_syn),
    n_drought_events               = coalesce(n_drought_events,               n_drought_syn),
    total_damage_adj_000us         = coalesce(total_damage_adj_000us,         total_damage_syn),
    total_deaths                   = coalesce(total_deaths,                   total_deaths_syn),
    total_affected                 = coalesce(total_affected,                 total_affected_syn),
    reconstruction_costs_adj_000us = coalesce(reconstruction_costs_adj_000us, reconstruction_costs_syn)
    # insured_damage_adj_000us: NOT coalesced — observed only, NA for synthetic years
  ) %>%
  dplyr::select(year, n_disaster_events, n_storm_events, n_flood_events,
                n_drought_events, total_damage_adj_000us, total_deaths,
                total_affected, reconstruction_costs_adj_000us,
                insured_damage_adj_000us) %>%
  add_synthetic_flag(emdat_obs_years)

cat("emdat_synthetic_final: ", nrow(emdat_synthetic_final), "rows,",
    sum(emdat_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 4. AGRICULTURE ORIENTATION INDEX (agri_govt_exp_clean)
# =============================================================
# Assumption: the agriculture orientation index (ratio of ag share
# of govt spending to ag share of GDP) is modelled as a linear trend
# fitted to observed 2001–2023 values, extrapolated backward to 1980
# with a floor of 0.1 (the index cannot realistically approach zero
# for an economy as agriculture-dependent as the Philippines).
# Source basis: FAO notes the Philippine index averaged ~0.8–1.2
# through the 1990s, consistent with the backward extrapolation here.

agri_exp_synthetic_final <- {
  obs_years_exp <- agri_govt_exp_clean$year
  
  filled <- fill_linear_trend(agri_govt_exp_clean, "year",
                              "agri_orientation_index",
                              floor_val = 0.1, cap_val = 3.0)
  
  FULL_YEARS %>%
    left_join(agri_govt_exp_clean, by = "year") %>%
    left_join(filled %>% rename(agri_idx_syn = agri_orientation_index), by = "year") %>%
    mutate(agri_orientation_index = coalesce(agri_orientation_index, agri_idx_syn)) %>%
    dplyr::select(year, agri_orientation_index) %>%
    add_synthetic_flag(obs_years_exp)
}

cat("agri_exp_synthetic_final: ", nrow(agri_exp_synthetic_final), "rows,",
    sum(agri_exp_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 5. OFFICIAL DEVELOPMENT FLOWS TO AGRICULTURE (official_flows_clean)
# =============================================================
# Assumption: ODA + other official flows to Philippine agriculture
# are modelled via linear trend extrapolation from observed 2002–2023
# values, with a floor of 0 (flows cannot be negative). Pre-2002
# values are extrapolated backward. The floor reflects that negative
# net official flows are structurally uncommon for a lower-middle-
# income economy in this period.

official_flows_synthetic_final <- {
  obs_years_oda <- official_flows_clean$year
  
  filled <- fill_linear_trend(official_flows_clean, "year",
                              "official_flows_agri_usd_m",
                              floor_val = 0)
  
  FULL_YEARS %>%
    left_join(official_flows_clean, by = "year") %>%
    left_join(filled %>% rename(flows_syn = official_flows_agri_usd_m), by = "year") %>%
    mutate(official_flows_agri_usd_m = coalesce(official_flows_agri_usd_m, flows_syn)) %>%
    dplyr::select(year, official_flows_agri_usd_m) %>%
    add_synthetic_flag(obs_years_oda)
}

cat("official_flows_synthetic_final: ", nrow(official_flows_synthetic_final), "rows,",
    sum(official_flows_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 6. WDI AGRICULTURE & RURAL DEVELOPMENT (agri_rural_long)
# =============================================================
# Strategy:
#   (a) Interpolate internal NAs using zoo::na.approx (linear).
#   (b) Extrapolate edge years (pre/post series) using OLS trend,
#       BUT only when >= MIN_OBS observed values exist AND the column
#       is not in the CONSTANT_COLS or LEAVE_NA_COLS lists.
#   (c) Columns with too few observations, physically constant values,
#       or where extrapolation is not meaningful are left as NA rather
#       than producing nonsensical negatives or zeros.
#
# Per-indicator bounds (floor_val / cap_val) are defined explicitly
# in the WDI_BOUNDS table below. This replaces the single regex-based
# floor from the previous version, which missed most indicators.

if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
library(zoo)

MIN_OBS <- 15   # minimum observed values required before extrapolating

# Columns where the true value is physically constant for Philippines
# (surface area does not change — use observed value for all years)
CONSTANT_COLS <- c(
  "surface_area_sq_km"
)

# Columns with too few observations or where OLS extrapolation is
# not meaningful — interpolate only, leave edge years as NA
LEAVE_NA_COLS <- c(
  "agriculture_irrigated_land_percent_of_total_agricultural_land",
  "rural_land_area_where_elevation_is_below_5m_percent_of_total_land_area"
)

# Per-indicator physical bounds: floor_val = minimum possible value,
# cap_val = maximum possible value. Percentages: [0, 100].
# Counts/areas/values: floor 0. Indices: floor 0.
WDI_BOUNDS <- tribble(
  ~col_pattern,                                          ~floor_val, ~cap_val,
  "percent",                                             0,          100,
  "access_to_electricity",                               0,          100,
  "irrigated_land",                                      0,          100,
  "cropland",                                            0,          100,
  "yield_kg_per_hectare",                                0,          NA,
  "cereal_production",                                   0,          NA,
  "cereal_yield",                                        0,          NA,
  "fertilizer_consumption",                              0,          NA,
  "fertiliser_consumption",                              0,          NA,
  "food_production_index",                               0,          NA,
  "crop_production_index",                               0,          NA,
  "livestock_production_index",                          0,          NA,
  "value_added",                                         0,          NA,
  "rural_population",                                    0,          NA,
  "rural_land_area",                                     0,          NA,
  "surface_area",                                        0,          NA,
  "imports",                                             0,          NA,
  "raw_material",                                        0,          100
)

## Helper: look up floor/cap for a given column name
get_bounds <- function(col_name) {
  matched <- WDI_BOUNDS %>%
    filter(str_detect(col_name, col_pattern)) %>%
    slice(1)   # first matching rule wins
  list(
    floor_val = if (nrow(matched) > 0 && !is.na(matched$floor_val)) matched$floor_val else NULL,
    cap_val   = if (nrow(matched) > 0 && !is.na(matched$cap_val))   matched$cap_val   else NULL
  )
}

## Identify numeric WDI columns
wdi_numeric_cols <- agri_rural_long %>%
  dplyr::select(-year) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

## Build a base: full year spine joined to observed WDI values
agri_rural_full <- FULL_YEARS %>%
  left_join(agri_rural_long, by = "year")

## Handle CONSTANT_COLS first: fill every year with the single observed value
for (col in intersect(CONSTANT_COLS, wdi_numeric_cols)) {
  obs_val <- agri_rural_long %>%
    pull(!!col) %>%
    .[!is.na(.)] %>%
    unique()
  if (length(obs_val) == 1) {
    agri_rural_full[[col]] <- obs_val
  } else if (length(obs_val) > 1) {
    # Multiple values recorded — use median, flag in assumption log
    agri_rural_full[[col]] <- median(obs_val, na.rm = TRUE)
  }
}

## Main fill loop for non-constant columns
for (col in setdiff(wdi_numeric_cols, CONSTANT_COLS)) {
  
  obs_col <- agri_rural_long %>%
    dplyr::select(year, val = !!col) %>%
    filter(!is.na(val))
  
  n_obs <- nrow(obs_col)
  
  ## Step 1: interpolate internal NAs regardless of n_obs
  ## (interpolation between two known points is always valid)
  agri_rural_full <- agri_rural_full %>%
    arrange(year) %>%
    mutate(!!col := zoo::na.approx(.data[[col]], na.rm = FALSE))
  
  ## Step 2: extrapolate edge NAs — only if enough observations
  ##         AND column is not in the leave-as-NA list
  if (n_obs < MIN_OBS || col %in% LEAVE_NA_COLS) next
  
  still_missing <- sum(is.na(agri_rural_full[[col]]))
  if (still_missing == 0) next
  
  bounds <- get_bounds(col)
  
  extrap <- fill_linear_trend(
    obs_col %>% rename(!!col := val),
    "year", col,
    floor_val = bounds$floor_val,
    cap_val   = bounds$cap_val
  )
  
  agri_rural_full <- agri_rural_full %>%
    left_join(extrap %>% rename(extrap_val = !!col), by = "year") %>%
    mutate(!!col := coalesce(.data[[col]], extrap_val)) %>%
    dplyr::select(-extrap_val)
}

agri_rural_synthetic_final <- agri_rural_full %>%
  add_synthetic_flag(agri_rural_long$year)

## Report which columns are still fully NA after filling
still_all_na <- agri_rural_synthetic_final %>%
  dplyr::select(all_of(wdi_numeric_cols)) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "n_na") %>%
  filter(n_na == nrow(agri_rural_synthetic_final))

if (nrow(still_all_na) > 0) {
  cat("WDI columns left fully NA (too few obs or in LEAVE_NA_COLS):\n")
  print(still_all_na$col)
}

cat("agri_rural_synthetic_final: ", nrow(agri_rural_synthetic_final), "rows,",
    sum(agri_rural_synthetic_final$synthetic), "synthetic\n")


# =============================================================
# 7. COMBINE INTO MASTER PANEL (replaces the full_join in main script)
# =============================================================
# Each dataset now covers MODEL_START–MODEL_END continuously.
# The synthetic_flag column from each dataset is retained as
# individual source flags, then combined into a single
# any_synthetic flag for easy filtering.

philippines_master_synthetic <- FULL_YEARS %>%
  left_join(agri_rural_synthetic_final %>%
              rename(synthetic_agri    = synthetic),          by = "year") %>%
  left_join(emdat_synthetic_final %>%
              rename(synthetic_emdat   = synthetic),          by = "year") %>%
  left_join(temp_synthetic_final %>%
              rename(synthetic_temp    = synthetic),          by = "year") %>%
  left_join(precip_synthetic_final %>%
              rename(synthetic_precip  = synthetic),          by = "year") %>%
  left_join(agri_exp_synthetic_final %>%
              rename(synthetic_exp     = synthetic),          by = "year") %>%
  left_join(official_flows_synthetic_final %>%
              rename(synthetic_flows   = synthetic),          by = "year") %>%
  mutate(
    any_synthetic = synthetic_agri | synthetic_emdat | synthetic_temp |
      synthetic_precip | synthetic_exp | synthetic_flows
  ) %>%
  arrange(year)

## Final sanity checks
cat("\n--- FINAL PANEL ---\n")
cat("Rows:   ", nrow(philippines_master_synthetic), "\n")
cat("Years:  ", range(philippines_master_synthetic$year), "\n")
cat("Any synthetic: ", sum(philippines_master_synthetic$any_synthetic, na.rm = TRUE), "rows\n")
cat("Fully observed:", sum(!philippines_master_synthetic$any_synthetic, na.rm = TRUE), "rows\n")
philippines_master_synthetic %>% count(year) %>% filter(n > 1) %>% print()  # expect 0 dupes

## Save
dir.create("data", showWarnings = FALSE)
write_csv(philippines_master_synthetic, "data/philippines_master_synthetic.csv")
cat("\nSaved data/philippines_master_synthetic.csv\n")


# =============================================================
# 8. ASSUMPTION LOG — print to console, include in appendix
# =============================================================
assumption_log <- tribble(
  ~dataset,                   ~method,                    ~justification,
  "Temperature/met series",   "OLS trend + N(0,resid_sd) noise",
  "WMO-standard gap-filling for continuous climate series",
  "Precip over 20mm",         "OLS trend + N(0,resid_sd) noise, floor 0",
  "Same as temperature; physical floor prevents negative days",
  "EM-DAT event counts",      "Poisson GLM with log-linear year trend",
  "Poisson is the standard actuarial model for event frequency; trend captures documented post-1980 increase in Philippine typhoon/flood records (IPCC AR6)",
  "EM-DAT damage/deaths",     "Log-normal severity, zero-inflation preserved",
  "Log-normal is the actuarial standard for property loss severity (Klugman et al., Loss Models)",
  "EM-DAT insured damage",    "NOT imputed — left as NA",
  "Imputing insured damage would contaminate the protection-gap calculation which is a primary model output",
  "Agri orientation index",   "OLS trend extrap, floor 0.1, cap 3.0",
  "FAO notes Philippines index ~0.8-1.2 in 1990s; bounds prevent unrealistic extrapolation",
  "ODA flows to agriculture", "OLS trend extrap, floor 0",
  "Negative net flows structurally uncommon for lower-middle-income economies",
  "WDI agri/rural indicators","zoo::na.approx interpolation + OLS edge extrapolation",
  "Linear interpolation for internal gaps is standard in WDI methodology; trend extrapolation bounded by physical/economic floors where applicable"
)

cat("\n=== ASSUMPTION LOG (include in competition appendix) ===\n")
print(assumption_log, n = Inf)