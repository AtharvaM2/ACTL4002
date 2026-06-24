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

# Import datasets
agri_rural_raw <- read_excel("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Agriculture & Rural Development.xls")

#Data Clean
colnames(agri_rural_raw) <- as.character(unlist(agri_rural_raw[3, ]))
agri_rural_clean <- agri_rural_raw[-c(1:3), ]
agri_rural_clean <- agri_rural_clean %>%
  janitor::clean_names()

agri_philippines <- agri_rural_clean %>%
  filter(country_name == "Philippines")

climate_rain <- read_excel("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Natural disasters - climate and rainfall.xlsx")

#Data clean
climate_rain_philippines <- climate_rain %>%
  filter(Country == "Philippines")

temp_series <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Observed Weather Time Series.csv")

precip_20mm <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Days_with_Precipitation_over_20mm.csv")
precip_20mm_philippines <- precip_20mm %>%
  filter(REF_AREA_LABEL == "Philippines")


agri_govt_exp <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/The agriculture orientation index for government expenditures.csv")
agri_govt_exp_phl <- agri_govt_exp %>%
  filter(REF_AREA_LABEL == "Philippines")

official_flows_agri <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Total official flows official development assistance plus other official flows to the agriculture sector 2000-2023 Millions US dollars 2022 constant prices.csv")
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
# STEP 2 — JOIN EVERYTHING ON YEAR
# =============================================================
## full_join (not inner_join) so the panel keeps every year present
## in ANY dataset - your coverage ranges from 1901 (temp_series) to
## 2025 (agri_rural), and an inner_join would crush this down to only
## the years common to ALL six datasets (likely just 2001-2023).

philippines_master <- agri_rural_long %>%
  full_join(climate_rain_annual,   by = "year") %>%
  full_join(temp_series_clean,     by = "year") %>%
  full_join(precip_20mm_clean,     by = "year") %>%
  full_join(agri_govt_exp_clean,   by = "year") %>%
  full_join(official_flows_clean,  by = "year") %>%
  arrange(year)

# =============================================================
# STEP 3 — SANITY CHECKS (run every time before using the panel)
# =============================================================

range(philippines_master$year, na.rm = TRUE)              # expect ~1901-2025
colSums(is.na(philippines_master)) %>% sort(decreasing = TRUE)  # spot fully-empty columns
philippines_master %>% count(year) %>% filter(n > 1)       # should be ZERO rows (no dupes)
philippines_master %>% filter(year == 2020) %>% glimpse()  # eyeball a known year

# =============================================================
# STEP 4 — DATA DICTIONARY (column descriptions, kept SEPARATE
# from the numeric panel - never add a text row inside the panel
# itself, since that forces every numeric column to character type
# and silently breaks every downstream calculation: mean(), lm(),
# the burn analysis, all of it)
# =============================================================
## Pulls the REAL indicator label/description text out of each
## source file rather than hand-typing guesses, so the dictionary
## stays accurate even if you add more indicators later.

dict_agri_rural <- agri_philippines %>%
  distinct(indicator_name) %>%
  transmute(
    column_name = janitor::make_clean_names(indicator_name),
    source      = "Agriculture & Rural Development (World Bank)",
    description = indicator_name
  )

dict_climate_annual <- tibble::tribble(
  ~column_name,                     ~source,                                  ~description,
  "n_disaster_events",              "EM-DAT (Natural disasters - climate and rainfall)", "Total number of recorded disaster events in the Philippines that year",
  "n_drought_events",                "EM-DAT (Natural disasters - climate and rainfall)", "Number of events classified Disaster Type = Drought",
  "n_flood_events",                   "EM-DAT (Natural disasters - climate and rainfall)", "Number of events classified Disaster Type = Flood",
  "n_storm_events",                    "EM-DAT (Natural disasters - climate and rainfall)", "Number of events classified Disaster Type = Storm",
  "total_deaths",                       "EM-DAT (Natural disasters - climate and rainfall)", "Sum of Total Deaths across all disaster events that year",
  "total_affected",                      "EM-DAT (Natural disasters - climate and rainfall)", "Sum of Total Affected (people) across all disaster events that year",
  "total_damage_adj_000us",               "EM-DAT (Natural disasters - climate and rainfall)", "Sum of Total Damage, Adjusted ('000 US$, CPI-adjusted) across all events that year",
  "insured_damage_adj_000us",              "EM-DAT (Natural disasters - climate and rainfall)", "Sum of Insured Damage, Adjusted ('000 US$, CPI-adjusted) across all events that year",
  "reconstruction_costs_adj_000us",         "EM-DAT (Natural disasters - climate and rainfall)", "Sum of Reconstruction Costs, Adjusted ('000 US$, CPI-adjusted) across all events that year",
  "storm_magnitude_mean",                    "EM-DAT (Natural disasters - climate and rainfall)", "Mean Magnitude across storm events that year - unit varies, see magnitude_scale_reference (often km/h wind speed)",
  "flood_magnitude_mean",                     "EM-DAT (Natural disasters - climate and rainfall)", "Mean Magnitude across flood events that year - unit varies, see magnitude_scale_reference",
  "drought_magnitude_mean",                    "EM-DAT (Natural disasters - climate and rainfall)", "Mean Magnitude across drought events that year - frequently NA, as droughts are often unmeasured by magnitude in EM-DAT"
)

## temp_series indicator labels - pulls whatever the original
## (pre-clean_names) column names were, since these typically
## describe the variable directly (e.g. "Average Mean Surface
## Air Temperature").
dict_temp_series <- tibble(
  column_name = janitor::make_clean_names(setdiff(names(temp_series), names(temp_series)[1])),
  source      = "Observed Weather Time Series",
  description = setdiff(names(temp_series), names(temp_series)[1])
)

dict_precip_20mm <- tibble(
  column_name = "days_precip_over_20mm",
  source      = "Days_with_Precipitation_over_20mm (World Bank CCKP / Data360)",
  description = unique(precip_20mm_philippines$INDICATOR_LABEL)[1]
)

dict_agri_govt_exp <- tibble(
  column_name = "agri_orientation_index",
  source      = "Agriculture orientation index for government expenditures",
  description = if ("INDICATOR_LABEL" %in% names(agri_govt_exp_phl)) {
    unique(agri_govt_exp_phl$INDICATOR_LABEL)[1]
  } else {
    "Agriculture orientation index for government expenditures (ratio of agriculture's share of govt spending to its share of GDP)"
  }
)

dict_official_flows <- tibble(
  column_name = "official_flows_agri_usd_m",
  source      = "Total official flows (ODA + other official flows) to agriculture sector",
  description = if ("INDICATOR_LABEL" %in% names(official_flows_agri_phl)) {
    unique(official_flows_agri_phl$INDICATOR_LABEL)[1]
  } else {
    "Total official development assistance plus other official flows to the agriculture sector, millions of US$, 2022 constant prices"
  }
)

dict_year <- tibble(
  column_name = "year",
  source      = "Join key",
  description = "Calendar year - the key all six source datasets are joined on"
)

# Combine into one master dictionary, one row per column in philippines_master
data_dictionary <- bind_rows(
  dict_year,
  dict_agri_rural,
  dict_climate_annual,
  dict_temp_series,
  dict_precip_20mm,
  dict_agri_govt_exp,
  dict_official_flows
) %>%
  distinct(column_name, .keep_all = TRUE)   # in case any indicator name appears in multiple sources
