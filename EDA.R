library(ggplot2)
library(readxl)
library(MASS)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(pscl)
library(janitor)

# =============================================================
# NEED TO REFERENCE BIG DATASET MADE IN DIFF SHEET 
# =============================================================

theme_set(theme_minimal(base_size = 12))
trim_to_data <- function(df, cols) {
  df %>% filter(if_all(all_of(cols), ~ !is.na(.x)))
}
# A consistent palette: greens for agri/exposure, ambers/reds for
# climate severity, blue for water-related, so plots read consistently
# across the whole deck without re-deriving the meaning of color each time.
COL_AGRI    <- "#2D6A4F"
COL_CLIMATE <- "#BA7517"
COL_SEVERE  <- "#A32D2D"
COL_WATER   <- "#185FA5"
COL_GAP     <- "#993C1D"

# =============================================================
# PART A — AGRICULTURE / RURAL DEVELOPMENT (Stage 1: problem)
# =============================================================

## A1. Rural population and growth rate (two stacked panels)
p_a1a <- ggplot(trim_to_data(philippines_master, "rural_population"), aes(year, rural_population / 1e6)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Yearly Rural Population", y = "No. of People (Millions)", x = NULL)

p_a1b <- ggplot(trim_to_data(philippines_master, "rural_population_growth_annual_percent"), aes(year, rural_population_growth_annual_percent)) +
  geom_line(color = COL_AGRI, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Rural Population Growth Rate", y = "% annual growth", x = "Year")

grid.arrange(p_a1a, p_a1b, ncol = 1)
## is the insurable rural population growing, flat, or shrinking?
## A shrinking/flat rural population caps how much your risk pool can
## grow organically without expanding geographic coverage.

## A2. Agricultural Employment - total vs male vs female
emp_long <- philippines_master %>%
  trim_to_data(c("employment_in_agriculture_percent_of_total_employment_modeled_ilo_estimate",
                 "employment_in_agriculture_male_percent_of_male_employment_modeled_ilo_estimate",
                 "employment_in_agriculture_female_percent_of_female_employment_modeled_ilo_estimate")) %>%
  dplyr::select(year,
                Total  = employment_in_agriculture_percent_of_total_employment_modeled_ilo_estimate,
                Male   = employment_in_agriculture_male_percent_of_male_employment_modeled_ilo_estimate,
                Female = employment_in_agriculture_female_percent_of_female_employment_modeled_ilo_estimate) %>%
  pivot_longer(-year, names_to = "group", values_to = "pct")

ggplot(emp_long, aes(year, pct, color = group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(total = COL_AGRI, male = COL_WATER, female = COL_SEVERE)) +
  labs(title = "Agricultural Employment by Gender",
       y = "% Employed", x = "Year", color = NULL)

## the male/female gap matters for equitable distribution design -
## if female employment share is falling faster, your cooperative
## distribution channel should actively target female-headed farms.

## A3. Agriculture value added - % GDP and current US$ (two panels, different units)
p_a3a <- ggplot(trim_to_data(philippines_master, "agriculture_forestry_and_fishing_value_added_percent_of_gdp"), aes(year, agriculture_forestry_and_fishing_value_added_percent_of_gdp)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Agriculture, forestry & fishing — % of GDP", y = "% GDP", x = NULL)

p_a3b <- ggplot(trim_to_data(philippines_master, "agriculture_forestry_and_fishing_value_added_current_us"), aes(year, agriculture_forestry_and_fishing_value_added_current_us / 1e9)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Agriculture, forestry & fishing — Value Added", y = "US$ billion", x = "Year")

grid.arrange(p_a3a, p_a3b, ncol = 1)

## READ: GDP share falling while absolute value rises = economy
## diversifying away from agriculture even as the sector itself grows -
## common pattern, and it means your "addressable market" argument
## should lean on ABSOLUTE value/employment, not GDP share alone.

## A4. Cereal yield (kg/hectare) - the core yield-volatility variable
ggplot(trim_to_data(philippines_master, "cereal_yield_kg_per_hectare"), aes(year, cereal_yield_kg_per_hectare)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(title = "Cereal yield, Philippines", y = "kg per hectare", x = "Year",
       subtitle = "Dashed line = linear trend; residuals from this trend feed the yield-volatility model")

## READ: this is your STAGE 1 core evidence chart. Flat trend + visible
## year-to-year noise = climate-driven volatility, exactly the signature
## your product targets. Detrend (residuals vs fitted) for the CV calc:

yield_trend <- lm(cereal_yield_kg_per_hectare ~ year, data = philippines_master,
                  na.action = na.exclude)
philippines_master$cereal_yield_resid <- residuals(yield_trend)
cat("Cereal yield CV (detrended):",
    sd(philippines_master$cereal_yield_resid, na.rm = TRUE) /
      mean(philippines_master$cereal_yield_kg_per_hectare, na.rm = TRUE), "\n")

# sd / mean = variability in yield index 

## A5. Food vs crop vs livestock production index (base 2014-2016=100)
index_long <- philippines_master %>%
  trim_to_data(c("food_production_index_2014_2016_100",
                 "crop_production_index_2014_2016_100",
                 "livestock_production_index_2014_2016_100")) %>%
  dplyr::select(year,
                Food      = food_production_index_2014_2016_100,
                Crop      = crop_production_index_2014_2016_100,
                Livestock = livestock_production_index_2014_2016_100) %>%
  pivot_longer(-year, names_to = "index_type", values_to = "index_value")

ggplot(index_long, aes(year, index_value, color = index_type)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = c(food = COL_AGRI, crop = COL_CLIMATE, livestock = COL_WATER)) +
  labs(title = "Production indices (2014-2016 = 100)", y = "Index", x = "Year", color = NULL)
## READ: dips in the CROP line that coincide with known disaster years
## (cross-reference against n_storm_events/n_drought_events) are a
## direct visual validation of the peril before touching satellite data.

## A6. Average precipitation in depth (the long-run baseline)
ggplot(trim_to_data(philippines_master, "average_precipitation_in_depth_mm_per_year"), aes(year, average_precipitation_in_depth_mm_per_year)) +
  geom_line(color = COL_WATER, linewidth = 1) +
  geom_hline(aes(yintercept = mean(average_precipitation_in_depth_mm_per_year, na.rm = TRUE)),
             linetype = "dashed", color = "grey40") +
  labs(title = "Average annual precipitation", y = "mm per year", x = "Year",
       subtitle = "Dashed line = long-run mean - your trigger deficit baseline")
##HOW CAN I CHANGE THIS 

## A7. Freshwater withdrawals (agriculture) & arable land - two panels
p_a7a <- ggplot(trim_to_data(philippines_master, "annual_freshwater_withdrawals_agriculture_percent_of_total_freshwater_withdrawal"),
                aes(year, annual_freshwater_withdrawals_agriculture_percent_of_total_freshwater_withdrawal)) +
  geom_line(color = COL_WATER, linewidth = 1) +
  labs(title = "Freshwater Withdrawals — Agriculture Share", y = "% of Total Withdrawal", x = NULL)

p_a7b <- ggplot(trim_to_data(philippines_master, "arable_land_hectares_per_person"),
                aes(year, arable_land_hectares_per_person)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Arable land per person", y = "Hectares/person", x = "Year")

grid.arrange(p_a7a, p_a7b, ncol = 1)
## READ: declining arable land per person + high agri water share =
## land/water constraint that's worth a sentence in your problem framing,
## but is secondary context, not a primary actuarial input.

## A8. Agricultural land (% of land area)
ggplot(trim_to_data(philippines_master, "agricultural_land_percent_of_land_area"), aes(year, agricultural_land_percent_of_land_area)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Agricultural land as % of Total Land Area", y = "% of Total Land Area", x = "Year")

## A9. Agriculture GDP share (standalone version, in case A3 is split
## across slides differently)
ggplot(trim_to_data(philippines_master, "agriculture_forestry_and_fishing_value_added_percent_of_gdp"), aes(year, agriculture_forestry_and_fishing_value_added_percent_of_gdp)) +
  geom_area(fill = COL_AGRI, alpha = 0.3) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Agriculture GDP share over time", y = "% Share of GDP", x = "Year")

## A10. Agricultural raw material imports (% of merchandise imports)
ggplot(trim_to_data(philippines_master, "agricultural_raw_materials_imports_percent_of_merchandise_imports"), aes(year, agricultural_raw_materials_imports_percent_of_merchandise_imports)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Agricultural Raw Material Imports", y = "% of Merchandise Imports", x = "Year")
## READ: genuinely peripheral to your core insurance case - include
## only if you need supply-chain context; don't spend slide space here.

## A11. Rural population vs rural electricity access (distribution feasibility)
elec_pop <- philippines_master %>%
  trim_to_data(c("rural_population_percent_of_total_population",
                 "access_to_electricity_rural_percent_of_rural_population")) %>%
  dplyr::select(year, rural_population_percent_of_total_population,
                access_to_electricity_rural_percent_of_rural_population) %>%
  pivot_longer(-year, names_to = "metric", values_to = "value")

ggplot(elec_pop, aes(year, value, color = metric)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(rural_population_percent_of_total_population = COL_AGRI,
               access_to_electricity_rural_percent_of_rural_population = COL_WATER),
    labels = c("Rural population (% of total)", "Rural electricity access (% of rural pop)")
  ) +
  labs(title = "Rural population vs Rural Electricity Access", y = "% Share", x = "Year", color = NULL)
## READ: high/rising electricity access = mobile money/USSD distribution
## is viable; a persistent gap signals where cooperative-led, lower-tech
## enrolment is still necessary.

## A12. Fertiliser consumption (proxy for input intensity / learning premium baseline)
ggplot(trim_to_data(philippines_master, "fertilizer_consumption_kilograms_per_hectare_of_arable_land"), aes(year, fertilizer_consumption_kilograms_per_hectare_of_arable_land)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  labs(title = "Fertilizer Consumption", y = "kg / hectare of Arable Land", x = "Year")
## READ: a rough baseline of input-use intensity, useful to sense-check
## any NDVI-derived "resilient practice" score once you have satellite data.

# =============================================================
# PART B — CLIMATE / DISASTER 
# =============================================================

## B1. Disaster type / subtype - frequency bar chart
ggplot(climate_rain_clean, aes(fct_infreq(disaster_type), fill = disaster_type)) +
  geom_bar() +
  scale_fill_manual(values = c(Storm = COL_SEVERE, Flood = COL_WATER,
                               Drought = COL_CLIMATE), na.value = "grey60") +
  labs(title = "Disaster Events by Type", x = NULL, y = "Number of Events") +
  theme(legend.position = "none")

## Disaster SUBTYPE breakdown (more granular)
ggplot(climate_rain_clean, aes(fct_infreq(disaster_subtype))) +
  geom_bar(fill = COL_CLIMATE) +
  coord_flip() +
  labs(title = "Disaster Events by Subtype", x = NULL, y = "Number of Events")

## B2. Location heatmap - events by admin region
## EM-DAT's location field is often a free-text list of place names
## within one event row, so this needs splitting before counting.
location_counts <- climate_rain_clean %>%
  filter(!is.na(location)) %>%
  separate_rows(location, sep = ";|,") %>%        # split multi-location strings
  mutate(location = str_trim(location)) %>%
  count(location, sort = TRUE) %>%
  filter(n >= 2)                                    # drop one-off mentions/noise

ggplot(head(location_counts, 20), aes(reorder(location, n), n)) +
  geom_col(fill = COL_SEVERE) +
  coord_flip() +
  labs(title = "Most frequently affected locations (top 20)", x = NULL, y = "Number of disaster mentions")
## NOTE: for a TRUE geographic heatmap (choropleth map of the Philippines
## shaded by event frequency per province), you need province-level
## admin boundary shapefiles (e.g. GADM) joined to this location field -
## that's a separate, heavier task. This bar chart is the fast version;
## flag if you want the full choropleth and I'll build that separately
## using the gadm_admin_units field from EM-DAT.

## B3. Start and end dates - seasonality (which months disasters cluster in)
ggplot(climate_rain_clean %>% filter(!is.na(start_month)),
       aes(factor(start_month, levels = 1:12, labels = month.abb))) +
  geom_bar(fill = COL_CLIMATE) +
  labs(title = "Disaster Events by Starting Period", x = "Month", y = "Number of Events")

#Defines trigger window - e.g. July-October for wet-season rice"

## B4. Magnitude of disaster - distribution by type (boxplot, since units differ)
ggplot(climate_rain_clean %>% filter(!is.na(magnitude), disaster_type %in% c("Storm","Flood","Drought")),
       aes(disaster_type, magnitude, fill = disaster_type)) +
  geom_boxplot(outlier.alpha = 0.5) +
  scale_fill_manual(values = c(Storm = COL_SEVERE, Flood = COL_WATER, Drought = COL_CLIMATE)) +
  labs(title = "Magnitude Distribution by Disaster Type",
       x = "Disaster Type", y = "Magnitude") +
  theme(legend.position = "none")
print(magnitude_scale_reference)   # always print this alongside the plot above
#Units differ by type - check magnitude_scale_reference before interpreting

## B5. Total affected - trend over time, by type
affected_by_type <- climate_rain_clean %>%
  filter(!is.na(start_year), disaster_type %in% c("Storm","Flood","Drought")) %>%
  group_by(year = start_year, disaster_type) %>%
  summarise(total_affected = sum(total_affected, na.rm = TRUE), .groups = "drop")

ggplot(affected_by_type, aes(year, total_affected / 1e6, fill = disaster_type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(Storm = COL_SEVERE, Flood = COL_WATER, Drought = COL_CLIMATE)) +
  labs(title = "People Affected by Disasters per Year", y = "No. of People (millions)", x = "Year", fill = NULL)

## B6. Total deaths - trend over time
ggplot(trim_to_data(philippines_master, "total_deaths"), aes(year, total_deaths)) +
  geom_col(fill = COL_SEVERE) +
  labs(title = "Disaster Deaths per Year", y = "Total Deaths", x = "Year")
## READ: weaker signal for crop insurance specifically (a typhoon can
## kill without destroying crops, and vice versa for slow-onset drought)
## - keep as secondary context, not a primary trigger variable.

## B7. Start/end months - frequency (shown here as a year x month heatmap)
month_year_heat <- climate_rain_clean %>%
  filter(!is.na(start_month), !is.na(start_year)) %>%
  count(start_year, start_month)

ggplot(month_year_heat, aes(start_month, start_year, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = COL_SEVERE) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Disaster Event Frequency Heatmap by Year",
       x = "Month", y = "Year", fill = "Events")
## READ: this is the clearest single chart for justifying your trigger
## WINDOW - look for a clear vertical band of color concentrated in
## particular months (typically Jul-Oct for PH typhoons).

## B8. Total damage (CPI/adjusted) over time
ggplot(trim_to_data(philippines_master, "total_damage_adj_000us"), aes(year, total_damage_adj_000us / 1e6)) +
  geom_col(fill = COL_GAP) +
  labs(title = "Total Disaster Damage (Adjusted)", y = "US$ billion", x = "Year")

## B9. Total damage vs insured damage = protection gap (THE key severity chart)
gap_data <- philippines_master %>%
  filter(!is.na(total_damage_adj_000us)) %>%
  mutate(
    uninsured_damage_000us = total_damage_adj_000us - insured_damage_adj_000us,
    protection_gap_ratio = ifelse(total_damage_adj_000us > 0,
                                  1 - (insured_damage_adj_000us / total_damage_adj_000us),
                                  NA)
  )

ggplot(gap_data, aes(year)) +
  geom_col(aes(y = total_damage_adj_000us / 1e6), fill = COL_GAP, alpha = 0.4) +
  geom_col(aes(y = insured_damage_adj_000us / 1e6), fill = COL_AGRI) +
  labs(title = "Total Damage vs Insured Damage (the protection gap)",
       subtitle = "Light = uninsured, dark green = insured",
       y = "US$ billion", x = "Year")

ggplot(gap_data, aes(year, protection_gap_ratio)) +
  geom_col(fill = COL_GAP) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Protection Gap Ratio by Year",
       y = "Protection Gap", x = "Year")
## READ: THIS is your headline problem-statement chart. A ratio
## consistently near 100% means almost none of the agricultural economic
## loss is currently insured - the precise gap your product fills.
## 1 - (Insured Damage / Total Damage) - strongest statement 

# =============================================================
# PART C — ADDITIONAL PLOTS 
# =============================================================

## C1. THE KEY STAGE 2 CHART: rainfall vs detrended cereal yield
## This is the single most important chart for trigger calibration - it
## directly tests whether your proposed index (rainfall) predicts the
## outcome you're insuring against (yield loss).
rain_yield <- philippines_master %>%
  dplyr::select(year, average_precipitation_in_depth_mm_per_year, cereal_yield_resid) %>%
  filter(!is.na(average_precipitation_in_depth_mm_per_year), !is.na(cereal_yield_resid))

ggplot(rain_yield, aes(average_precipitation_in_depth_mm_per_year, cereal_yield_resid)) +
  geom_point(color = COL_WATER, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = COL_SEVERE, se = TRUE) +
  labs(title = "Rainfall vs detrended cereal yield",
       subtitle = paste("Pearson r =", round(cor(rain_yield$average_precipitation_in_depth_mm_per_year,
                                                 rain_yield$cereal_yield_resid,
                                                 use = "complete.obs"), 2)),
       x = "Average annual precipitation (mm)", y = "Yield residual (kg/ha, detrended)")
## depends on rainfall data - might change 
## READ: target r > 0.5-0.65 for the index to be considered viable for a
## parametric trigger. A weak/flat relationship here is important to know
## NOW, before committing to rainfall as your sole index variable - it
## may justify why USP 2 (the cooperative confirmation layer) exists, to
## correct for exactly this kind of basis risk.

## C2. Disaster frequency vs agricultural value added
ggplot(philippines_master %>% filter(!is.na(n_disaster_events)),
       aes(n_disaster_events, agriculture_forestry_and_fishing_value_added_percent_of_gdp)) +
  geom_point(color = COL_AGRI, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(title = "Disaster Frequency vs Agriculture's GDP share",
       x = "No. of Disaster Events per Year", y = "Agriculture % of GDP")

## C3. Loss proxy over time - previews the Stage 3 monitoring chart
## structure, using total_damage as a stand-in for "claims" until you
## have an actual premium pool to divide it by.
ggplot(gap_data, aes(year, total_damage_adj_000us / 1e6)) +
  geom_line(color = COL_GAP, linewidth = 1) +
  geom_point(color = COL_GAP, size = 2) +
  labs(title = "Annual aggregate Loss",
       y = "US$ billion", x = "Year")
## Once a real premium pool exists, divide this by earned premium to get an actual loss ratio

## C4. Days with precipitation over 20mm - flood-risk proxy distinct from
## total annual rainfall volume (same total, very different flood risk,
## if concentrated into fewer heavy-rain days)
ggplot(trim_to_data(philippines_master, "days_precip_over_20mm"), aes(year, days_precip_over_20mm)) +
  geom_line(color = COL_WATER, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(title = "Days per Year with >20mm Precipitation",
       y = "No. of Days", x = "Year")

## Rising trend = increasing flood/extreme-rainfall risk, distinct from total rainfall volume

## C5. Official development flows to agriculture vs total disaster damage -
## tests whether aid flows track disaster severity; supports your
## public-private structure argument.
aid_vs_damage <- philippines_master %>%
  dplyr::select(year, official_flows_agri_usd_m, total_damage_adj_000us) %>%
  filter(!is.na(official_flows_agri_usd_m), !is.na(total_damage_adj_000us)) %>%
  mutate(total_damage_usd_m = total_damage_adj_000us / 1000)

ggplot(aid_vs_damage, aes(year)) +
  geom_line(aes(y = official_flows_agri_usd_m), color = COL_AGRI, linewidth = 1) +
  geom_line(aes(y = total_damage_usd_m), color = COL_GAP, linewidth = 1, linetype = "dashed") +
  labs(title = "Official Aid Flows to Agriculture vs Total Disaster Damage",
       subtitle = "Solid = aid flows (US$m); dashed = total damage (US$m)",
       y = "US$ million", x = "Year")
## READ: if the dashed (damage) line is consistently far above the solid
## (aid) line, that's direct evidence current public flows are
## insufficient relative to need - strengthens the case for the
## reinsurance/development bank layers in your public-private structure.

## C6. Agriculture orientation index for government expenditure
ggplot(trim_to_data(philippines_master, "agri_orientation_index"), aes(year, agri_orientation_index)) +
  geom_line(color = COL_AGRI, linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  labs(title = "Agriculture Orientation Index for Government Expenditure",
       y = "Index", x = "Year")
## Index = 1: govt agri spending matches agri's GDP share; below 1: underfunded relative to economic weight
## READ: a persistently sub-1 index is a strong, citable justification
## for why a PUBLIC-PRIVATE structure (rather than pure government
## provision) is necessary - government alone is structurally underweight
## on agriculture spending relative to the sector's economic importance.
