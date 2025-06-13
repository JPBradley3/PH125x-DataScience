######################################################################################
#
# Bibliography of Datasets Used
#
######################################################################################
######################################################################################
#
# The Williams Institute UCLA
#    • LGBT Data & Demographics
#        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/37166
#    • U.S. Transgender Population Estimates
#        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/38853
#    • GENIUSS Group (Gender Identity in U.S. Surveillance)
#        ◦ https://www.icpsr.umich.edu/web/RCMD/studies/37229
#        ◦ https://www.icpsr.umich.edu/web/RCMD/studies/34363
#    • TransPop Survey Overview
#        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/38421
#
######################################################################################
######################################################################################
#
# R SCRIPT FOR WRANGLING WILLIAMS INSTITUTE DATASETS FROM ICPSR
#
# Objective: Combine multiple datasets to create a single, state-level analytical
# frame that contains both population estimates and aggregated survey characteristics.
#
# Steps:
# 1. Load raw data files (surveys and state-level estimates).
# 2. Harmonize individual-level survey data into one master data frame.
# 3. Aggregate the harmonized individual data to the state level using survey weights.
# 4. Join the aggregated statistics with the state-level population estimates.
#
######################################################################################


# STEP 1: SETUP - LOAD LIBRARIES
#-------------------------------------------------------------------------------------
library(tidyverse)
library(haven)


# STEP 2: CONFIGURE FILE PATHS FOR ACCESSIBLE DATA
#-------------------------------------------------------------------------------------
base_path <- "C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data"

# Paths for the two datasets you have downloaded
path_lgbt_demographics <- file.path(base_path, "DSDR-37166", "37166-0007-Data.csv") # Check .csv extension
path_transpop  <- file.path(base_path, "DSDR-38421", "38421-0001-Data.dta")


# STEP 3: LOAD ACCESSIBLE DATA FROM FILES
#-------------------------------------------------------------------------------------
lgbt_demographics_df <- read_csv(path_lgbt_demographics)
transpop_df  <- read_dta(path_transpop)


# STEP 4: CREATE THE GEOGRAPHIC DATA FRAME MANUALLY
#-------------------------------------------------------------------------------------
# Data sourced from the Williams Institute website. We use tribble() to create a data frame.
# This replaces the need to download the restricted .xlsx file.
geo_df <- tribble(
  ~state_name,    ~state_fips, ~trans_pop_estimate,
  "Alabama",      "01",        16100,
  "Alaska",       "02",        3000,
  "Arizona",      "04",        36900,
  "Arkansas",     "05",        12000,
  "California",   "06",        192900,
  "Colorado",     "08",        27400,
  "Connecticut",  "09",        14100,
  "Delaware",     "10",        4100,
  "Florida",      "12",        121100,
  "Georgia",      "13",        55500,
  "Hawaii",       "15",        6800,
  "Idaho",        "16",        6400,
  "Illinois",     "17",        52200,
  "Indiana",      "18",        26800,
  "Iowa",         "19",        11900,
  "Kansas",       "20",        10500,
  "Kentucky",     "21",        16700,
  "Louisiana",    "22",        18100,
  "Maine",        "23",        6000,
  "Maryland",     "24",        27600,
  "Massachusetts","25",        30000,
  "Michigan",     "26",        37600,
  "Minnesota",    "27",        22700,
  "Mississippi",  "28",        11100,
  "Missouri",     "29",        23500,
  "Montana",      "30",        4200,
  "Nebraska",     "31",        7100,
  "Nevada",       "32",        14600,
  "New Hampshire","33",        5700,
  "New Jersey",   "34",        38700,
  "New Mexico",   "35",        9000,
  "New York",     "36",        94800,
  "North Carolina","37",       47200,
  "North Dakota", "38",        2500,
  "Ohio",         "39",        44000,
  "Oklahoma",     "40",        16000,
  "Oregon",       "41",        20500,
  "Pennsylvania", "42",        48500,
  "Rhode Island", "44",        4300,
  "South Carolina","45",       20600,
  "South Dakota", "46",        3000,
  "Tennessee",    "47",        28600,
  "Texas",        "48",        125300,
  "Utah",         "49",        11900,
  "Vermont",      "50",        2900,
  "Virginia",     "51",        38100,
  "Washington",   "52",        39800,
  "West Virginia","53",        6300,
  "Wisconsin",    "54",        21100,
  "Wyoming",      "55",        1900
)

# STEP 5: HARMONIZE & COMBINE ACCESSIBLE INDIVIDUAL-LEVEL DATA
#-------------------------------------------------------------------------------------
# --- Harmonize LGBT Demographics (37166) ---
lgbt_demographics_harmonized <- lgbt_demographics_df %>%
  mutate(source = "LGBT_Demographics_37166") %>%
  rename(age = AGE, state_fips = STATEFIP, survey_weight = L2BWT) %>%
  mutate(state_fips = str_pad(as.character(state_fips), width = 2, side = "left", pad = "0")) %>%
  select(source, age, state_fips, survey_weight)

# --- Harmonize TransPop (38421) ---
transpop_harmonized <- transpop_df %>%
  mutate(source = "TransPop_38421") %>%
  rename(age = V101_AGE, state_fips = V203_STATE, survey_weight = FINALWT) %>%
  mutate(state_fips = as.character(state_fips)) %>%
  select(source, age, state_fips, survey_weight)

# --- Stack the TWO Harmonized Data Frames ---
individual_df <- bind_rows(lgbt_demographics_harmonized, transpop_harmonized)

# STEP 6: AGGREGATE & JOIN TO CREATE FINAL STATE-LEVEL FRAME
#-------------------------------------------------------------------------------------
state_level_stats <- individual_df %>%
  filter(!is.na(state_fips), !is.na(survey_weight), !is.na(age), survey_weight > 0) %>%
  group_by(state_fips) %>%
  summarise(
    mean_age_weighted = weighted.mean(age, w = survey_weight, na.rm = TRUE),
    survey_respondents_n = n(),
    .groups = 'drop'
  )

final_geo_df <- left_join(geo_df, state_level_stats, by = "state_fips")

# STEP 7: VIEW THE FINAL COMBINED DATA FRAME
#-------------------------------------------------------------------------------------
cat("\n--- Final combined state-level data frame (built from accessible sources) ---\n")
print(final_geo_df, n=51)