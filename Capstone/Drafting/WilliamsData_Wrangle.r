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
# install.packages(c("tidyverse", "haven", "readxl")) # Run once if not installed
library(tidyverse)
library(haven)
library(readxl)


# STEP 2: CONFIGURE FILE PATHS
#-------------------------------------------------------------------------------------
# Set the base path to your specific data directory, using forward slashes.
base_path <- "C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data"

# Define paths to specific data files using file.path() for robustness.
# NOTE: The subfolder names ('DSDR-38421') and filenames ('38421-0001-Data.dta')
# must match what is inside your 'data' folder.
path_transpop  <- file.path(base_path, "DSDR-38421", "38421-0001-Data.dta")
path_geniuss   <- file.path(base_path, "RCMD-37229", "37229-0001-Data.csv")
path_estimates <- file.path(base_path, "DSDR-38853", "Transgender Adult Population Estimates by State (June 2022).xlsx")


# STEP 3: LOAD DATA FROM FILES
#-------------------------------------------------------------------------------------
transpop_df  <- read_dta(path_transpop)
geniuss_df   <- read_csv(path_geniuss) # Use read_dta() if it's a Stata file instead of CSV
estimates_df <- read_excel(path_estimates, sheet = "State Estimates") # Specify sheet name if needed


# STEP 4: HARMONIZE & COMBINE INDIVIDUAL-LEVEL DATA
#-------------------------------------------------------------------------------------
# This step requires consulting the codebooks. The variable names below are
# educated guesses. YOU MUST VERIFY THESE AGAINST THE DOCUMENTATION.

# --- Harmonize the TransPop Dataset (DSDR-38421) ---
transpop_harmonized <- transpop_df %>%
  mutate(source = "TransPop") %>%
  rename(
    age = V101_AGE,          # <-- VERIFY THIS VARIABLE NAME!
    state_fips = V203_STATE, # <-- VERIFY THIS VARIABLE NAME!
    survey_weight = FINALWT  # <-- VERIFY THIS VARIABLE NAME!
  ) %>%
  mutate(
    gender_identity_category = case_when(
      GENDER_TME == 1 ~ "Transgender Man",
      GENDER_TW == 1 ~ "Transgender Woman",
      GENDER_NB == 1 ~ "Nonbinary",
      TRUE ~ NA_character_
    ),
    state_fips = as.character(state_fips)
  ) %>%
  select(source, age, state_fips, gender_identity_category, survey_weight)

# --- Harmonize the GENIUSS Dataset (RCMD-37229) ---
geniuss_harmonized <- geniuss_df %>%
  mutate(source = "GENIUSS") %>%
  rename(
    age = `_AGEG5YR`,         # <-- VERIFY THIS VARIABLE NAME!
    state_fips = `_STATE`,    # <-- VERIFY THIS VARIABLE NAME!
    survey_weight = `_LLCPWT` # <-- VERIFY THIS VARIABLE NAME!
  ) %>%
  mutate(
    gender_identity_category = case_when(
      SEX == 1 & TRANSGND == 2 ~ "Transgender Woman", # These names (SEX, TRANSGND) are common but must be verified
      SEX == 2 & TRANSGND == 1 ~ "Transgender Man",
      TRANSGND == 3 ~ "Nonbinary",
      SEX == 1 & TRANSGND == 1 ~ "Cisgender Man",
      SEX == 2 & TRANSGND == 2 ~ "Cisgender Woman",
      TRUE ~ NA_character_
    ),
    state_fips = str_pad(as.character(state_fips), width = 2, side = "left", pad = "0")
  ) %>%
  select(source, age, state_fips, gender_identity_category, survey_weight)

# --- Stack the Harmonized Data Frames ---
individual_df <- bind_rows(transpop_harmonized, geniuss_harmonized)

cat("\n--- Glimpse of the combined individual-level data frame ---\n")
glimpse(individual_df)


# STEP 5: AGGREGATE & JOIN TO CREATE FINAL STATE-LEVEL FRAME
#-------------------------------------------------------------------------------------
# Prepare the state estimates data frame for joining.
geo_df <- estimates_df %>%
  rename(
    state_name = State,
    state_fips = `State FIPS Code`, # <-- VERIFY THIS COLUMN NAME!
    trans_pop_estimate = `Transgender Adult Population Estimate`
  ) %>%
  mutate(
    state_fips = str_pad(as.character(state_fips), width = 2, side = "left", pad = "0")
  ) %>%
  select(state_name, state_fips, trans_pop_estimate)

# Aggregate the combined survey data to the state level
state_level_stats <- individual_df %>%
  filter(!is.na(state_fips), !is.na(survey_weight), !is.na(age)) %>%
  group_by(state_fips) %>%
  summarise(
    mean_age_weighted = weighted.mean(age, w = survey_weight, na.rm = TRUE),
    survey_respondents_n = n(),
    .groups = 'drop'
  )

cat("\n--- Aggregated statistics calculated from survey data ---\n")
print(state_level_stats)

# --- Join the aggregated stats to the geographic estimates ---
final_geo_df <- left_join(geo_df, state_level_stats, by = "state_fips")


# STEP 6: VIEW THE FINAL COMBINED DATA FRAME
#-------------------------------------------------------------------------------------
cat("\n--- Final combined state-level data frame ---\n")
cat("Each row represents a state, combining population estimates with aggregated survey data.\n\n")

print(final_geo_df)