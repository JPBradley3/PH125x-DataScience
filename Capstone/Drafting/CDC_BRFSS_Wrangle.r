###################################################################################
###################################################################################
##
##                         ---------------------------------
##                            U.S. LGBT Emigration Study
##                         ---------------------------------
##
##  Name:             James P. Bradley III
##  Class:            PH125.9x Data Science
##  Assignment:       Capstone Project
##  Institute:        Harvard Online
##
##  Description:      This script processes and visualizes data from the
##                    CDC's BRFSS survey datasets on LGBT demographics and
##                    transgender population estimates.
##
###################################################################################
###################################################################################

###################################################################################
###################################################################################
##
##                         ---------------------------------
##                                   Bibliography
##                         ---------------------------------
##
##    CDC BRFSS Behavioral Surveillance System
##
##    • BRFSS ASC Data & Documentation
##        ◦ https://www.cdc.gov/brfss/annual_data/2023/files/LLCP23V2_ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2022/files/LLCP22V2_ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2021/files/LLCP21V3_ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2020/files/LLCP20V3_ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2019/files/LLCP19V3_ASC.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2018/files/LLCP18V3_ASC.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2017/files/LLCP17V3_ASC.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2016/files/LLCP16V3_ASC.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2015/llcp_multiq.html
##        ◦ https://www.cdc.gov/brfss/annual_data/2014/files/LLCP14V3_ASC.ZIP
##        ◦ http://www.cdc.gov/brfss/annual_data/2013/files/llcp13v3_asc.zip
##        ◦ http://www.cdc.gov/brfss/annual_data/2012/files/LLCP12V3_ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011ASC.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10ASC.zip
##
##   • BRFSS XPT Data & Documentation
##        ◦ https://www.cdc.gov/brfss/annual_data/2023/files/LLCP2023XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2021/files/LLCP2021XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip
##        ◦ https://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2013/files/LLCP2013XPT.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2012/files/LLCP2012XPT.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011XPT.ZIP
##        ◦ https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip
##
##
###################################################################################
###################################################################################

# ==============================
# SET THE WORKING DIRECTORY
# ==============================
# Set the working directory to the correct project folder
setwd("C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting")

# ==============================
# LOAD LIBRARIES
# ==============================
# Use pacman to manage packages: it will install them if not present, then load them.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Core suite for data manipulation (dplyr) and visualization (ggplot2)
  haven,      # To read SAS data files (.XPT), the format for BRFSS
  survey,     # ESSENTIAL for analyzing complex survey data like BRFSS
  sf,         # For handling spatial (map) data
  tigris,     # To download U.S. Census shapefiles for maps
  viridis,    # For colorblind-friendly and attractive plot palettes
  fs,         # For cross-platform file and directory management
  here,       # For creating file paths relative to the project root, making the script portable
  maps,       # For map data
  patchwork   # For combining plots
)

# ==============================
# SET FILE PATHS
# ==============================
# Create directories for processed data and plots.
dir_create(here("data_processed"))
dir_create(here("plots"))

# ==============================
# LOAD DATA
# ==============================
# Define a function to load and pre-process a single year's data from CSV files
load_brfss_csv <- function(year) {
  cat("Processing year:", year, "\n")
  
  # For 2010, the filename pattern is different
  if (year == 2010) {
    file_name <- paste0("CDBRFS10.csv")
  } else {
    file_name <- paste0("LLCP", year, ".csv")
  }
  
  # Use absolute path to ensure correct file location
  file_path <- file.path("C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/BRFSS_Data", 
                         as.character(year), file_name)
  
  if (!file.exists(file_path)) {
    warning("CSV file not found at: ", file_path, ". Please check the file name and location. Skipping.")
    return(NULL)
  }
  
  # Read the CSV file
  data <- read_csv(file_path, show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(year = year)

  data %>%
    select(
        year,
        state = `_state`,
        weight = `_llcpwt`,
        strata = `_ststr`,
        psu = `_psu`,
        sex_orient = any_of(c("sexorien", "sxorient")), 
        transgender = any_of(c("trnsgndr", "transgen"))
    )
}

# Define the years to analyze
years_to_process <- 2014:2022

# Use map() to apply the function to each year and combine the results
brfss_raw <- map_df(years_to_process, load_brfss_csv)

# Check if any data was loaded
if (nrow(brfss_raw) == 0) {
  stop("No BRFSS data was loaded. Please check the file paths.")
}

# Save the combined raw data as a feather file for faster loading in the future
write_rds(brfss_raw, file.path("C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data_processed", "brfss_2014-2022_raw.rds"))

# ==============================
# CLEAN DATA
# ==============================
if (exists("brfss_raw") && nrow(brfss_raw) > 0) {
  brfss_clean <- brfss_raw %>%
    mutate(state = as.integer(state)) %>%
    filter(state < 60) %>%
    mutate(
      lgb_status = as.factor(case_when(
        sex_orient == 1              ~ "Straight",
        sex_orient %in% c(2, 3, 4)   ~ "LGB",
        TRUE                         ~ NA_character_
      )),
      trans_status = as.factor(case_when(
        transgender %in% c(1, 2, 3) & year < 2019 ~ "Transgender",
        transgender == 4 & year < 2019 ~ "Cisgender",
        transgender == 1 & year >= 2019 ~ "Transgender",
        transgender == 2 & year >= 2019 ~ "Cisgender",
        TRUE ~ NA_character_
      ))
    ) %>%
    left_join(
      tigris::states(cb = TRUE, year = 2022) %>%
        as.data.frame() %>%
        select(state_fips_char = STATEFP, state_name = NAME) %>%
        mutate(state_fips_numeric = as.integer(state_fips_char)) %>%
        distinct(state_fips_numeric, state_name),
      by = c("state" = "state_fips_numeric")
    )
} else {
  message("No data available to clean.")
  brfss_clean <- tibble(
    year = integer(),
    state = integer(),
    weight = numeric(),
    strata = numeric(),
    psu = numeric(),
    sex_orient = numeric(),
    transgender = numeric(),
    lgb_status = factor(),
    trans_status = factor(),
    state_name = character()
  )
}

# Add binary variable
brfss_clean <- brfss_clean %>%
  mutate(trans_binary = ifelse(trans_status == "Transgender", 1, 0),
         lgb_binary = ifelse(lgb_status == "LGB", 1, 0))

# Simple aggregation by state
trans_by_state <- brfss_clean %>%
  filter(!is.na(trans_binary)) %>%
  group_by(state_name) %>%
  summarize(
    total = n(),
    trans_count = sum(trans_binary, na.rm = TRUE),
    trans_percent = mean(trans_binary, na.rm = TRUE) * 100
  ) %>%
  filter(total >= 30)

lgb_by_state <- brfss_clean %>%
  filter(!is.na(lgb_binary)) %>%
  group_by(state_name) %>%
  summarize(
    total = n(),
    lgb_count = sum(lgb_binary, na.rm = TRUE),
    lgb_percent = mean(lgb_binary, na.rm = TRUE) * 100
  ) %>%
  filter(total >= 30)

# Create maps
us_states <- tigris::states(cb = TRUE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "AS", "MP", "UM"))

# Transgender map
trans_map_data <- us_states %>%
  left_join(trans_by_state, by = c("NAME" = "state_name"))

trans_map <- ggplot(trans_map_data) +
  geom_sf(aes(fill = trans_percent), color = "white", size = 0.2) +
  scale_fill_viridis(option = "plasma", name = "Transgender %", na.value = "gray90") +
  theme_minimal()

# LGB map
lgb_map_data <- us_states %>%
  left_join(lgb_by_state, by = c("NAME" = "state_name"))

lgb_map <- ggplot(lgb_map_data) +
  geom_sf(aes(fill = lgb_percent), color = "white", size = 0.2) +
  scale_fill_viridis(option = "viridis", name = "LGB %", na.value = "gray90") +
  theme_minimal()

# Save maps
ggsave(here("plots", "transgender_map.png"), trans_map, width = 10, height = 7)
ggsave(here("plots", "lgb_map.png"), lgb_map, width = 10, height = 7)

# Save processed data
write_rds(trans_by_state, here("data_processed", "transgender_by_state.rds"))
write_rds(lgb_by_state, here("data_processed", "lgb_by_state.rds"))

# Print completion message
cat("Analysis complete. Results saved to the 'plots' and 'data_processed' directories.\n")
