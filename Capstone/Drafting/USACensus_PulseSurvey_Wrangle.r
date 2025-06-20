#############################################################################################################################
#############################################################################################################################
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
##                    U.S. Census Household Pulse Survey to analyze LGBT demographics
##                    and potential emigration patterns.
##
#############################################################################################################################
#############################################################################################################################

# Load required libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(knitr)
library(readr)

# Set working directory if needed
# setwd("your/working/directory")

#############################################################################################################################
##                                          Data Download and Import
#############################################################################################################################

# Function to download and extract Pulse Survey data
download_pulse_data <- function(url, destfile) {
  temp <- tempfile()
  download.file(url, temp)
  unzip(temp, exdir = "data_raw")
  unlink(temp)
  return(list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)[1])
}

# Example usage for one dataset
# pulse_url <- "https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk63/HPS_Week63_PUF_CSV.zip"
# csv_path <- download_pulse_data(pulse_url, "data_raw/week63.zip")
# pulse_data <- fread(csv_path)

#############################################################################################################################
##                                          Data Processing Functions
#############################################################################################################################

# Function to clean and standardize Pulse Survey data
clean_pulse_data <- function(data) {
  # Select relevant columns
  # Adjust column names based on the specific dataset structure
  data_clean <- data %>%
    select(
      SCRAM,                # Unique identifier
      WEEK,                 # Survey week
      TBIRTH_YEAR,          # Birth year
      GENID_DESCRIBE,       # Gender identity
      SEXUAL_ORIENTATION,   # Sexual orientation
      EST_ST,               # State
      MOVE,                 # Moving/relocation status
      MOVEWHY_CRIME,        # Moving due to crime
      MOVEWHY_QUALLIFE,     # Moving for quality of life
      MOVEWHY_HOUSING,      # Moving for housing reasons
      MOVEWHY_HEALTHSVC,    # Moving for health services
      MOVEWHY_GOVTPOLICY    # Moving due to government policy
    )
  
  # Handle missing values
  data_clean <- data_clean %>%
    mutate(across(everything(), ~ifelse(. %in% c(-88, -99), NA, .)))
  
  # Create derived variables
  data_clean <- data_clean %>%
    mutate(
      age = 2023 - TBIRTH_YEAR,  # Calculate age (adjust year as needed)
      lgbt_status = case_when(
        SEXUAL_ORIENTATION %in% c(2, 3, 4) | GENID_DESCRIBE == 2 ~ "LGBT",
        SEXUAL_ORIENTATION == 1 & GENID_DESCRIBE == 1 ~ "Non-LGBT",
        TRUE ~ NA_character_
      ),
      moving_status = case_when(
        MOVE == 1 ~ "Planning to move",
        MOVE == 2 ~ "Not planning to move",
        TRUE ~ NA_character_
      )
    )
  
  return(data_clean)
}

# Function to analyze LGBT emigration patterns
analyze_lgbt_emigration <- function(data) {
  # Calculate proportion planning to move by LGBT status
  move_by_lgbt <- data %>%
    filter(!is.na(lgbt_status), !is.na(moving_status)) %>%
    group_by(lgbt_status, moving_status) %>%
    summarise(count = n()) %>%
    group_by(lgbt_status) %>%
    mutate(proportion = count / sum(count))
  
  # Analyze reasons for moving by LGBT status
  reasons_by_lgbt <- data %>%
    filter(!is.na(lgbt_status), moving_status == "Planning to move") %>%
    group_by(lgbt_status) %>%
    summarise(
      crime = mean(MOVEWHY_CRIME == 1, na.rm = TRUE),
      quality_of_life = mean(MOVEWHY_QUALLIFE == 1, na.rm = TRUE),
      housing = mean(MOVEWHY_HOUSING == 1, na.rm = TRUE),
      health_services = mean(MOVEWHY_HEALTHSVC == 1, na.rm = TRUE),
      govt_policy = mean(MOVEWHY_GOVTPOLICY == 1, na.rm = TRUE)
    )
  
  return(list(move_by_lgbt = move_by_lgbt, reasons_by_lgbt = reasons_by_lgbt))
}

#############################################################################################################################
##                                          Data Visualization Functions
#############################################################################################################################

# Function to plot moving intentions by LGBT status
plot_moving_intentions <- function(move_data) {
  ggplot(move_data, aes(x = lgbt_status, y = proportion, fill = moving_status)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Moving Intentions by LGBT Status",
      x = "LGBT Status",
      y = "Proportion",
      fill = "Moving Status"
    ) +
    theme_minimal()
}

# Function to plot reasons for moving by LGBT status
plot_moving_reasons <- function(reasons_data) {
  reasons_long <- reasons_data %>%
    pivot_longer(
      cols = c(crime, quality_of_life, housing, health_services, govt_policy),
      names_to = "reason",
      values_to = "proportion"
    )
  
  ggplot(reasons_long, aes(x = reason, y = proportion, fill = lgbt_status)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Reasons for Moving by LGBT Status",
      x = "Reason",
      y = "Proportion",
      fill = "LGBT Status"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#############################################################################################################################
##                                          Main Analysis Workflow
#############################################################################################################################

# Example workflow
# 1. Download and import data
# pulse_url <- "https://www2.census.gov/programs-surveys/demo/datasets/hhp/2023/wk63/HPS_Week63_PUF_CSV.zip"
# csv_path <- download_pulse_data(pulse_url, "data_raw/week63.zip")
# pulse_data <- fread(csv_path)

# 2. Clean and process data
# pulse_clean <- clean_pulse_data(pulse_data)

# 3. Analyze emigration patterns
# emigration_analysis <- analyze_lgbt_emigration(pulse_clean)

# 4. Visualize results
# plot_moving_intentions(emigration_analysis$move_by_lgbt)
# plot_moving_reasons(emigration_analysis$reasons_by_lgbt)

# 5. Save results
# ggsave("figures/moving_intentions.png", width = 10, height = 6)
# ggsave("figures/moving_reasons.png", width = 10, height = 6)
# write_csv(emigration_analysis$move_by_lgbt, "results/move_by_lgbt.csv")
# write_csv(emigration_analysis$reasons_by_lgbt, "results/reasons_by_lgbt.csv")

#############################################################################################################################
##                                          Bibliography
#############################################################################################################################

# U.S. Census Household Pulse Survey data sources are listed in the original script
# For full bibliography, refer to the original documentation