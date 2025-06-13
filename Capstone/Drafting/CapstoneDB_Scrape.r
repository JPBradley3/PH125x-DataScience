# --------------------------
# R Script: Wrangle LGBT/Trans Data from Major US Sources
# Author: J.P. Bradley
# --------------------------



# Load packages
library(tidyverse)
library(haven)
library(readxl)
library(jsonlite)
library(janitor)
library(httr)

# Helper function: Download if absent
download_if_needed <- function(local_path, url) {
  if(!file.exists(local_path)) {
    message(sprintf("Downloading: %s", url))
    dir.create(dirname(local_path), showWarnings=FALSE, recursive=TRUE)
    download.file(url, destfile=local_path, mode = "wb")
  }
}

# ---------------------------
# 1. Williams Institute UCLA
wi_lgbt_path <- "data/williams_lgbt_data.csv"
wi_lgbt_url <- "https://williamsinstitute.law.ucla.edu/publications/"  # Starting page. Update to a DIRECT CSV file URL or download 'williams_lgbt_data.csv' manually to 'data/' folder.
download_if_needed(wi_lgbt_path, wi_lgbt_url)
if (file.exists(wi_lgbt_path)) {
  wi_lgbt <- read_csv(wi_lgbt_path) %>% clean_names()
}

# ---------------------------
# 2. U.S. Census ACS Survey (PUMS)
acs_pums_path <- "data/acs_pums.csv"
acs_pums_url <- "https://www2.census.gov/programs-surveys/acs/data/pums/2022/1-Year/csv_pus.zip"
download_if_needed("data/acs_pus.zip", acs_pums_url)
if (file.exists("data/acs_pus.zip")) {
  unzip("data/acs_pus.zip", files = "psam_pusa.csv", exdir = "data")
  file.rename("data/psam_pusa.csv", acs_pums_path)
  acs_pums <- read_csv(acs_pums_path) %>% clean_names()
}

# ---------------------------
# 3. CDC BRFSS
brfss_data_path <- "data/brfss_2022.XPT"
brfss_url <- "https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022.XPT"
download_if_needed(brfss_data_path, brfss_url)
if (file.exists(brfss_data_path)) {
  brfss <- read_xpt(brfss_data_path) %>% clean_names()
}

# ---------------------------
# 4. CDC YRBS
yrbs_data_path <- "data/yrbs_2021.csv"
yrbs_url <- "https://www.cdc.gov/healthyyouth/data/yrbs/files/yrbs2021national.csv"
download_if_needed(yrbs_data_path, yrbs_url)
if (file.exists(yrbs_data_path)) {
  yrbs <- read_csv(yrbs_data_path) %>% clean_names()
}

# ---------------------------
# 5. Household Pulse Survey
pulse_path <- "data/pulse_survey.csv"
pulse_url <- "https://www.census.gov/programs-surveys/household-pulse-survey/data.html" # Starting page. Find a specific weekly/phase PUMS CSV, then update to its DIRECT URL or download 'pulse_survey.csv' manually.
download_if_needed(pulse_path, pulse_url)
if (file.exists(pulse_path)) {
  pulse <- read_csv(pulse_path) %>% clean_names()
}

# ---------------------------
# 6. HRC (Human Rights Campaign) LGBTQ Youth Reports
hrc_youth_path <- "data/hrc_lgbtq_youth.xlsx"
hrc_youth_url <- "https://www.hrc.org/resources" # Starting page. Find specific report/data. Update to a DIRECT .xlsx file URL or download 'hrc_lgbtq_youth.xlsx' manually.
download_if_needed(hrc_youth_path, hrc_youth_url)
if (file.exists(hrc_youth_path)) {
  hrc_youth <- read_excel(hrc_youth_path) %>% clean_names()
}

# ---------------------------
# 7. Fenway Institute Transgender Health
fenway_path <- "data/fenway_trans_research.csv"
fenway_url <- "https://fenwayhealth.org/the-fenway-institute/research/" # Starting page. Find specific research data. Update to a DIRECT CSV file URL or download 'fenway_trans_research.csv' manually.
download_if_needed(fenway_path, fenway_url)
if (file.exists(fenway_path)) {
  fenway <- read_csv(fenway_path) %>% clean_names()
}

# ---------------------------
# 8. Gallup Polls
gallup_path <- "data/gallup_lgbt.xlsx"
gallup_url <- "https://news.gallup.com/topic/lgbt_rights.aspx" # Starting page. Find specific poll data. Update to a DIRECT .xlsx file URL or download 'gallup_lgbt.xlsx' manually/extract data.
download_if_needed(gallup_path, gallup_url)
if (file.exists(gallup_path)) {
  gallup <- read_excel(gallup_path) %>% clean_names()
}

# ---------------------------
# 9. Additional Major Transgender Data Sources
usts_path <- "data/usts_2015.sav"
usts_url <- "https://www.ustranssurvey.org/reports" # USTS data requires registration (see site). Once registered, you might get a direct download link for the .sav file to use here, or download manually.
download_if_needed(usts_path, usts_url)
if (file.exists(usts_path)) {
  usts <- read_sav(usts_path) %>% clean_names()
}

pew_path <- "data/pew_lgbtq.csv"
pew_url <- "https://www.pewresearch.org/download-datasets/" # Starting page. Find the specific LGBTQ dataset, get its DIRECT download URL and use here, or download 'pew_lgbtq.csv' manually.
download_if_needed(pew_path, pew_url)
if (file.exists(pew_path)) {
  pew <- read_csv(pew_path) %>% clean_names()
}

# ICPSR example (if zipped CSV)
# unzip("data/icpsr_lgbt.zip", exdir = "data/icpsr")
# icpsr <- read_csv("data/icpsr/icpsr_lgbt.csv") %>% clean_names()

# GLAAD (if API/JSON)
# response <- fromJSON("https://glaad.org/api/data")
# glaad <- as_tibble(response$data) %>% clean_names()

# ---------------------------
# Standardize Key Variables (example)
## Custom variable selection, ID columns, and harmonization will be needed!
## E.g., selecting age, gender, sexual orientation, location, weights, etc.
standardize_vars <- function(df) {
  df %>%
    rename_with(~str_replace_all(., c("gender_identity" = "gender_id",
                                      "sex_at_birth" = "birth_sex",
                                      "respondent_id" = "id")))
}

wi_lgbt_std     <- standardize_vars(wi_lgbt)
acs_pums_std    <- standardize_vars(acs_pums)
brfss_std       <- standardize_vars(brfss)
yrbs_std        <- standardize_vars(yrbs)
pulse_std       <- standardize_vars(pulse)
hrc_youth_std   <- standardize_vars(hrc_youth)
fenway_std      <- standardize_vars(fenway)
gallup_std      <- standardize_vars(gallup)
usts_std        <- standardize_vars(usts)
pew_std         <- standardize_vars(pew)
# etc...

# ---------------------------
# Combine Datasets (optional - "long" format with Source variable)
all_lgbt_data <- bind_rows(
  wi_lgbt_std %>% mutate(source = "WI_LGBT"),
  acs_pums_std %>% mutate(source = "ACS_PUMS"),
  brfss_std %>% mutate(source = "BRFSS"),
  yrbs_std %>% mutate(source = "YRBS"),
  pulse_std %>% mutate(source = "PulseSurvey"),
  hrc_youth_std %>% mutate(source = "HRC_Youth"),
  fenway_std %>% mutate(source = "Fenway"),
  gallup_std %>% mutate(source = "Gallup"),
  usts_std %>% mutate(source = "USTS2015"),
  pew_std %>% mutate(source = "PewResearch")
  # continue ...
)

# Preview the final combined data
glimpse(all_lgbt_data)

# Save combined wrangled data
write_csv(all_lgbt_data, "outputs/all_lgbt_data_combined.csv")