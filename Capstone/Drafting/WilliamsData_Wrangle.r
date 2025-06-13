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
##                    Williams Institute datasets on LGBT demographics and
##                    transgender population estimates.
##
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
##    The Williams Institute UCLA
##
##    • LGBT Data & Demographics
##        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/37166
##
##    • U.S. Transgender Population Estimates
##        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/38853
##        ◦ https://williamsinstitute.law.ucla.edu/publications/adult-lgbt-pop-us/
##
##    • GENIUSS Group (Gender Identity in U.S. Surveillance)
##        ◦ https://www.icpsr.umich.edu/web/RCMD/studies/37229
##        ◦ https://www.icpsr.umich.edu/web/RCMD/studies/34363
##
##    • TransPop Survey Overview
##        ◦ https://www.icpsr.umich.edu/web/DSDR/studies/38421
##
##
###################################################################################
###################################################################################

# ==============================
# LOAD LIBRARIES
# ==============================
library(tidyverse)   # Core data manipulation and visualization (includes dplyr, ggplot2, etc.)
library(haven)       # For reading SPSS/Stata/SAS files, useful for survey microdata
library(ggplot2)     # Flexible system for building plots layer by layer
library(maps)        # Contains US state/county polygon data for mapping
library(viridis)     # Colorblind-friendly palettes for plots and maps
library(scales)      # Tools for axis scaling, format percent, comma, etc.
library(patchwork)   # Allows you to easily combine multiple ggplots

# ==============================
# SET FILE PATHS
# ==============================
# Set up directory paths for your data files, for easier reading/loading
base_path <- "C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data"
path_lgbt_demographics <- file.path(base_path, "DSDR-37166", "37166-0007-Data.rda")
path_transpop <- file.path(base_path, "DSDR-38421", "38421-0001-Data.rda")

# ==============================
# LOAD MICRODATA
# ==============================
load(path_lgbt_demographics)      # Loads R objects from the .rda file into the workspace
lgbt_demographics_df <- da37166.0007 # Assign to a more convenient name

load(path_transpop)               # Do the same for the second .rda file
transpop_df <- da38421.0001

# ==============================
#  LOAD ICPSR DATA
# ==============================

# Adjust the following to your ICPSR path and object name as needed!
load("C:/Users/Parker/Downloads/ICPSR37938/37938-0001-Data.rda")
if (!exists("da37938.0001")) stop("da37938.0001 not found!")
icpsr_df <- da37938.0001

# ==============================
#  PREPARE WILLIAMS DATA
# ==============================

lgbt_data <- tribble(
  ~State, ~Percent_LGBT, ~LGBT_Total, ~LGB_Total, ~LGB_Cisgender, ~LGB_Trans, ~Transgender_Total, ~Trans_Straight_Other, ~Trans_LGB,
"US",                4.5, 11343000, 10388000, 9946000, 392000, 1397150, 1065000, 392000,
"Alabama",           3.1, 117000, 101000, 95000, 6000, 22500, 16000, 6000,
"Alaska",            3.7, 21000, 19000, 18000, 1000, 2700, 2000, 1000,
"Arizona",           4.5, 242000, 220000, 212000, 9000, 30550, 19400, 9000,
"Arkansas",          3.3, 76000, 66000, 62000, 4000, 13400, 9000, 4000,
"California",        5.3, 1615000, 1458000, 1397000, 61000, 218400, 157000, 61000,
"Colorado",          4.6, 200000, 185000, 179000, 6000, 20850, 15000, 6000,
"Connecticut",       3.9, 111000, 102000, 99000, 3000, 12400, 9300, 3000,
"Delaware",          4.5, 34000, 31000, 30000, 1000, 4550, 3000, 1000,
"District of Columbia", 9.8, 56000, 45000, 41000, 4000, 14550, 10000, 4000,
"Florida",           4.6, 790000, 700000, 672000, 28000, 100300, 72000, 28000,
"Georgia",           4.5, 356000, 316000, 301000, 16000, 55550, 40000, 16000,
"Hawaii",            4.6, 52000, 47000, 46000, 1000, 8549, 7500, 1000,
"Idaho",             2.8, 38000, 32000, 31000, 1000, 4750, 3000, 1000,
"Illinois",          4.3, 426000, 389000, 376000, 14000, 62950, 48900, 14000,
"Indiana",           4.5, 227000, 209000, 202000, 8000, 29950, 20000, 8000,
"Iowa",              3.6, 87800, 79900, 77300, 3000, 11700, 8500, 3000,
"Kansas",            3.3, 77000, 67000, 63000, 3000, 10850, 7000, 3000,
"Kentucky",          3.1, 117000, 104000, 99000, 5000, 15900, 9000, 5000,
"Louisiana",         3.9, 139000, 124000, 118000, 6000, 20000, 15000, 6000,
"Maine",             3.4, 53800, 48800, 46700, 2000, 6800, 4800, 2000,
"Maryland",          4.2, 198000, 182000, 175000, 8000, 22900, 16200, 8000,
"Massachusetts",     5.4, 296000, 275000, 267000, 8000, 32250, 22900, 8000,
"Michigan",          4.0, 311000, 288000, 279000, 9000, 34750, 24900, 9000,
"Minnesota",         4.1, 171000, 162000, 155000, 7000, 24250, 17200, 7000,
"Mississippi",       3.1, 81000, 73400, 71400, 2000, 11000, 9000, 2000,
"Missouri",          3.8, 184000, 162000, 155000, 7000, 25550, 18300, 7000,
"Montana",           2.9, 24000, 22000, 21000, 1000, 2700, 2000, 1000,
"Nebraska",          3.8, 55000, 51000, 49000, 2000, 5400, 4000, 2000,
"Nevada",            5.5, 127000, 118000, 114000, 4000, 12750, 9000, 4000,
"New Hampshire",     4.7, 61000, 57000, 55000, 2000, 8300, 6000, 2000,
"New Jersey",        4.5, 298000, 270000, 259000, 11000, 30300, 19200, 11000,
"New Mexico",        4.5, 77000, 66400, 62800, 3000, 11550, 7800, 3000,
"New York",          5.2, 880000, 800000, 769000, 31000, 90600, 59500, 31000,
"North Carolina",    4.4, 319000, 287000, 274000, 13000, 44750, 32000, 13000,
"North Dakota",      3.6, 16000, 14600, 14000, 600, 2100, 1500, 600,
"Ohio",              4.3, 389000, 361000, 349000, 12000, 38950, 29000, 12000,
"Oklahoma",          3.8, 113000, 104000, 100000, 4000, 18800, 15000, 4000,
"Oregon",            4.6, 183000, 169000, 163000, 6000, 19750, 14000, 6000,
"Pennsylvania",      4.1, 426000, 385000, 373000, 12000, 52700, 40600, 12000,
"Rhode Island",      4.9, 64000, 59200, 57000, 2000, 6900, 4800, 2000,
"South Carolina",    3.5, 124000, 112000, 107000, 4000, 14300, 10100, 4000,
"South Dakota",      3.6, 24600, 22300, 21700, 600, 3400, 2800, 600,
"Tennessee",         4.3, 191000, 177000, 170000, 7000, 31250, 23500, 7000,
"Texas",             4.1, 858000, 796000, 761000, 35000, 125350, 90300, 35000,
"Utah",              3.4, 82800, 76700, 74700, 2000, 10500, 8000, 2000,
"Vermont",           5.3, 26700, 24700, 23700, 1000, 3400, 2500, 1000,
"Virginia",          3.9, 257000, 243000, 233000, 10000, 34500, 25000, 10000,
"Washington",        5.2, 290000, 266000, 257000, 9000, 39150, 29200, 9000,
"West Virginia",     3.9, 63000, 58000, 56000, 2000, 6700, 4700, 2000,
"Wisconsin",         3.8, 158000, 147000, 142000, 5000, 19750, 14200, 5000,
"Wyoming",           3.3, 15000, 14000, 13000, 1000, 1460, 1000, 1000
)

# ==============================
#  CLEAN ICPSR DATA
# ==============================
#names(icpsr_df) <- trimws(names(icpsr_df))
#if ("GZIPSTATE" %in% names(icpsr_df)) {
#  icpsr_df <- icpsr_df %>% rename(state_abbr = GZIPSTATE)
#} else {
#  stop("Couldn't find a state abbreviation column. Columns: ", paste(names(icpsr_df), collapse=", "))
#}

# Map abbreviation to full state name
#state_abbr_to_name <- tibble::tibble(
#    state_abbr = state.abb,
#    state_name = state.name
#) %>% 
#  bind_rows(tibble::tibble(state_abbr = "DC", state_name = "District of Columbia"))

#icpsr_df <- icpsr_df %>%
#    left_join(state_abbr_to_name, by = "state_abbr")

# ==============================
# FIND THE CORRECT COLUMN NAME
# ==============================
cat("\n--- All columns in lgbt_demographics_df ---\n")
print(names(lgbt_demographics_df)) # List all columns so you can look for a gender identity variable

# ==============================
# SUBSET DATA FOR ANALYSIS
# ==============================
# Find the specific column identifying gender/gender identity (replace this as needed)
gender_identity_column <- "W1SEX_GENDER" 

# Filter out any respondent whose gender identity indicates "Transgender" (case-insensitive)
lgb_df <- lgbt_demographics_df %>%
  filter(!grepl("Transgender", .data[[gender_identity_column]], ignore.case = TRUE))

# For consistency/clarity, rename the trans pop dataframe
trans_df <- transpop_df

# Output counts for validation, helps check that filtering/data loading worked
cat(sprintf("\nTotal LGBT respondents: %d", nrow(lgbt_demographics_df)))
cat(sprintf("\nFiltered LGB respondents: %d", nrow(lgb_df)))
cat(sprintf("\nTotal Transgender respondents: %d\n", nrow(trans_df)))


# ==============================
# TRANSGENDER ANALYSIS (GMILESAWAY)
# ==============================
# This plot visualizes the distribution of miles to the nearest LGBT health center
# among transgender respondents, grouped by census region.
ggplot(trans_df, aes(x = GCENREG, y = GMILESAWAY)) +
  geom_boxplot(fill = "skyblue", outlier.shape = 1, outlier.size = 2) + # Boxplot of miles by region
  scale_y_log10(labels = scales::comma) + # Use log scale, for right-skewed data like distances
  labs(x = "Census Region", 
       y = "Miles to Nearest LGBT Health Center (log scale)", 
       title = "Transgender Respondents: Distance to Nearest LGBT Health Center") +
  theme_minimal() # Minimal, clean look

# ==============================
# LGB ANALYSIS (using AGE as example variable)
# ==============================
# Here, plot the age distribution by census region for LGB respondents
ggplot(lgb_df, aes(x = GCENREG, y = W1AGE)) +
  geom_boxplot(fill = "lightcoral") +      # Boxplot of ages per region
  labs(x = "Census Region", 
       y = "Age", 
       title = "LGB Respondents: Age Distribution by Region") +
  theme_minimal()

# ==============================
# STATE-LEVEL AGGREGATE MAPS
# ==============================
# Prepare state lookup/reference tables and population estimates for merging
# These tables connect state FIPS codes to names and provide trans/LGB numbers

state_lookup_df <- tribble(
    ~state_fips, ~state_name,
    "01", "Alabama",    "02", "Alaska",     "04", "Arizona",   "05", "Arkansas",    "06", "California",
    "08", "Colorado",   "09", "Connecticut","10", "Delaware",  "11", "District of Columbia", "12", "Florida",
    "13", "Georgia",    "15", "Hawaii",     "16", "Idaho",     "17", "Illinois",    "18", "Indiana",
    "19", "Iowa",       "20", "Kansas",     "21", "Kentucky",  "22", "Louisiana",   "23", "Maine",
    "24", "Maryland",    "25", "Massachusetts", "26", "Michigan", "27", "Minnesota", "28", "Mississippi",
    "29", "Missouri",   "30", "Montana",    "31", "Nebraska",  "32", "Nevada",     "33", "New Hampshire",
    "34", "New Jersey", "35", "New Mexico", "36", "New York",  "37", "North Carolina", "38", "North Dakota",
    "39", "Ohio",       "40", "Oklahoma",   "41", "Oregon",    "42", "Pennsylvania", "44", "Rhode Island",
    "45", "South Carolina", "46", "South Dakota", "47", "Tennessee", "48", "Texas",
    "49", "Utah", "50", "Vermont", "51", "Virginia", "53", "Washington",
    "54", "West Virginia", "55", "Wisconsin", "56", "Wyoming"
)

trans_geo_df <- tribble(
    ~state_fips, ~trans_pop_estimate,
    "01", 16100,  "02", 3000,   "04", 36900,   "05", 12000,    "06", 192900,
    "08", 27400,  "09", 14100,  "10", 4100,    "12", 121100,   "13", 55500,
    "15", 6800,   "16", 6400,   "17", 52200,   "18", 26800,    "19", 11900,
    "20", 10500,  "21", 16700,  "22", 18100,   "23", 6000,     "24", 27600,
    "25", 30000,  "26", 37600,  "27", 22700,   "28", 11100,    "29", 23500,
    "30", 4200,   "31", 7100,   "32", 14600,   "33", 5700,     "34", 38700,
    "35", 9000,   "36", 94800,  "37", 47200,   "38", 2500,     "39", 44000,
    "40", 16000,  "41", 20500,  "42", 48500,   "44", 4300,     "45", 20600,
    "46", 3000,   "47", 28600,  "48", 125300,  "49", 11900,    "50", 2900,
    "51", 38100,  "53", 39800,  "54", 6300,    "55", 21100,    "56", 1900
)

lgb_geo_df <- tribble(
    ~state_fips, ~lgb_pop_estimate,
    "01", 137000,  "02", 27000,  "04", 314000,  "05", 85000,   "06", 2160000,
    "08", 263000,  "09", 148000, "10", 52000,   "11", 49000, "12", 1148000, "13", 497000,
    "15", 72000,   "16", 60000,  "17", 521000,  "18", 229000,  "19", 102000,
    "20", 92000,   "21", 133000, "22", 152000,  "23", 70000,   "24", 283000,
    "25", 357000,  "26", 366000, "27", 232000,  "28", 78000,   "29", 207000,
    "30", 42000,   "31", 62000,  "32", 148000,  "33", 64000,   "34", 368000,
    "35", 94000,   "36", 989000, "37", 438000,  "38", 22000,   "39", 412000,
    "40", 135000,  "41", 231000, "42", 466000,  "44", 57000,   "45", 159000,
    "46", 27000,   "47", 231000, "48", 1083000, "49", 112000,  "50", 35000,
    "51", 352000,  "53", 379000, "54", 48000,   "55", 205000,  "56", 17000
)

state_pop_df <- tribble(
    ~state_fips, ~pop_2020,
    "01", 5024279, "02", 733391,  "04", 7151502,  "05", 3011524,  "06", 39538223,
    "08", 5773714, "09", 3605944, "10", 989948,   "11", 689545,   "12", 21538187,
    "13", 10711908,"15", 1455271, "16", 1839106,  "17", 12812508, "18", 6785528,
    "19", 3190369, "20", 2937880, "21", 4505836,  "22", 4657757,  "23", 1362359,
    "24", 6177224, "25", 7029917, "26", 10077331, "27", 5706494,  "28", 2961279,
    "29", 6154913, "30", 1084225, "31", 1961504,  "32", 3104614,  "33", 1377529,
    "34", 9288994, "35", 2117522, "36", 20201249, "37", 10439388, "38", 779094,
    "39", 11799448,"40", 3959353, "41", 4237256,  "42", 13002700, "44", 1097379,
    "45", 5118425, "46", 886667,  "47", 6910840,  "48", 29145505, "49", 3271616,
    "50", 643077,  "51", 8631393, "53", 7693612,  "54", 1793716,  "55", 5893718,
    "56", 576851
)

# Join tables to assemble a single dataframe with state name, estimates, and percentages
# Set national subgroup proportions (as % of all adults)
final_df <- state_lookup_df %>%
    left_join(trans_geo_df, by = "state_fips") %>%
    left_join(lgb_geo_df, by = "state_fips") %>%
    left_join(state_pop_df, by = "state_fips") %>%
    mutate(
        `% Transgender` = 100 * trans_pop_estimate / pop_2020,
        `% LGB` = 100 * lgb_pop_estimate / pop_2020,
        state_name_lc = tolower(state_name)
    ) %>%
    pivot_longer(
        cols = c("% Transgender", "% LGB"),
        names_to = "population_group",
        values_to = "percent"
    )


# Get US state boundaries as polygons for plotting
us_states <- map_data("state")

# Join the percent data onto map polygons using region (state name, lowercased)
plot_df <- us_states %>%
  left_join(final_df, by = c("region" = "state_name_lc"), relationship = "many-to-many")

# Build the map for '% Transgender' (faceting comes later with patchwork)
# Map for % Transgender
p_trans <- plot_df %>%
  filter(population_group == "% Transgender") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "gray70") +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent_format(scale = 1),
    na.value = "white",
    name = "% Transgender"
  ) +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "% Transgender Population by State") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Map for % LGB
p_lgb <- plot_df %>%
  filter(population_group == "% LGB") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "gray70") +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent_format(scale = 1),
    na.value = "white",
    name = "% LGB"
  ) +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "% LGB Population by State") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Combine the two state maps side-by-side with a shared title & subtitle
combined_plot <- p_trans + p_lgb +
  patchwork::plot_annotation(
    title = "Estimated Percentage of Adults by State",
    subtitle = "Comparison of Transgender and LGB Populations",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16),
                  plot.subtitle = element_text(hjust = 0.5, size = 12))
  )
print(combined_plot)

# ==============================
# END OF CODE
# ==============================