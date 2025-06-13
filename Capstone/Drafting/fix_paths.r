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
    # Force ALL column names to be lowercase
    rename_with(tolower)
  
  # Select the columns we need
  data %>%
    select(
      # Survey design variables
      year = iyear,
      state = `_state`,
      weight = `_llcpwt`,
      strata = `_ststr`,
      psu = `_psu`,
      
      # SOGI variables 
      sex_orient = any_of(c("sexorien", "sxorient")), 
      transgender = any_of(c("trnsgndr", "transgen"))
    ) %>%
    mutate(year = as.integer(year))
}

# Define the years to analyze
years_to_process <- 2014:2022

# Use map() to apply the function to each year and combine the results
brfss_raw <- map_df(years_to_process, load_brfss_csv)

# Save the combined raw data as a feather file for faster loading in the future
write_rds(brfss_raw, "C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data_processed/brfss_2014-2022_raw.rds")

# Print summary to confirm data was loaded
cat("Data loaded successfully!\n")
cat("Number of rows:", nrow(brfss_raw), "\n")
cat("Number of columns:", ncol(brfss_raw), "\n")
cat("Years included:", paste(sort(unique(brfss_raw$year)), collapse=", "), "\n")