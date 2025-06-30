## ----setup_libraries, echo=FALSE, results='asis'---------------------------------------------------------------------------
# Load necessary libraries
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gt")) install.packages("gt")
if (!require("knitr")) install.packages("knitr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
if (!require("reshape2")) install.packages("reshape2")
if (!require("ranger")) install.packages("ranger")


library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gt)
library(knitr)
library(tidyverse)
library(ggcorrplot)
library(reshape2)
library(ranger)


## ----setup_download, echo=FALSE, results='asis'----------------------------------------------------------------------------
# Step 1: Download the zip file
zip_url <- "https://archive.ics.uci.edu/static/public/352/online+retail.zip"
zip_file <- "online_retail.zip"

if (!file.exists(zip_file)) {
  download.file(zip_url, destfile = zip_file, mode = "wb")
}

# Step 2: Unzip the file
unzip(zip_file, exdir = ".", overwrite = TRUE)


## ----setup_load, echo=TRUE, results='asis'---------------------------------------------------------------------------------
# Step 3: Load the Excel file that was extracted
excel_file <- "Online Retail.xlsx"

retail <- read_excel(excel_file)

# Check the structure of the data
glimpse(retail)


## ----setup_table_formatted, echo=TRUE, out.width='100%'--------------------------------------------------------------------
head(retail, 5) %>%
  gt() %>%
  tab_header(
    title = "First 5 Rows of Raw Retail Data",
    subtitle = "Preview using the 'gt' package"
  ) %>%
  fmt_currency(
    columns = UnitPrice,
    currency = "GBP" # Assuming UK currency based on data
  ) %>%
  fmt_number(
    columns = Quantity,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  tab_source_note(
    source_note = "Raw data glimpse before cleaning."
  )


## ----data-cleaning, echo=TRUE----------------------------------------------------------------------------------------------
# Remove rows with missing CustomerID, negative or zero quantity, cancelled invoices, and keep only UK customers
clean_retail <- retail %>%
  filter(!is.na(CustomerID),
         Quantity > 0,
         !grepl("^C", InvoiceNo),
         Country == "United Kingdom")

clean_retail_head_gt <- head(clean_retail) %>%
  gt() %>%
  # You can add more formatting if you like, e.g.:
  tab_header(title = "First Rows of Cleaned Retail Data (UK Sales)") %>%
  fmt_currency(columns = UnitPrice, currency = "GBP") %>% # Assuming UnitPrice and currency in GBP
  fmt_number(columns = Quantity, decimals = 0) # Format Quantity as whole numbers

# Print the gt table
clean_retail_head_gt


## ----additional-cleaning1, echo=TRUE---------------------------------------------------------------------------------------
clean_retail <- retail %>%
  filter(UnitPrice > 0) %>%
  filter(!is.na(Description) & Description != "")


## ----additional-cleaning2, echo=TRUE---------------------------------------------------------------------------------------
# Remove extremely high quantities (top 1% as outliers)
clean_retail <- clean_retail %>%
  filter(Quantity < quantile(Quantity, 0.99, na.rm=TRUE))

# Remove special (non-product) stock codes
special_items <- c(
  "POST", "D", "DOT", "M", "S", "AMAZONFEE", "m", "DCGSSBOY",
  "DCGSSGIRL", "PADS", "B", "CRUK", "C2", "BANK CHARGES", "gift_0001"
)
clean_retail <- clean_retail %>%
  filter(!StockCode %in% special_items)


## ----additional-cleaning3, echo=TRUE---------------------------------------------------------------------------------------
# Remove descriptions indicating test or adjustment transactions
clean_retail <- clean_retail %>%
  filter(!grepl("ADJUST|TEST|test|Test", Description, ignore.case = TRUE))

# Keep only well-formed invoice dates within range
clean_retail <- clean_retail %>%
  mutate(InvoiceDate = as.POSIXct(InvoiceDate)) %>%
  filter(InvoiceDate >= "2010-01-01" & InvoiceDate <= "2012-01-01")


## ----additional-cleaning4, echo=TRUE---------------------------------------------------------------------------------------
# Remove duplicate InvoiceNo + StockCode + CustomerID (keeping first)
clean_retail <- clean_retail %>%
  distinct(InvoiceNo, StockCode, CustomerID, .keep_all = TRUE)

# Create TotalPrice and remove very low-value transactions
clean_retail <- clean_retail %>%
  mutate(TotalPrice = Quantity * UnitPrice) %>%
  filter(TotalPrice > 0.01)


## ----additional-cleaning5, echo=TRUE---------------------------------------------------------------------------------------
# Force key fields to character for consistency
clean_retail <- clean_retail %>%
  mutate(
    CustomerID = as.character(CustomerID),
    InvoiceNo = as.character(InvoiceNo),
    StockCode = as.character(StockCode)
  )


## ----additional-cleaning6, echo=TRUE---------------------------------------------------------------------------------------
cleaning_overview <- data.frame(
  Metric = c("Original rows", "Cleaned rows", "Percentage retained"),
  Value = c(
    nrow(retail),
    nrow(clean_retail),
    round(nrow(clean_retail) / nrow(retail) * 100, 2)
  )
)
gt_cleaning_overview <- cleaning_overview %>%
  gt() %>%
  cols_label(
    Metric = md("**Metric**"),
    Value = md("**Value**")
  ) %>%
  tab_header(
    title = md("### Cleaning Overview")
  )
gt_cleaning_overview


## ----additional-cleaning7, echo=TRUE---------------------------------------------------------------------------------------
missing_vals <- data.frame(
  Variable = names(clean_retail),
  Missing = as.integer(colSums(is.na(clean_retail)))
)
gt_missing_vals <- missing_vals %>%
  gt() %>%
  cols_label(
    Variable = md("**Variable**"),
    Missing = md("**Missing Values**")
  ) %>%
  tab_header(
    title = md("### Missing Values per Column")
  )
gt_missing_vals


## ----additional-cleaning8, echo=TRUE---------------------------------------------------------------------------------------
if (nrow(clean_retail) > 0) {
  summary_tbl <- bind_rows(
    tibble(
      Variable = "Quantity",
      Min = min(clean_retail$Quantity, na.rm = TRUE),
      Q1 = quantile(clean_retail$Quantity, 0.25, na.rm = TRUE),
      Median = median(clean_retail$Quantity, na.rm = TRUE),
      Mean = mean(clean_retail$Quantity, na.rm = TRUE),
      Q3 = quantile(clean_retail$Quantity, 0.75, na.rm = TRUE),
      Max = max(clean_retail$Quantity, na.rm = TRUE),
      N = length(clean_retail$Quantity)
    ),
    tibble(
      Variable = "UnitPrice",
      Min = min(clean_retail$UnitPrice, na.rm = TRUE),
      Q1 = quantile(clean_retail$UnitPrice, 0.25, na.rm = TRUE),
      Median = median(clean_retail$UnitPrice, na.rm = TRUE),
      Mean = mean(clean_retail$UnitPrice, na.rm = TRUE),
      Q3 = quantile(clean_retail$UnitPrice, 0.75, na.rm = TRUE),
      Max = max(clean_retail$UnitPrice, na.rm = TRUE),
      N = length(clean_retail$UnitPrice)
    )
  )
  gt_summary <- summary_tbl %>%
    gt() %>%
    fmt_number(
      columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max"),
      decimals = 2
    ) %>%
    cols_label(
      Variable = md("**Variable**"),
      Min = md("**Min**"),
      Q1 = md("**1st Quartile**"),
      Median = md("**Median**"),
      Mean = md("**Mean**"),
      Q3 = md("**3rd Quartile**"),
      Max = md("**Max**"),
      N = md("**N**")
    ) %>%
    tab_header(
      title = md("### Summary Statistics for `Quantity` and `UnitPrice`")
    )
  gt_summary
} else {
  cat("The clean_retail data frame is empty — summaries not available.\n")
}


## ----additional-cleaning9, echo=TRUE---------------------------------------------------------------------------------------
if (nrow(clean_retail) > 0) {
  var_names <- names(clean_retail)
  var_types <- sapply(clean_retail, function(x) class(x)[1])
  str_data <- data.frame(
    Variable = var_names,
    DataType = var_types,
    Details = NA_character_,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(str_data))) {
    var_name <- str_data$Variable[i]
    var_type <- str_data$DataType[i]
    sample_values <- head(na.omit(clean_retail[[var_name]]), 3)
    if (length(sample_values) == 0) {
      str_data$Details[i] <- "No data/All NA"
    } else if (var_type %in% c("numeric", "integer")) {
      str_data$Details[i] <- paste0(var_type, " ", paste(round(sample_values, 2), collapse = ", "))
    } else if (var_type %in% c("character", "factor")) {
      str_data$Details[i] <- paste0(var_type, " \"", paste(sample_values, collapse = "\", \""), "\"")
    } else if (var_type == "POSIXct") {
      str_data$Details[i] <- paste0(var_type, ", format: \"", format(sample_values[1], "%Y-%m-%d %H:%M:%S"), "\" ...")
    } else {
      str_data$Details[i] <- var_type
    }
  }
  gt_table <- str_data %>%
    gt() %>%
    cols_label(
      Variable = md("**Variable**"),
      DataType = md("**Data Type**"),
      Details = md("**Details / Sample Values**")
    ) %>%
    tab_header(
      title = md("### Structure of `clean_retail` Data Frame"),
      subtitle = md("A summary of variable types and example data after cleaning.")
    ) %>%
    tab_source_note(
      source_note = md("Sample values are illustrative and derived from a subset of the cleaned data. Values reflect the data after all specified cleaning steps.")
    ) %>%
    opt_table_lines("none") %>%
    opt_row_striping()
  gt_table
} else {
  cat("No data remaining in clean_retail after filtering. Cannot display structure table.\n")
}


## ----data-refinement1, echo=TRUE-------------------------------------------------------------------------------------------
customer_summary <- clean_retail %>%
  group_by(CustomerID) %>%
  summarise(
    recency = as.numeric(difftime(max(clean_retail$InvoiceDate), max(InvoiceDate), units = "days")),
    n_orders = n_distinct(InvoiceNo),
    n_transactions = n(),
    n_unique_products = n_distinct(StockCode),
    total_spent = sum(TotalPrice),
    avg_order_value = total_spent / n_orders,
    avg_item_price = mean(UnitPrice),
    avg_items_per_order = n_transactions / n_orders,
    days_as_customer = as.numeric(difftime(max(InvoiceDate), min(InvoiceDate), units = "days")),
    first_purchase = min(InvoiceDate),
    last_purchase = max(InvoiceDate)
  ) %>%
  mutate(
    purchase_frequency_rate = ifelse(days_as_customer > 0, n_orders / days_as_customer, 0)
  )


## ----data-refinement2, echo=TRUE-------------------------------------------------------------------------------------------
summary_stats <- tibble(
  Metric = names(customer_summary)[-1],  # exclude CustomerID
  Mean = sapply(customer_summary[,-1], mean, na.rm = TRUE),
  Median = sapply(customer_summary[,-1], median, na.rm = TRUE),
  Min = sapply(customer_summary[,-1], min, na.rm = TRUE),
  Max = sapply(customer_summary[,-1], max, na.rm = TRUE)
)
summary_stats %>%
  gt() %>%
  fmt_number(everything(), decimals = 2) %>%
  tab_header(
    title = md("### Customer-Level Summary Statistics")
  ) %>%
  cols_label(
    Metric = md("**Metric**"),
    Mean = md("**Mean**"),
    Median = md("**Median**"),
    Min = md("**Min**"),
    Max = md("**Max**")
  )


## ----data-refinement3, echo=TRUE-------------------------------------------------------------------------------------------
customer_summary %>%
  head(10) %>% 
  gt() %>%
  tab_header(
    title = md("### Sample of Customer-Level Features")
  )


## ----data-refinement4, echo=TRUE-------------------------------------------------------------------------------------------
customer_rfm <- customer_summary %>%
  mutate(
    R_score = ntile(desc(recency), 5),
    F_score = ntile(n_orders, 5),
    M_score = ntile(total_spent, 5),
    RFM_score = paste0(R_score, F_score, M_score)
  )


## ----data-refinement5, echo=TRUE-------------------------------------------------------------------------------------------
customer_rfm %>%
  select(CustomerID, recency, n_orders, total_spent, R_score, F_score, M_score, RFM_score) %>%
  head(20) %>%
  gt() %>%
  tab_header(
    title = md("### Sample RFM Scores")
  )


## ----data-refinement6, echo=TRUE-------------------------------------------------------------------------------------------
dataset_stats <- tibble(
  Metric = c("Total transactions after cleaning", "Total unique customers", "Date range"),
  Value = c(
    nrow(clean_retail),
    nrow(customer_summary),
    paste(
      as.character(min(clean_retail$InvoiceDate)), 
      "to", 
      as.character(max(clean_retail$InvoiceDate))
    )
  )
)
dataset_stats %>%
  gt() %>%
  tab_header(
    title = md("### Cleaned Data Overview")
  ) %>%
  cols_label(
    Metric = md("**Metric**"),
    Value = md("**Value**")
  )


## ----data-refinement7, echo=TRUE-------------------------------------------------------------------------------------------
write.csv(clean_retail, "clean_retail_data.csv", row.names = FALSE)
write.csv(customer_rfm, "customer_rfm_data.csv", row.names = FALSE)


## ----exploratory-data-analysis1, echo=TRUE---------------------------------------------------------------------------------
# Create summary table for overview
dataset_overview <- data.frame(
  Metric = c(
    "Total transactions",
    "Unique customers",
    "Unique products",
    "Unique invoices",
    "Date range"
  ),
  Value = c(
    format(nrow(clean_retail), big.mark = ","),
    format(n_distinct(clean_retail$CustomerID), big.mark = ","),
    format(n_distinct(clean_retail$StockCode), big.mark = ","),
    format(n_distinct(clean_retail$InvoiceNo), big.mark = ","),
    paste(
      as.character(min(clean_retail$InvoiceDate)),
      "to",
      as.character(max(clean_retail$InvoiceDate))
    )
  )
)

# Display as gt table
dataset_overview %>%
  gt() %>%
  tab_header(title = "Dataset Overview")


## ----exploratory-data-analysis2, echo=TRUE---------------------------------------------------------------------------------
# Get summary stats for TotalPrice
tp_summary <- summary(clean_retail$TotalPrice)
tp_summary_names <- names(tp_summary)

# Build data frame for gt
transaction_values <- data.frame(
  Statistic = c(tp_summary_names, "Total revenue"),
  Value = c(
    as.character(tp_summary),
    paste0("£", format(sum(clean_retail$TotalPrice), big.mark = ",", nsmall = 2))
  ),
  row.names = NULL
)

# Display gt table
transaction_values %>%
  gt() %>%
  tab_header(title = "Transaction Values Summary")


## ----exploratory-data-analysis3, echo=TRUE---------------------------------------------------------------------------------
# Add date parts
clean_retail <- clean_retail %>%
  mutate(
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    Day = day(InvoiceDate),
    Weekday = wday(InvoiceDate, label = TRUE),
    Hour = hour(InvoiceDate),
    YearMonth = floor_date(InvoiceDate, "month")
  )


## ----exploratory-data-analysis4, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------
# Daily sales
daily_sales <- clean_retail %>%
  group_by(Date = as.Date(InvoiceDate)) %>%
  summarise(
    n_transactions = n(),
    total_revenue = sum(TotalPrice),
    n_customers = n_distinct(CustomerID)
  )

# Plot daily revenue trend
ggplot(daily_sales, aes(x = Date, y = total_revenue)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(title = "Daily Revenue Trend", x = "Date", y = "Revenue (£)") +
  theme_minimal()


## ----exploratory-data-analysis5, echo=TRUE---------------------------------------------------------------------------------
# Monthly sales
monthly_sales <- clean_retail %>%
  group_by(YearMonth) %>%
  summarise(
    n_transactions = n(),
    total_revenue = sum(TotalPrice),
    n_customers = n_distinct(CustomerID),
    avg_order_value = mean(TotalPrice)
  )

# Plot monthly revenue
ggplot(monthly_sales, aes(x = YearMonth, y = total_revenue)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(title = "Monthly Revenue", x = "Month", y = "Revenue (£)") +
  theme_minimal()


## ----exploratory-data-analysis6, echo=TRUE---------------------------------------------------------------------------------
hourly_pattern <- clean_retail %>%
  group_by(Hour) %>%
  summarise(
    avg_transactions = n() / n_distinct(as.Date(InvoiceDate)),
    avg_revenue = sum(TotalPrice) / n_distinct(as.Date(InvoiceDate))
  )
ggplot(hourly_pattern, aes(x = Hour, y = avg_revenue)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  labs(title = "Average Revenue by Hour of Day", x = "Hour", y = "Average Revenue (£)") +
  theme_minimal()


## ----exploratory-data-analysis7, echo=TRUE---------------------------------------------------------------------------------
weekday_pattern <- clean_retail %>%
  group_by(Weekday) %>%
  summarise(
    avg_transactions = n() / n_distinct(as.Date(InvoiceDate)),
    avg_revenue = sum(TotalPrice) / n_distinct(as.Date(InvoiceDate)),
    total_revenue = sum(TotalPrice)
  )

ggplot(weekday_pattern, aes(x = Weekday, y = total_revenue)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  labs(title = "Total Revenue by Day of Week", x = "Day of Week", y = "Total Revenue (£)") +
  theme_minimal()


## ----exploratory-data-analysis8, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------
top_products_qty <- clean_retail %>%
  group_by(StockCode, Description) %>%
  summarise(
    total_quantity = sum(Quantity),
    total_revenue = sum(TotalPrice),
    n_transactions = n(),
    n_customers = n_distinct(CustomerID)
  ) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

# Display with gt
top_products_qty %>%
  select(Product = Description, Quantity = total_quantity, Revenue = total_revenue) %>%
  gt() %>%
  tab_header(title = "Top 10 Products by Quantity Sold")


## ----exploratory-data-analysis9, echo=TRUE---------------------------------------------------------------------------------
customer_frequency <- clean_retail %>%
  group_by(CustomerID) %>%
  summarise(
    n_purchases = n_distinct(InvoiceNo),
    total_spent = sum(TotalPrice),
    first_purchase = min(InvoiceDate),
    last_purchase = max(InvoiceDate),
    customer_lifespan = as.numeric(difftime(last_purchase, first_purchase, units = "days"))
  )
# Display key customer stats with gt
customer_stats <- data.frame(
  Metric = c(
    "Average purchases per customer",
    "Average customer lifetime value (£)",
    "Average customer lifespan (days)"
  ),
  Value = c(
    round(mean(customer_frequency$n_purchases), 2),
    round(mean(customer_frequency$total_spent), 2),
    round(mean(customer_frequency$customer_lifespan), 2)
  )
)

customer_stats %>% gt() %>% tab_header(title = "Customer Behavior Statistics")


## ----exploratory-data-analysis10, echo=TRUE--------------------------------------------------------------------------------
# Basket analysis
basket_analysis <- clean_retail %>%
  group_by(InvoiceNo, CustomerID) %>%
  summarise(
    n_items = sum(Quantity),
    n_unique_items = n_distinct(StockCode),
    basket_value = sum(TotalPrice),
    .groups = 'drop'
  )

# Basket stats table
basket_stats <- data.frame(
  Metric = c(
    "Average items per basket",
    "Average unique items per basket",
    "Average basket value (£)"
  ),
  Value = c(
    round(mean(basket_analysis$n_items), 2),
    round(mean(basket_analysis$n_unique_items), 2),
    round(mean(basket_analysis$basket_value), 2)
  )
)

basket_stats %>%
  gt() %>%
  tab_header(title = "Basket Analysis")


## ----exploratory-data-analysis11, echo=TRUE--------------------------------------------------------------------------------
# Price distribution
ggplot(clean_retail %>% filter(UnitPrice < 20), aes(x = UnitPrice)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Unit Prices (< £20)", 
       x = "Unit Price (£)", y = "Frequency") +
  theme_minimal()


## ----exploratory-data-analysis12, echo=TRUE--------------------------------------------------------------------------------
# Quantity distribution
ggplot(clean_retail %>% filter(Quantity < 50), aes(x = Quantity)) +
  geom_histogram(bins = 50, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Quantities (< 50)", 
       x = "Quantity", y = "Frequency") +
  theme_minimal()


## ----exploratory-data-analysis13, echo=TRUE--------------------------------------------------------------------------------
monthly_pattern <- clean_retail %>%
  mutate(Month_name = month(InvoiceDate, label = TRUE)) %>%
  group_by(Month_name) %>%
  summarise(
    avg_daily_revenue = sum(TotalPrice) / n_distinct(as.Date(InvoiceDate)),
    total_revenue = sum(TotalPrice),
    .groups = 'drop'
  )

ggplot(monthly_pattern, aes(x = Month_name, y = avg_daily_revenue, group = 1)) +
  geom_line(color = "red", size = 1.5) +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Average Daily Revenue by Month", 
    x = "Month", y = "Average Daily Revenue (£)"
  ) +
  theme_minimal()


## ----exploratory-data-analysis14, echo=TRUE--------------------------------------------------------------------------------
summary_stats <- data.frame(
  Metric = c(
    "Total Revenue", "Number of Transactions", "Number of Customers", 
    "Number of Products", "Average Order Value", "Average Items per Order",
    "Average Customer Lifetime Value", "Average Purchase Frequency"
  ),
  Value = c(
    paste("£", format(sum(clean_retail$TotalPrice), big.mark=",", nsmall=2)),
    format(nrow(clean_retail), big.mark=","),
    format(n_distinct(clean_retail$CustomerID), big.mark=","),
    format(n_distinct(clean_retail$StockCode), big.mark=","),
    paste("£", round(mean(basket_analysis$basket_value), 2)),
    round(mean(basket_analysis$n_items), 2),
    paste("£", round(mean(customer_frequency$total_spent), 2)),
    round(mean(customer_frequency$n_purchases), 2)
  )
)

# Display with gt
summary_stats %>%
  gt() %>%
  tab_header(title = "Ecommerce Summary Statistics")


## ----exploratory-data-analysis15, echo=TRUE--------------------------------------------------------------------------------
# Best selling day
best_day <- daily_sales %>% arrange(desc(total_revenue)) %>% head(1)
cat("Best selling day:", as.character(best_day$Date), 
    "with revenue: £", format(best_day$total_revenue, big.mark=",", nsmall=2), "\n")

# Peak shopping hour
peak_hour <- hourly_pattern %>% arrange(desc(avg_revenue)) %>% head(1)
cat("Peak shopping hour:", peak_hour$Hour, ":00\n")

# Most popular day of week
popular_day <- weekday_pattern %>% arrange(desc(total_revenue)) %>% head(1)
cat("Most popular shopping day:", as.character(popular_day$Weekday), "\n")


## ----correlation-analysis1, echo=TRUE--------------------------------------------------------------------------------------
# Structure check & basic feature engineering
if (exists("customer_summary")) {

  # --- Show structure as gt table ---
  column_overview <- tibble(
    Variable = names(customer_summary),
    Type = sapply(customer_summary, function(x) class(x)[1]),
    Example = sapply(customer_summary, function(x) as.character(x[1]))
  )

  structure_gt <- column_overview %>%
    gt() %>%
    tab_header(
      title = "customer_summary Structure"
    )

  # --- Show number of rows as gt table ---
  row_count_gt <- tibble(
    `Number of Rows` = nrow(customer_summary)
  ) %>%
    gt() %>%
    tab_header(
      title = "customer_summary Row Count"
    )

  # Display the structure and row count gt tables
  print(structure_gt)
  print(row_count_gt)

  # --- Feature engineering ---
  correlation_data <- customer_summary %>%
    filter(!is.na(recency) & !is.na(n_orders) & !is.na(total_spent)) %>%
    mutate(
      avg_days_between_purchases = case_when(
        n_orders <= 1 ~ NA_real_,
        days_as_customer == 0 ~ 0,
        TRUE ~ days_as_customer / (n_orders - 1)
      ),
      product_diversity_ratio = case_when(
        n_transactions == 0 ~ 0,
        TRUE ~ n_unique_products / n_transactions
      ),
      spent_per_day_active = case_when(
        days_as_customer == 0 ~ total_spent,
        TRUE ~ total_spent / (days_as_customer + 1)
      ),
      price_sensitivity = case_when(
        avg_order_value == 0 ~ 0,
        TRUE ~ avg_item_price / avg_order_value
      ),
      purchase_consistency = case_when(
        days_as_customer == 0 ~ n_orders,
        TRUE ~ n_orders / ((days_as_customer + 1) / 30)
      )
    )

  # --- Correlation analysis ---
  cor_vars <- correlation_data %>%
    select(
      recency,
      n_orders,
      total_spent,
      avg_days_between_purchases,
      product_diversity_ratio,
      spent_per_day_active,
      price_sensitivity,
      purchase_consistency
    )

  cor_matrix <- cor(cor_vars, use = "complete.obs")
  cor_df <- cor_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable")

  correlation_gt <- cor_df %>%
    gt(rowname_col = "Variable") %>%
    tab_header(
      title = "Correlation Matrix of Customer Metrics"
    ) %>%
    fmt_number(
      columns = 2:ncol(cor_df),
      decimals = 2
    )

  # Display the correlation matrix gt table
  print(correlation_gt)
  invisible()

} else {
  stop("customer_summary data not found. Please load the data first.")
}



## ----correlation-analysis2, echo=TRUE--------------------------------------------------------------------------------------
# Select correlation variables & handle NAs
correlation_vars <- correlation_data %>%
  select(recency, n_orders, n_transactions, n_unique_products, 
         total_spent, avg_order_value, avg_item_price, avg_items_per_order, 
         days_as_customer, purchase_frequency_rate, 
         product_diversity_ratio, spent_per_day_active, 
         price_sensitivity, purchase_consistency)

# Show NA counts as a gt table
na_counts <- colSums(is.na(correlation_vars))
na_counts_df <- data.frame(
  Variable = names(na_counts),
  NA_Count = as.vector(na_counts)
)
na_counts_df %>%
  gt() %>%
  tab_header(
    title = "Missing Values per Variable"
  )

# Remove columns with >10% missing
threshold <- nrow(correlation_vars) * 0.1
cols_to_keep <- names(na_counts[na_counts < threshold])

correlation_data_clean <- correlation_vars %>%
  select(all_of(cols_to_keep)) %>%
  na.omit()

# Show cleaned data dimensions
cleaned_dimensions <- data.frame(
  Statistic = c("Rows in cleaned data", "Columns"),
  Value = c(nrow(correlation_data_clean), ncol(correlation_data_clean))
)
cleaned_dimensions %>%
  gt() %>%
  tab_header(title = "Cleaned Data Dimensions")


## ----correlation-analysis3, echo=TRUE--------------------------------------------------------------------------------------
if (nrow(correlation_data_clean) > 30) {
  cor_mat <- cor(correlation_data_clean, method = "pearson")
  cor_rounded <- round(cor_mat, 2)
  
  # Convert to a data frame, then to a tibble, and filter out self-correlations
  cor_long <- as.data.frame(as.table(cor_rounded)) %>%
    as_tibble() %>% # Explicitly convert to tibble
    filter(Var1 != Var2) # Use dplyr::filter to keep it a tibble
  
  cor_long$abs_cor <- abs(cor_long$Freq)
  
  # Explicitly call dplyr::slice() to prevent any masking issues
  top_cors <- cor_long %>%
    arrange(desc(abs_cor)) %>%
    dplyr::slice(1:10) %>% # <--- THE CRUCIAL CHANGE HERE
    select(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)
  
  top_cors %>%
    gt() %>%
    fmt_number(columns = Correlation, decimals = 2) %>%
    tab_header(
      title = "Top 10 Variable Pairs by Absolute Correlation"
    )
} else {
  cat("Not enough data for correlation analysis.\n")
}


## ----correlation-analysis4, echo=TRUE--------------------------------------------------------------------------------------
# Correlation plot (corrplot, basic)
cor_melted <- melt(cor_mat)
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  ggtitle("Correlation Heatmap of Customer Metrics") +
  coord_fixed()


## ----correlation-analysis5, echo=TRUE--------------------------------------------------------------------------------------
# Find all strong correlations (|r| > 0.5)
cor_upper <- cor_mat
cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
strong_cors <- which(abs(cor_upper) > 0.5, arr.ind = TRUE)

if (length(strong_cors) > 0) {
  strong_cor_df <- data.frame(
    Variable1 = rownames(cor_mat)[strong_cors[,1]],
    Variable2 = colnames(cor_mat)[strong_cors[,2]],
    Correlation = round(cor_mat[strong_cors], 2),
    stringsAsFactors = FALSE
  )

  
  strong_cor_df %>%
    gt() %>%
    fmt_number(columns = Correlation, decimals = 2) %>%
    tab_header(
      title = "Variable Pairs with Strong Correlation (|r| > 0.5)"
    )
} else {
  cat("No variable pairs with |correlation| > 0.5 found.\n")
}


## ----correlation-analysis6, echo=TRUE--------------------------------------------------------------------------------------
if ("total_spent" %in% colnames(cor_mat)) {
  spending_cors <- cor_mat["total_spent", ]
  spending_cors <- spending_cors[names(spending_cors) != "total_spent"]
  
  # Create spending_df directly as a tibble for consistency
  spending_df <- tibble( # Changed from data.frame to tibble
    Variable = names(spending_cors),
    Correlation = as.numeric(spending_cors)
  )
  spending_df$abs_cor <- abs(spending_df$Correlation)
  
  # Top 5 positive & negative correlations
  top_pos <- spending_df %>%
    filter(Correlation > 0) %>%
    arrange(desc(Correlation)) %>%
    dplyr::slice(1:5) # Explicitly call dplyr::slice()
  
  top_neg <- spending_df %>%
    filter(Correlation < 0) %>%
    arrange(Correlation) %>%
    dplyr::slice(1:5) # Explicitly call dplyr::slice()
  
  if (nrow(top_pos) > 0) {
    top_pos %>%
      select(-abs_cor) %>%
      gt() %>%
      fmt_number(columns = Correlation, decimals = 2) %>%
      tab_header(
        title = "Top 5 Positive Correlations with total_spent"
      )
  } else {
    # If no positive correlations, display a message within gt
    gt(data.frame(Message="No positive correlations found.")) %>%
      tab_header(title = "Top 5 Positive Correlations with total_spent")
  }

  if (nrow(top_neg) > 0) {
    top_neg %>%
      select(-abs_cor) %>%
      gt() %>%
      fmt_number(columns = Correlation, decimals = 2) %>%
      tab_header(
        title = "Top 5 Negative Correlations with total_spent"
      )
  } else {
    # If no negative correlations, display a message within gt
    gt(data.frame(Message="No negative correlations found.")) %>%
      tab_header(title = "Top 5 Negative Correlations with total_spent")
  }
}


## ----correlation-analysis7, echo=TRUE--------------------------------------------------------------------------------------
# Unique upper triangle correlations (excluding diagonal)
upper_vals <- cor_upper[!is.na(cor_upper)]

# Prepare summary stats for gt table
summary_stats <- data.frame(
  Statistic = c(
    "Number of variable pairs",
    "Mean absolute correlation",
    "Median absolute correlation",
    "Max positive correlation",
    "Max negative correlation"
  ),
  Value = c(
    length(upper_vals),
    round(mean(abs(upper_vals)), 3),
    round(median(abs(upper_vals)), 3),
    round(max(upper_vals), 3),
    round(min(upper_vals), 3)
  ),
  stringsAsFactors = FALSE
)

# Show table with gt
library(gt)
summary_stats %>%
  gt() %>%
  tab_header(title = "Correlation Summary Statistics")

# Distribution plot
hist(
  upper_vals, breaks = 20,
  main = "Distribution of Correlations",
  xlab = "Correlation Coefficient",
  col = "lightblue"
)


## ----interesting-variables, echo=TRUE--------------------------------------------------------------------------------------
# Create composite features based on correlation insights
enhanced_features <- customer_summary %>%
  mutate(
    # Frequency-Value Index (leveraging the 0.72 correlation)
    frequency_value_index = n_orders * log1p(avg_order_value),
    
    # Specialization Score (leveraging the -0.54 correlation)
    specialization_score = n_transactions / n_unique_products,
    
    # Engagement Momentum (combining multiple positive correlations)
    engagement_momentum = (n_orders * n_unique_products) / (recency + 1),
    
    # Value Concentration (how much value in few products)
    value_concentration = total_spent / n_unique_products,
    
    # Loyalty Index (frequency despite time)
    loyalty_index = n_orders / log1p(days_as_customer + 1)
  )

# Calculate correlation matrix for new features
cor_mat_new_feats <- cor(enhanced_features %>% 
    select(total_spent, frequency_value_index, specialization_score, 
           engagement_momentum, value_concentration, loyalty_index),
    use = "complete.obs"
  )

# Convert matrix to data frame with variable name column
cor_df <- as.data.frame(round(cor_mat_new_feats, 2))
cor_df <- tibble::rownames_to_column(cor_df, "Variable")

# Render as gt table
cor_df %>%
  gt() %>%
  tab_header(
    title = "Correlations between total_spent and Composite Features"
  )


## ----baseline-model-kmeans, echo=TRUE--------------------------------------------------------------------------------------
# Helper function
rmse <- function(true, pred) sqrt(mean((true - pred)^2))


## ----baseline-predictive-model, echo=TRUE----------------------------------------------------------------------------------
# Use your customer_summary, or read in your data
# Example: 
customer_summary <- read.csv("customer_rfm_data.csv")

# Features: Use numeric columns, remove IDs and non-feature columns
X <- customer_summary %>% 
  select_if(is.numeric) %>% 
  select(-CustomerID, -total_spent) # <--- Add -total_spent here
y <- customer_summary$total_spent # Or your numeric target

# Split into train/test
set.seed(123)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

# Prepare segmentation features from your customer_summary
segmentation_features <- customer_rfm %>%
  select(
    CustomerID,
    recency,
    n_orders,
    total_spent,
    avg_order_value,
    n_unique_products,
    days_as_customer,
    purchase_frequency_rate
  ) %>%
  na.omit()

# Create scaled version for clustering
features_scaled <- segmentation_features %>%
  select(-CustomerID) %>%
  scale() %>%
  as.data.frame()

# Add CustomerID back
features_scaled$CustomerID <- segmentation_features$CustomerID


## ----baseline-predictive-model-evaluation, echo=TRUE-----------------------------------------------------------------------
# Determine optimal number of clusters using elbow method
set.seed(123)
wss <- map_dbl(1:10, function(k){
  kmeans(select(features_scaled, -CustomerID), centers = k, nstart = 25)$tot.withinss
})

# Plot elbow curve
elbow_data <- data.frame(k = 1:10, wss = wss)
ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters",
       y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

# Apply K-means with optimal k (let's say 4)
kmeans_model <- kmeans(select(features_scaled, -CustomerID), centers = 4, nstart = 25)

# Add cluster assignments
segmentation_features$kmeans_cluster <- kmeans_model$cluster

# Profile clusters
kmeans_profile <- segmentation_features %>%
  group_by(kmeans_cluster) %>%
  summarise(
    n_customers = n(),
    avg_recency = mean(recency),
    avg_frequency = mean(n_orders),
    avg_monetary = mean(total_spent),
    avg_order_value = mean(avg_order_value),
    avg_products = mean(n_unique_products),
    avg_customer_age = mean(days_as_customer)
  ) %>%
  mutate(
    cluster_name = case_when(
      avg_monetary > quantile(segmentation_features$total_spent, 0.75) ~ "High Value",
      avg_frequency > quantile(segmentation_features$n_orders, 0.75) ~ "Loyal",
      avg_recency < quantile(segmentation_features$recency, 0.25) ~ "Active",
      TRUE ~ "Occasional"
    )
  )

# Display cluster profiles
kmeans_profile %>%
  gt() %>%
  tab_header(title = "K-Means Customer Segments") %>%
  fmt_number(columns = c(avg_recency, avg_monetary, avg_order_value, avg_customer_age), 
             decimals = 0) %>%
  fmt_number(columns = avg_frequency, decimals = 1)


## ----baseline-predictive-model-1, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------
library(mclust)

# Fit GMM
gmm_model <- Mclust(select(features_scaled, -CustomerID))

# Add cluster assignments
segmentation_features$gmm_cluster <- gmm_model$classification

# GMM provides probability of belonging to each cluster
cluster_probabilities <- gmm_model$z
segmentation_features$cluster_certainty <- apply(cluster_probabilities, 1, max)

# Profile GMM clusters
gmm_profile <- segmentation_features %>%
  group_by(gmm_cluster) %>%
  summarise(
    n_customers = n(),
    avg_certainty = mean(cluster_certainty),
    avg_monetary = mean(total_spent),
    avg_frequency = mean(n_orders)
  )

gmm_profile %>%
  gt() %>%
  tab_header(title = "Gaussian Mixture Model Segments") %>%
  fmt_percent(columns = avg_certainty, decimals = 1)

