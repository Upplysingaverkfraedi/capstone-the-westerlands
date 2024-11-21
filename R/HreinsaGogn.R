library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# Load the original data with appropriate encoding and delimiter
kaupskra <- read_csv2("heilbrigdisumdaemi/data/heilbrigdisumdaemi/kaupskra.csv", 
                      locale = locale(encoding = "ISO-8859-1"))

# Debug: Print a sample of the original data
print(head(kaupskra))

# Process data to calculate accurate average price per square meter
processed_data <- kaupskra %>%
  select(sveitarfel = SVEITARFELAG, date = UTGDAG, property_type = TEGUND, price = KAUPVERD, area = EINFLM) %>%
  mutate(
    year = year(ymd(date)),               # Extract year from date
    price_per_sqm = price / area          # Calculate price per square meter
  ) %>%
  drop_na(year, price, area) %>%          # Remove rows with missing year, price, or area
  filter(area > 0,                        # Ensure area is non-zero
         price_per_sqm >= 200,            # Filter out low outliers
         price_per_sqm <= 2000) %>%       # Filter out high outliers
  group_by(sveitarfel, year, property_type) %>%
  summarize(
    total_price = sum(price, na.rm = TRUE),      # Sum total price for each group
    total_area = sum(area, na.rm = TRUE),        # Sum total area for each group
    average_price_per_sqm = total_price / total_area,  # Calculate average price per square meter
    .groups = "drop"
  )

# Save the processed data to the specified path
write_csv(processed_data, "data/heilbrigdisumdaemi/processed_kaupskra_per_sqm.csv")

