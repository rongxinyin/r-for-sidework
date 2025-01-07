# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Define the file path (replace with actual path if necessary)
file_path <- "data/4. Actual Demographics - Youth.xlsx"  # Update path if needed

# Read the first sheet of the Excel file with specific rows and headers
raw_data <- read_excel(file_path, sheet = 1, skip = 7, col_names = TRUE)

# Extract the demographic data
demo_data <- raw_data[-c(1, nrow(raw_data)-1, nrow(raw_data)), 1:6]
age_data <- raw_data[-c(1, nrow(raw_data)-1, nrow(raw_data)), 7:20]
gender_data <- raw_data[-c(1, nrow(raw_data)-1, nrow(raw_data)), 21:24]
ethnicity_data <- raw_data[-c(1, nrow(raw_data)-1, nrow(raw_data)), 25:36]
zipcode_data <- raw_data[-c(1, nrow(raw_data)-1, nrow(raw_data)), 37:ncol(raw_data)]

# Combine the demographic data with age and gender data
new_age_data <- cbind(demo_data, age_data)
new_gender_data <- cbind(demo_data, gender_data)
new_ethnicity_data <- cbind(demo_data, ethnicity_data)
new_zipcode_data <- cbind(demo_data, zipcode_data)

# Rename columns contains "Other" to "Others"
colnames(new_age_data)[colnames(new_age_data) == "Other"] <- "Others"
colnames(new_gender_data)[grepl("Other", colnames(new_gender_data))] <- "Others"
colnames(new_ethnicity_data)[grepl("Other", colnames(new_ethnicity_data))] <- "Others"
colnames(new_zipcode_data)[grepl("Other", colnames(new_zipcode_data))] <- "Others"

# Reshape the age, gender, ethnicity, and zipcode data into a longer format
pivot_longer_age <- new_age_data %>%
  pivot_longer(
    cols = -c(1:6),
    names_to = "Age_Group",
    values_to = "Count"
  ) %>%
  mutate(
    New_Age_Group = case_when(
      as.numeric(Age_Group) >= 5 & as.numeric(Age_Group) <= 6 ~ "5-6 years old",
      as.numeric(Age_Group) >= 7 & as.numeric(Age_Group) <= 10 ~ "7-10 years old",
      as.numeric(Age_Group) >= 11 & as.numeric(Age_Group) <= 13 ~ "11-13 years old",
      as.numeric(Age_Group) >= 14 & as.numeric(Age_Group) <= 17 ~ "14-17 years old",
      grepl("18+", Age_Group) ~ "18+ years old",
      TRUE ~ "Others"
    )
  )

pivot_longer_gender <- new_gender_data %>%
  pivot_longer(
    cols = -c(1:6),
    names_to = "Gender_Group",
    values_to = "Count"
  )

pivot_longer_ethnicity <- new_ethnicity_data %>%
  pivot_longer(
    cols = -c(1:6),
    names_to = "Ethnicity_Group",
    values_to = "Count"
  )

pivot_longer_zipcode <- new_zipcode_data %>%
  pivot_longer(
    cols = -c(1:6),
    names_to = "Zipcode_Group",
    values_to = "Count"
  )

# Save the reshaped data to CSV files
write.csv(demo_data, "output_csvs/demographic_data.csv", row.names = FALSE)
write.csv(pivot_longer_age, "output_csvs/age_data_long.csv", row.names = FALSE)
write.csv(pivot_longer_gender, "output_csvs/gender_data_long.csv", row.names = FALSE)
write.csv(pivot_longer_ethnicity, "output_csvs/ethnicity_data_long.csv", row.names = FALSE)
write.csv(pivot_longer_zipcode, "output_csvs/zipcode_data_long.csv", row.names = FALSE)

