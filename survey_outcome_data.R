# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Define the file path (replace with actual path if necessary)
question3_path <- "output_csvs/question_3.csv"
question4_path <- "output_csvs/question_4.csv"
question5_path <- "output_csvs/question_5.csv"

survey_category_path <- "data/survey_category.csv"

# Read the survey data from the output CSV files
# nolint start
survey_question3 <- read.csv(question3_path, sep = ",", header = TRUE, check.names = FALSE)
survey_question4 <- read.csv(question4_path, sep = ",", header = TRUE, check.names = FALSE)
survey_question5 <- read.csv(question5_path, sep = ",", header = TRUE, check.names = FALSE)
survey_category <- read.csv(survey_category_path, sep = ",", header = TRUE, check.names = FALSE)
# nolint end

# Pivot (starting from the 11th column)
pivot_longer_question3 <- survey_question3 %>%
    pivot_longer(
        cols = -c(1:10),
        names_to = "survey_option",
        values_to = "response"
    )

pivot_longer_question4 <- survey_question4 %>%
    pivot_longer(
        cols = -c(1:10),
        names_to = "survey_option",
        values_to = "response"
    )

pivot_longer_question5 <- survey_question5 %>%
    pivot_longer(
        cols = -c(1:10),
        names_to = "survey_option",
        values_to = "response"
    )

# Merge the survey data with the survey category data
merged_question3 <- merge(pivot_longer_question3, survey_category, by = "survey_option")
merged_question5 <- merge(pivot_longer_question5, survey_category, by = "survey_option")

# Merge merged data
merged_data <- rbind(merged_question3, merged_question5)

# Save the merged data to a CSV file
write.csv(merged_question3, "output_csvs/merged_question3.csv", row.names = FALSE)
write.csv(merged_question5, "output_csvs/merged_question5.csv", row.names = FALSE)
write.csv(merged_data, "output_csvs/merged_outcome_data.csv", row.names = FALSE)

