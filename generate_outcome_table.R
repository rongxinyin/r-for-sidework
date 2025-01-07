# Load required libraries
library(dplyr)
library(tidyr)
library(kableExtra)

# Load the data
data <- read.csv("output_csvs/merged_outcome_data.csv")

# Filter necessary columns and remove rows with NA values
filtered_data <- data %>%
  select(category, survey_option, response) %>%
  drop_na()

# Calculate percentages grouped by category and survey_option
percentage_table <- filtered_data %>%
  group_by(category, survey_option, response) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(category, survey_option) %>%
  mutate(percentage = 100 * count / sum(count)) %>%
  ungroup()

# Reorder the response levels
response_order <- c("Strongly Disagree", "Disagree", "Not Sure", "Agree", "Strongly Agree")
percentage_table$response <- factor(percentage_table$response, levels = response_order)

# Pivot the data to create the desired table format
pivot_table <- percentage_table %>%
  select(-count) %>%
  pivot_wider(names_from = response, values_from = percentage, values_fill = list(percentage = 0))

# Reorder columns as per the specified list
reordered_columns <- c("Strongly Disagree", "Disagree", "Not Sure", "Agree", "Strongly Agree")
pivot_table <- pivot_table %>%
  select(category, survey_option, all_of(reordered_columns))

# Format the percentages with a '%' sign
pivot_table <- pivot_table %>%
  mutate(across(where(is.numeric), ~ sprintf("%.1f%%", .)))

# Generate the HTML table with merged and styled "category" column
html_table <- pivot_table %>%
  kable(format = "html", row.names = FALSE, col.names = c("", "Survey Option", reordered_columns)) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  pack_rows(index = table(pivot_table$category), bold = TRUE, italic = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>% # Make first column bold and separate with border
  column_spec(3:7, bold = FALSE)                     # Keep percentage columns regular

# Save the HTML table
save_kable(html_table, file = "output_htmls/survey_outcome_percentage_table.html")

# Print message
cat("HTML table generated and saved as 'survey_outcome_percentage_table.html'")