# Load required libraries
library(dplyr)
library(tidyr)
library(DT)
library(htmlwidgets)

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
  mutate(across(where(is.numeric), ~ sprintf("%.0f%%", .)))

# Create the datatable with merged cells in the first column
datatable_html <- datatable(
  pivot_table,
  rownames = FALSE,
  colnames = c("", "", reordered_columns),
  options = list(
    dom = 't', # Removes pagination, search, and info
    columnDefs = list(
      list(className = 'dt-center', targets = 2:6), # Center-align percentages
      list(width = "5%", targets = 2:6),           # Set equal width for the last five columns
      list(orderable = FALSE, targets = "_all")       # Disable sorting for all columns
    ),
    rowCallback = JS(
      "function(row, data, index) {",
      "  var api = this.api();",
      "  if (index > 0 && data[0] === api.cell(index - 1, 0).data()) {",
      "    $('td:eq(0)', row).remove();", # Remove duplicate category cells
      "  } else {",
      "    var rowspan = 1;",
      "    for (var i = index + 1; i < api.rows().data().length; i++) {",
      "      if (data[0] === api.cell(i, 0).data()) {",
      "        rowspan++;",
      "      } else {",
      "        break;",
      "      }",
      "    }",
      "    $('td:eq(0)', row).attr('rowspan', rowspan);", # Merge cells
      "  }",
      "}"
    )
  ),
  escape = FALSE # Allow HTML formatting
)

# Save the output to an HTML file
saveWidget(datatable_html, "output_htmls/survey_outcome_percentage_table_DT.html", selfcontained = TRUE)

# Print message
cat("HTML table generated and saved as 'survey_outcome_percentage_table_DT.html'")