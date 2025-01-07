# Load necessary libraries
library(DT)
library(dplyr)
library(stringr)
library(htmlwidgets)

# Define the path to the survey data
data_path <- "output_csvs/demographic_data.csv"
# Load the survey data
survey_data <- read.csv(data_path, sep = ",", header = TRUE, check.names = FALSE)

# Calculate the number of programs served and the number of youth served
programs_served <- nrow(survey_data)
youth_served <- sum(survey_data$Total)

# Create a data frame with the calculated values for display
summary_df <- data.frame(
  Metric = c("Number of Programs Served:", "Number of Youth Served:"),
  Value = c(programs_served, youth_served)
)
# Save the interactive table to an HTML file
html_file <- "output_htmls/program_served_table.html"

# Render the styled table using DT
datatable(summary_df, rownames = FALSE, options = list(
  pageLength = 8.5,
  autoWidth = TRUE,
  dom = "t", # Remove search and pagination
  columnDefs = list(
    list(className = "dt-right", width = "50%", targets = 0), # Align "Metric" column to the left
    list(className = "dt-left", width = "50%", targets = 1), # Center-align "Value" column
    list(orderable = FALSE, targets = "_all"), # Disable sorting for all columns
    list(targets = 0:1, title = "") # Remove column title for the first column
  )
)) %>%
  formatStyle(
    "Value",
    color = "black", # Show only text with no background
    backgroundColor = NULL,
    fontWeight = "bold"
  ) %>%
  saveWidget(file = html_file, selfcontained = TRUE)

cat("Styled interactive table saved to:", html_file)
