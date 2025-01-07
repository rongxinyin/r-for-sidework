# Load necessary libraries
library(DT)
library(dplyr)
library(stringr)
library(htmlwidgets)

# Define the path to the survey data
data_path <- "output_csvs/question_3.csv"
# Load the survey data
survey_data <- read.csv(data_path, sep=",", header=TRUE, check.names=FALSE)

# Focus on the relevant columns (starting from the 11th column)
response_columns <- colnames(survey_data)[11:ncol(survey_data)]

# Shorten the column names for labels if they are too long
abbreviated_columns <- sapply(response_columns, function(x) {
  if (nchar(x) > 40) {
    stringr::str_wrap(x, width = 40) # Wrap labels to 40 characters
  } else {
    x
  }
})

# Calculate the distribution of "Agree" and "Strongly Agree"
response_distribution <- sapply(response_columns, function(col) {
  agree_counts <- sum(survey_data[[col]] %in% c("Agree", "Strongly Agree"), na.rm = TRUE)
  total_counts <- sum(!is.na(survey_data[[col]]))
  (agree_counts / total_counts) * 100
})

# Convert the distribution into a data frame
distribution_df <- data.frame(
  Question = abbreviated_columns, # Use the shortened labels
  Percentage = response_distribution / 100
)

# Create two sets of data (e.g., current and previous values)
distribution_df <- distribution_df %>%
  mutate("Strategic Goal" = rep(0.9, nrow(distribution_df)))  # Use unique goals for each row

# Save the interactive table to an HTML file
html_file <- "output_htmls/question3_interactive_table.html"

# Render the styled table using DT
datatable(distribution_df, rownames = FALSE, options = list(
  pageLength = 8.5,
  autoWidth = TRUE,
  dom = 't',  # Remove search and pagination
  columnDefs = list(
    list(className = 'dt-right', width = '50%', targets = 0),  # Align "Question" column to the left
    list(className = 'dt-left', width = '30%', targets = 1),  # Align "Question" column to the left
    list(className = 'dt-center', width = '20%', targets = 2),  # Center-align other columns
    list(orderable = FALSE, targets = "_all"),       # Disable sorting for all columns
    list(targets = 0:1, title = "") # Remove column titles for the first two columns
  )
)) %>%
  formatPercentage("Percentage", 0) %>%
  formatPercentage("Strategic Goal", 0) %>%
  formatStyle(
    'Percentage',
    background = styleColorBar(c(0, 1), 'skyblue', -90),  # Horizontal bar for percentage
    backgroundSize = '85% 60%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'left'
  ) %>%
  formatStyle(
    'Percentage',
    color = 'black',
    fontWeight = 'bold',
    textAlign = 'right'
  ) %>%
  formatStyle(
    'Strategic Goal',
    color = 'black',  # Show only text with no background
    backgroundColor = NULL,
    fontWeight = 'bold'
  ) %>%
  saveWidget(file = html_file, selfcontained = TRUE)

cat("Styled interactive table saved to:", html_file)
