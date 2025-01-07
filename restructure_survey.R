library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

# Load the raw CSV
input_file <- 'data/test.csv'  # Replace with the correct path to your CSV file
output_folder <- 'output_csvs/'  # Define output folder

# Ensure the output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Read the CSV data
raw_data <- read_csv(input_file, col_names = FALSE)

# Extract the first two rows as hierarchical headers (columns 10 onward)
top_level_headers <- raw_data[1, 10:ncol(raw_data)]
second_level_headers <- raw_data[2, 10:ncol(raw_data)]

# Fill blank top-level headers with the previous non-blank value
for (i in seq_along(top_level_headers)) {
    if (is.na(top_level_headers[i])) {
        top_level_headers[i] <- top_level_headers[i - 1]
    }
}

# Combine the two levels of headers
combined_headers <- paste(top_level_headers, second_level_headers, sep = " | ")
colnames(raw_data) <- c(raw_data[1, 1:9], second_level_headers)

# Remove the first two rows (headers) to leave only the data rows
data_rows <- raw_data[-(1:2), ]

# Ensure proper column naming for participant IDs (columns 1-9)
# colnames(data_rows)[1:9] <- c(paste0("Col", 1:9))

# Split data by top-level headers
unique_top_headers <- unique(as.vector(unlist(top_level_headers)))
print(unique_top_headers)

# Create a data frame to store the top-level headers as a column and corresponding question numbers as another column
top_headers_df <- data.frame(Survey.Question = unique_top_headers, "Question.Number" = paste0("question_", 1:length(unique_top_headers)))

print(top_headers_df)

# top_headers_df <- data.frame(Survey.Question = unique_top_headers, "Question.Number" = paste0("question_", 1:length(unique_top_headers)))

# Save the top-level headers to a CSV file
write_csv(top_headers_df, paste0(output_folder, "top_level_headers.csv"))

for (top_header in unique_top_headers) {
  # Select columns corresponding to the current top-level header
  relevant_columns <- which(top_level_headers == top_header)
  subset_data <- data_rows[, c(1:9, relevant_columns + 9)] # Include first 9 columns and relevant answers

  # Insert the top-level header as the first column
  subset_data <- cbind("Survey Question" = top_header, subset_data)
  
  # Save to a CSV file
question_number <- top_headers_df$Question.Number[which(top_headers_df$Survey.Question == top_header)]
output_file <- paste0(output_folder, question_number, ".csv")
write_csv(subset_data, output_file)

}