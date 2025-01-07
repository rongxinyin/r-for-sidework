# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(DT)

# Load the survey data
survey_data <- read.csv("output_csvs/question_3.csv", sep = ",", header = TRUE, check.names = FALSE) # nolint

# Focus on the relevant columns (starting from the 11th column)
response_columns <- colnames(survey_data)[11:ncol(survey_data)]

# Shorten the column names for labels if they are too long
abbreviated_columns <- sapply(response_columns, function(x) {
  if (nchar(x) > 40) {
    str_wrap(x, width = 40) # Wrap labels to 40 characters
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
  Percentage = round(response_distribution, 0)
)

# Add a strategic goal number for illustration purposes
distribution_df <- distribution_df %>%
  mutate(Strategic_Goal = paste("Goal", 1:n())) # Use unique goals for each row

# Display the data frame in an interactive table with bar plots for percentages
datatable(distribution_df, options = list(pageLength = 10, autoWidth = TRUE)) %>%
  formatStyle(
    "Percentage",
    background = styleColorBar(c(0, 100), "lightblue"), # Create a bar plot in the Percentage column
    backgroundSize = "100% 90%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  ) %>%
  formatStyle(
    "Strategic_Goal",
    color = "white",
    backgroundColor = "skyblue",
    fontWeight = "bold"
  )


# # Create the bar plot with wrapped labels, reduced spacing, and increased plot width
# ggplot(distribution_df, aes(x = Percentage, y = reorder(Question, Percentage))) +
#   geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +  # Reduced bar width
#   geom_text(aes(label = paste0(Percentage, "%")), hjust = -0.2, color = "black", size = 4) +
#   geom_text(aes(x = 110, label = "90%"), hjust = -0.2, color = "black", size = 4) +
#   labs(
#     title = "Survey Responses with Strategic Goals and Bar Values"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     axis.title.x = element_blank(),  # Hide x-axis label
#     axis.title.y = element_blank(),  # Hide y-axis label
#     axis.text.y = element_text(size = 8), # Adjust y-axis text size for readability
#     axis.ticks = element_blank(),    # Remove axis ticks
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1) # Compact x-axis labels if needed
#   ) +
#   scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +  # Reduce vertical gaps between bars
#   xlim(0, 105)  # Ensure space for annotations