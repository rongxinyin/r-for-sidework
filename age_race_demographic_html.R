# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Read the demographic data
age_data <- read.csv("output_csvs/age_data_long.csv", header = TRUE)
ethnicity_data <- read.csv("output_csvs/ethnicity_data_long.csv", header = TRUE)

# List of age groups for reordering
age_order = c( "5-6 years old", "7-10 years old", "11-13 years old", "14-17 years old", "18+ years old")

# Aggregate counts by New_Age_Group
aggregated_age_data <- age_data %>%
  group_by(New_Age_Group) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 0)) %>%
  arrange(factor(New_Age_Group, levels = age_order))

# Aggregate counts by Gender_Group
aggregated_ethnicity_data <- ethnicity_data %>%
  group_by(Ethnicity_Group) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the interactive bar plot for age group distribution
plot_age <- plot_ly(
    aggregated_age_data,
    x = ~Count,
    y = ~factor(New_Age_Group, levels = rev(age_order)),
    text = ~paste(Percentage, "%"),
    type = "bar",
    orientation = "h",
    marker = list(color = "lightcoral"),
    name = "Age Group"
) %>%
layout(
        xaxis = list(title = "Count"),
        yaxis = list(title = "Age Group"),
        margin = list(l = 100)
)

# Create the interactive bar plot for gender group distribution
plot_ethnicity <- plot_ly(
    aggregated_ethnicity_data,
    x = ~Count,
    y = ~reorder(Ethnicity_Group, Count),
    text = ~paste(Percentage, "%"),
    type = "bar",
    orientation = "h",
    marker = list(color = "skyblue"),
    name = "Ethnicity Group"
) %>%
layout(
        xaxis = list(title = "Count"),
        yaxis = list(title = "Race/Ethnicity Group"),
        margin = list(l = 100)
)

# Combine the two interactive plots side by side with increased horizontal space
combined_plot <- subplot(
        plot_ethnicity, plot_age, 
        nrows = 1, 
        shareX = FALSE, 
        shareY = FALSE, 
        titleX = FALSE, 
        titleY = FALSE,
        widths = c(0.45, 0.45),  # Set widths to adjust horizontal space
        margin = 0.1            # Increase horizontal space between plots
) %>%
        layout(
    annotations = list(
      list(
        x = 0.0,  # Adjust position for the left title
        y = 1.05,
        text = "<b>Race/Ethnicity</b>",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left"
      ),
      list(
        x = 0.6,  # Adjust position for the right title
        y = 1.05,
        text = "<b>Age (as of first day of grant)</b>",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "right"
      )
    ),
    showlegend = FALSE  # Hide the legend
  )

# Save the combined interactive plot as an HTML file
html_file <- "Age_Ethnicity_Distribution_Interactive.html"
htmlwidgets::saveWidget(combined_plot, file = html_file, selfcontained = TRUE)

cat("Interactive HTML file saved as:", html_file)