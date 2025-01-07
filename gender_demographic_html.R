# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Read the demographic data
demo_data <- read.csv("output_csvs/demographic_data.csv", header = TRUE)
gender_data <- read.csv("output_csvs/gender_data_long.csv", header = TRUE)

# List of age groups for reordering
gender_order <- c("Female", "Male", "Non-Binary", "Others")

# Aggregate counts by program
aggregated_demo_data <- demo_data %>%
  group_by(Funding.Strategy) %>%
  summarise(Total = sum(Total)) %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1))

# Aggregate counts by Gender_Group
aggregated_gender_data <- gender_data %>%
  group_by(Gender_Group) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Create the interactive bar plot for age group distribution
plot_gender <- plot_ly(
  aggregated_gender_data,
  x = ~Count,
  y = ~ factor(Gender_Group, levels = rev(gender_order)),
  text = ~ paste(Percentage, "%"),
  type = "bar",
  orientation = "h",
  marker = list(color = "lightcoral"),
  name = "Gender Group"
) %>%
  layout(
    xaxis = list(title = "Count"),
    yaxis = list(title = "Gender Group"),
    margin = list(l = 100)
  )

# Create the interactive bar plot for gender group distribution
plot_demo <- plot_ly(
  aggregated_demo_data,
  x = ~Total,
  y = ~ reorder(Funding.Strategy, Total),
  text = ~ paste(Percentage, "%"),
  type = "bar",
  orientation = "h",
  marker = list(color = "skyblue"),
  name = "Funding Strategy"
) %>%
  layout(
    xaxis = list(title = "Count"),
    yaxis = list(title = "Funding Strategy"),
    margin = list(l = 100)
  )

# Combine the two interactive plots side by side with increased horizontal space
combined_plot <- subplot(
  plot_demo, plot_gender,
  nrows = 1,
  shareX = FALSE,
  shareY = FALSE,
  titleX = FALSE,
  titleY = FALSE,
  widths = c(0.45, 0.45), # Set widths to adjust horizontal space
  margin = 0.1 # Increase horizontal space between plots
) %>%
  layout(
    annotations = list(
      list(
        x = 0.0, # Adjust position for the left title
        y = 1.05,
        text = "<b>Funding Strategy</b>",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left"
      ),
      list(
        x = 0.6, # Adjust position for the right title
        y = 1.05,
        text = "<b>Gender</b>",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "right"
      )
    ),
    showlegend = FALSE # Hide the legend
  )

# Save the combined interactive plot as an HTML file
html_file <- "output_htmls/Gender_Distribution_Interactive.html"
htmlwidgets::saveWidget(combined_plot, file = html_file, selfcontained = TRUE)

cat("Interactive HTML file saved as:", html_file)
