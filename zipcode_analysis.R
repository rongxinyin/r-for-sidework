# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Load the CSV data
file_path <- "output_csvs/zipcode_data_long.csv"
zipcode_data <- read.csv(file_path)

# Rename the column "Zipcode_Group" to "ZIP_Code" for consistency
zipcode_data <- zipcode_data %>%
  rename(ZIP = Zipcode_Group)

# Group by ZIP code and calculate counts
zipcode_grouped <- zipcode_data %>%
  group_by(ZIP) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 0)) %>%
  arrange(desc(Percentage))

# Load spatial data for ZIP codes (replace with GeoJSON if available)
geojson_path <- "data/ark28722-s7888q-geojson.json"  # Update with the actual path
zipcode_map <- st_read(geojson_path)

# Join the count data with spatial data using inner join
zipcode_map <- zipcode_map %>%
  inner_join(zipcode_grouped, by = c("ZIP" = "ZIP"))

# Create the map plot
map_plot <- leaflet(data = zipcode_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~colorNumeric("Greys", Count)(Count),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = ~paste(ZIP, ":", Count)
  ) %>%
  addLegend(
    pal = colorNumeric("Greys", zipcode_map$Count),
    values = zipcode_map$Count,
    title = "Number of Youth",
    position = "bottomright"
  )

# Save the map as an HTML file
map_html_file <- "output_htmls/zipcode_map.html"
saveWidget(map_plot, map_html_file, selfcontained = TRUE)

# Create the bar plot
bar_plot <- ggplot(zipcode_grouped, aes(x = Count, y = reorder(ZIP, Count))) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), hjust = -0.2, size = 4) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# Save the bar plot as a PNG image
bar_plot_image <- "zipcode_bar_plot.png"
ggsave(bar_plot_image, bar_plot, width = 10, height = 6, dpi = 300)

# Combine Leaflet map and bar plot into a single HTML
combined_html <- tagList(
  tags$div(
    style = "display:flex; flex-direction:row; justify-content:space-between;",
    tags$div(style = "width:50%;", as.tags(map_plot)),
    tags$div(style = "width:50%;",
             tags$img(src = bar_plot_image, style = "width:100%; height:auto;"))
  )
)

# Save the combined HTML
combined_html_file <- "output_htmls/zipcode_map_and_bar.html"
save_html(combined_html, file = combined_html_file)

cat("Combined HTML file saved as:", combined_html_file)