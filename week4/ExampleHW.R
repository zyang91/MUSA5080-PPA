# Load datasets from OpenDataPhilly - download to project folder
library(sf)
#I downloaded demolitions and census block groups geojson files into my project folder
demolitions_sf <- read_sf("demolitions.geojson")
censusbg_sf <- read_sf("Census_Block_Groups_2010.geojson")
food_sf <- read_sf("NeighborhoodFoodRetail.geojson")

# Check coordinate reference systems (CRS)
st_crs(demolitions_sf)
st_crs(censusbg_sf)
st_crs(food_sf)

# Reproject data to PCS - PA state plane
demolitions <- st_transform(demolitions_sf, 2272)
block_groups <- st_transform(censusbg_sf, 2272)
food_sf <- st_transform(food_sf , 2272)

st_crs(demolitions_sf)
st_crs(censusbg_sf)

# Quick view of data
head(demolitions_sf)
head(censusbg_sf )
head(food_sf)

# Plot block groups and demolitions to check alignment
ggplot() +
  geom_sf(data = block_groups, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = demolitions, color = "red", alpha = 0.6) +
  labs(title = "Philadelphia Block Groups & Building Demolitions")

#maybe our indicator is simply the number of housing demolitions in a census tract
# Spatially join demolitions to block groups
demolitions_with_blocks <- st_join(demolitions, block_groups, left = FALSE)

# Count demolitions per block group
demolition_counts <- demolitions_with_blocks %>%
  group_by(GEOID10) %>%  # GEOID is the unique identifier for each block group
  summarise(demolition_count = n())

# Ensure demolition_counts is a regular tibble
demolition_counts <- demolition_counts %>% st_drop_geometry()

# Perform left_join safely
block_groups <- left_join(block_groups, demolition_counts, by = "GEOID10")

# Fill NA demolition counts with 0
block_groups$demolition_count[is.na(block_groups$demolition_count)] <- 0

# Join food access data to block groups
food_sf <- food_sf %>% st_drop_geometry()
block_groups <- left_join(block_groups, food_sf %>% select(GEOID10, HPSS_ACCESS), 
                          by = c("GEOID10" = "GEOID10"))
# Determine high demolition threshold (75th percentile)
high_demolition_threshold <- quantile(block_groups$demolition_count, 0.75, na.rm = TRUE)

# Create a categorical risk indicator
block_groups <- block_groups %>%
  mutate(risk_category = case_when(
    demolition_count >= high_demolition_threshold & HPSS_ACCESS == "Low Access" ~ "At Risk",
    demolition_count >= high_demolition_threshold | HPSS_ACCESS == "Low Access" ~ "Moderate Risk",
    TRUE ~ "Low Risk"
  ))

# View the categorized data
table(block_groups$risk_category)

# Plot the categorized risk map
ggplot(block_groups) +
  geom_sf(aes(fill = risk_category), color = "white") +
  scale_fill_manual(values = c("Low Risk" = "green", "Moderate Risk" = "yellow", "At Risk" = "red")) +
  labs(title = "Neighborhood Risk Categorization (Demolitions & Food Access)",
       fill = "Risk Level") +
  theme_void()

