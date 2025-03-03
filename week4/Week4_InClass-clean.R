#Week 4
#February 7, 2025
#Reference: https://r.geocompx.org/

#This is the first part of today's adventure.
#Today's goal is to go over core GIS functions in R.
#If necessary, install the following packages
install.packages("sf")
install.packages("terra")
### Built in data
install.packages("spData")
install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")

#Load the libraries
library(sf)
library(terra)
library(dplyr)
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

#Let's explore some sf/geographic data.
#The world data layer comes from the spdatalarge package. Let's load it first
world <- world
class(world)

names(world)
head(world,1)


summary(world["lifeExp"])

#read in an sf object using 'st_read'
world_dfr = st_read(system.file("shapes/world.gpkg", package = "spData"))


# now try with 'read_sf'
world_tbl = read_sf(system.file("shapes/world.gpkg", package = "spData"))

head(world_tbl,1)
head(world_dfr,1)

# Now for some raster data using Terra Package
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

#type in the name of the raster in the console

##spatial data operations
# spatial subsetting - intersection

nz <- nz
canterbury = nz %>% filter(Name == "Canterbury")
nz_height <- nz_height
#find points that intersect Canterbury (Intersect)
canterbury_height = nz_height[canterbury, ]

summary(canterbury_height)
plot(canterbury_height[2])
plot(nz_height[2])

#points that don't intersect (Disjoint)
nz_height[canterbury, , op = st_disjoint]

#another approach - returns binary yes or no if true for each obs.
st_intersects(x = nz_height, y = canterbury, sparse = FALSE)

#more operators
st_within(nz_height, canterbury, sparse = FALSE)
st_touches(nz_height, canterbury, sparse = FALSE) #points on the edge (not within)

st_disjoint(nz_height, canterbury, sparse = FALSE)[, 1]

st_is_within_distance(nz_height, canterbury, dist = 0.2, sparse = FALSE)[, 1]


#Distance relationships
#find distance between highest point and centroid of canterbury
#returns a matrix
nz_highest = nz_height %>% slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

#so let's create an entire distance matrix between canterbury and otago and 3 points. Use shortand way of filtering.
#why the 0s?
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

co<-co%>% filter(Name=="Otago")
co_height = nz_height[co, ]
st_distance(co, co_height[1:2],)
st_distance(nz, co_height[1,2],)

#plot them
plot(st_geometry(nz)[1])
plot(st_geometry(co_height)[1:2], add = TRUE)

#Spatial joining - an illustrative demo
set.seed(2018) # set seed for reproducibility
(bb = st_bbox(world)) # the world's bounds

random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") # set coordinates and CRS

#find points that intersect the world
world_random = world[random_points, ]
nrow(world_random)

random_joined = st_join(random_points, world["name_long"])

#set left = FALSE to override the default left join
# the default spatial operation is st_intersects

#Now we can do a distance-based join.
#Here we can see that these two cycle hire datasets are close, but not exactly on top of each other.
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

#Check if points are the same using st_intersects
any(st_intersects(cycle_hire, cycle_hire_osm, sparse = FALSE))

# We want to join the capacity variable in cycle_hire_osm onto the 'official' target data in cycle_hire.

# How many are within 20 m of each other?
sel = st_is_within_distance(cycle_hire, cycle_hire_osm,
                            dist = units::set_units(20, "m"))
summary(lengths(sel) > 0)

# now we will assign them the values using st_join:
z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance,
            dist = units::set_units(20, "m"))
nrow(cycle_hire)

nrow(z)

#too many! What happend?
# some cycle_hire stations have multiple matches in the osm layer.
# one solution is to aggregate values for overlapping points and return the mean.
z = z %>%
  group_by(id) %>%
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

#Spatial Aggregation
#find the average height of points in each region.
#the aggregating object is nz
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

#that function should do the same thing as we've done using group by after assigning the name of the region to the points.
nz_agg2 = st_join(x = nz, y = nz_height)  %>%
  group_by(Name)  %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))


#incongruent overlay using areal weighted interpoltaion,
# when the boundary line across different geographies not matches.
# Interpolation based on areas of overlap
iv = incongruent["value"] # keep only the values to be transferred
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)

agg_aw$value

# set extensive = FALSE when dealing with percents

## OK your turn to try some challenges
# Canterbury was the region of New Zealand containing most of the 101 highest points in the country.
#How many of these high points does the Canterbury region contain?
#Hint: find canterbury and then find how many points (from nz_height) intersect canterbury

st_intersects(nz_height, canterbury, sparse = FALSE) %>% sum()
# Answer: 70


#Which region has the second highest number of nz_height points, and how many does it have?
#go for the spatial join and group-by approach!
highest2<- st_join(nz, nz_height) %>%
  group_by(Name) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(2)

# Answer: West Coast, with 22 points

#calculate the distance between the centroid of colorado and the centroid of PA?
#How does this differ if we don't calculate centroid and just use distance on the polygons?
#there is a data layer called us_states you can use from the loaded sp_data package
us_states = us_states
pa<- us_states %>% filter(NAME=="Pennsylvania")
CO<- us_states %>% filter(NAME=="Colorado")
st_distance(st_centroid(pa), st_centroid(CO))

### Moving onward!
##More geometric operations
# ye old buffer

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)


#clipping - a form of subsetting
#illustrative example
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b, border = "gray")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3) # add text
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "gray")
plot(x_and_y, col = "lightgray", border = "gray", add = TRUE) # intersecting area


##dissolve geometries happens when you group by using sf data
regions2 = us_states %>%
  group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))
plot(regions2, border = "gray")

# we can also UNION places together. Add texas to the western union
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)
## Let's talk about Projections and CRSs
plot(texas_union , border = "gray")


###Now let's talk about projections and coordinate systems
# create a simple data frame for london in lat/long
london = data.frame(lon = -0.1, lat = 51.5) %>%
  st_as_sf(coords = c("lon", "lat"))
st_is_longlat(london)

#why did we get NA??
london_geo = st_set_crs(london, "EPSG:4326")
st_is_longlat(london_geo)

#let's buffer london in lat/long
london_buff_lonlat = st_buffer(london_geo, dist = 1)
sf::sf_use_s2(TRUE)
plot(london_buff_lonlat)

#create a different one
london_proj = data.frame(x = 530000, y = 180000)  %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:27700")

#check it out. Pay attention to Length Unit
st_crs(london_proj)

london_buff_projected = st_buffer(london_proj, 100000)

plot(london_buff_projected)

#when to reproject
st_distance(london_geo, london_proj)

london2 = st_transform(london_geo, "EPSG:27700")
st_crs(london2)

st_distance(london2, london_proj)

#check another layer
st_crs(cycle_hire_osm)
#re-project it
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
st_crs(cycle_hire_osm_projected)


### Mapping Time! (Tiger files are back up so let's pivot to using census data)


options(tigris_use_cache = TRUE)
# setting geometry = TRUE is the key here
library(tidycensus)
library(tidyverse)
library(tigris)

# get US data. Geometry = TRUE. move AK and HI to bottom for mapping
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()

plot(us_median_age$geometry)

# a very basic ggplot map. following the same forumula we used for a plot, but we will use 'geom_sf' to make it a map
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf()

#move beyond the default
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdPu",
                       direction = 1) +
  labs(title = "  Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()

## What happens if you don't have direction = 1?

## Ok - let's run through a few more workflows that integrates what we've learned so far.
# Repetition = knowledge gained.
#From Tidycensus book

#first, get census tracts for both states in Kanasas City (go Birds! Boo Chiefs!)
#re-project from NAD83 to KC regional coordinate system
# CRS used: NAD83(2011) Kansas Regional Coordinate System
# Zone 11 (for Kansas City)
ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)

#now get KC boundary
kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)

#plot them both
ggplot() +
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()
#Practice subsetting using the intersection command we learned:
kc_tracts <- ks_mo_tracts[kc_metro, ]

ggplot() +
  geom_sf(data = kc_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

# just get those that fall WITHIN, excluding those that touch the boundary. Two approaches
kc_tracts_within <- ks_mo_tracts %>%
  st_filter(kc_metro, .predicate = st_within)

# Equivalent syntax:
# kc_metro2 <- kc_tracts[kc_metro, op = st_within]

ggplot() +
  geom_sf(data = kc_tracts_within, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()


# Point in Polygon Spatial Joins
library(mapview)

gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.361748, -82.374377,
                -82.38177, -82.259461, -82.367436, -82.404031,
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576,
               29.677201, 29.674923, 29.71099, 29.711587,
               29.648227, 29.624037)
)
#make it an sf
# CRS: NAD83(2011) / Florida North
gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440)

mapview(
  gainesville_sf,
  col.regions = "red",
  legend = FALSE
)

##Get some ACS data on health insurance
alachua_insurance <- get_acs(
  geography = "tract",
  variables = "DP03_0096P",
  state = "FL",
  county = "Alachua",
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate,
         pct_insured_moe = moe) %>%
  st_transform(6440)

#make a map
mapview(
  alachua_insurance,
  zcol = "pct_insured",
  layer.name = "% with health<br/>insurance"
) +
  mapview(
    gainesville_sf,
    col.regions = "red",
    legend = FALSE
  )

## join insurance info from ACS to points
patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance
)
##Polygon-Polygon Spatial Join
# get some CBSA data
# CRS: NAD83(2011) / Texas Centric Albers Equal Area
tx_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)

#get tract-level acs data for all of Texas
pct_hispanic <- get_acs(
  geography = "tract",
  variables = "DP05_0071P",
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)

names(pct_hispanic)
names(tx_cbsa)
# now do the spatial join.
# we are doing an *inner join*
# notice where we put the spatial relationship
# we also have to deal with columns with the same name. that's what the suffix part is for
hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE
)


#cool. let's make a faceted plot to compare census tracts within each of the 4 cbsa
hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
  ggplot() +
  geom_density(aes(x = estimate_tracts), color = "navy", fill = "navy",
               alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~NAME_metro) +
  labs(title = "Distribution of Hispanic/Latino population by Census tract",
       subtitle = "Largest metropolitan areas in Texas",
       y = "Kernel density estimate",
       x = "Percent Hispanic/Latino in Census tract")


##ok your turn, re-create this last analysis with the CBSA for the top 2 CBSA in PA and get PA census tracts (any variable you want. use load_variables to find one that interests you)
# spatially overlay them. create the facet map
#also, use ggplot to make a map of your variable for one of the CBSA.
# you can work with a friend!


# Last thing. Another example of the areal interpolation. What if you overlay polygons with different sizes and they don't perfectly alight
# Load 2020 ACS 5-Year Total Population for Census Tracts (PA only)
tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",  # Total population from ACS
  state = "PA",
  year = 2020,
  geometry = TRUE
)

# Load 2020 ACS 5-Year Total Population for ZCTAs (Must be for entire U.S.)
zctas <- get_acs(
  geography = "zcta",
  variables = "B01003_001",  # Total population from ACS
  year = 2020,
  geometry = TRUE
)

# Filter only ZCTAs within Pennsylvania (if necessary)
zctas <- zctas[st_intersects(zctas, st_union(tracts), sparse = FALSE), ]

# View the first few rows
head(tracts)
head(zctas)


# Check the CRS (Coordinate Reference System) of both layers
st_crs(tracts)
st_crs(zctas)

# Reproject both layers to a common CRS (if needed)
tracts <- st_transform(tracts, 3857)  # Web Mercator
zctas <- st_transform(zctas, 3857)

# Extract only population values from tracts
tracts_population <- tracts["value"]  # Keep only population column

# Perform areal-weighted interpolation to transfer population from tracts to ZCTAs
zcta_population <- st_interpolate_aw(tracts_population, zctas, extensive = TRUE)

# Add the interpolated population estimates to ZCTA layer
zctas$population_estimate <- zcta_population$value
library(ggplot2)

# Map the estimated population per ZCTA
ggplot(zctas) +
  geom_sf(aes(fill = population_estimate), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Estimated Population by ZIP Code (ZCTA) in Pennsylvania",
       fill = "Population") +
  theme_minimal()
