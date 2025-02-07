##Week 3: refresher (or intro) to ggplot with tidycensus
#source: chapter 4 Tidycensus book

library(tidycensus)
library(tidyverse)
#First get some data to play with. These are 2016-2020 ACS estimates
ga_wide <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  output = "wide",
  year = 2020
)

#The basics:
#Initatlize a plot with ggplot() function.
#We usually supply a dataset and an aesthetic, defined with the aes() function.
# The asthetics are mappings of data or elements of the plot like fills or colors

options(scipen = 999)

ggplot(ga_wide, aes(x = medincE)) + 
  geom_histogram()

#the default is to use 30 bins. We can change it using the bins parameter.
ggplot(ga_wide, aes(x = medincE)) + 
  geom_histogram(bins = 15)


#next, a boxplot. Here we need to assign a column to the y-parameter
ggplot(ga_wide, aes(y = medincE)) + 
  geom_boxplot()

#show the relationshionship between 2 variables. Need to specify x & y columns
ggplot(ga_wide, aes(x = medageE, y = medincE)) + 
  geom_point()

#to more clearly visualize the relationship, draw a curve that fits the relationship between the two.
# to do this, we add another 'layer' to the plot
# the method lm fits a straight (linear) line between the points.
ggplot(ga_wide, aes(x = medageE, y = medincE)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#Let's decorate.
# get new data. these are 1-year acs estimates. at the cbsa level.
# fetches the percent of commuters who take public transit to work.
# we will work with 20 largest metros (sorted by summary_var = total pop)
metros <-  get_acs(
  geography = "cbsa",
  variables = "DP03_0021P",
  summary_var = "B01003_001",
  survey = "acs1",
  year = 2019
) %>%
  slice_max(summary_est, n = 20)

#a default visualization. (ugly)
ggplot(metros, aes(x = NAME, y = estimate)) + 
  geom_col()

#start to improve.
# first, the name field is not doing us any favors. reduce to just the first name of the cbsa
# then, reorder the plot from highest to lowest estimate for legibility
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col()


# now we can work on the axis labels using the labs() function
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col() +  
  theme_minimal() + 
  labs(title = "Public transit commute share", 
       subtitle = "2019 1-year ACS estimates", 
       y = "", 
       x = "ACS estimate", 
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package") 

#Now let's really decorate this thing! Notice where we specify colors, transparency, widths
library(scales)

metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col(color = "navy", fill = "navy", 
           alpha = 0.5, width = 0.85) +  
  theme_minimal(base_size = 12, base_family = "Verdana") + 
  scale_x_continuous(labels = label_percent(scale = 1)) + 
  labs(title = "Public transit commute share", 
       subtitle = "2019 1-year ACS estimates", 
       y = "", 
       x = "ACS estimate", 
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package") 

ggsave("metro_transit.png")

ggsave(
  filename = "metro_transit.png",
  path = "~/images",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

#If time permits! Let's go to Maine!
maine <- get_decennial(
  state = "Maine",
  geography = "county",
  variables = c(totalpop = "P1_001N"),
  year = 2020
) %>%
  arrange(desc(value))

#get some income data and clean up the name field. Just leave the name of the county
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))

##make a plot using tricks learned above
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "", 
       y = "ACS estimate") + 
  theme_minimal(base_size = 12.5) + 
  scale_x_continuous(labels = label_dollar())


#we can now add error bars to incorporate MOEs
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_minimal(base_size = 12.5) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "2016-2020 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = label_dollar())

#ACS estimates over time
# get 1-year estimates for Deschutes county, oregon
years <- 2005:2019
names(years) <- years

deschutes_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "OR",
    county = "Deschutes",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")

ggplot(deschutes_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()

#add MOE ribbon and decorate the plot
ggplot(deschutes_value, aes(x = year, y = estimate, group = 1)) + 
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe), 
              fill = "navy",
              alpha = 0.4) + 
  geom_line(color = "navy") + 
  geom_point(color = "navy", size = 2) + 
  theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = label_dollar(scale = .001, suffix = "k")) + 
  labs(title = "Median home value in Deschutes County, OR",
       x = "Year",
       y = "ACS estimate",
       caption = "Shaded area represents margin of error around the ACS estimate")