install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

nyctib<-flights
class(nyctib)

names(nyctib)

setwd("C:/Users/zyang/Downloads/MUSA5080_Spring25/Week1")

census2020 <- read.csv("LTDB_Std_2020_fullcount.csv")

glimpse(census2020)

names(census2020)

census2020<-rename(census2020, whitehhs=nhwt20)
names(census2020)

census_select <- census2020 %>%
  select(pop20,whitehhs, nhblk20,hisp20,asian20)

# Alternative
census_select<- census2020 %>%
  select(pop20, -ntv20)

census_select<- census2020 %>%
  mutate(pwhitehh=whitehhs/pop20,
         pnhblk=nhblk20/pop20,
         phisp=hisp20/pop20,
         pasian=asian20/pop20)

census_select<-census_select %>%
  mutate(mwhite= case_when(pwhitehh>0.5~"Majority",
                           TRUE~"not majority"))

census2020<-census_select %>%
  select(TRTID2010,pwhitehh,mwhite)

census_2000<-read.csv("LTDB_Std_2000_fullcount.csv")

census_joined<- left_join(census2020, census_2000, by=c("TRTID2010"="TRTID10"))
