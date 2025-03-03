library(tidyverse)

setwd("C:/Users/zyang/OneDrive/Desktop/MUSA5080_Spring25/Week1")

census2020 <- read.csv("LTDB_Std_2020_fullcount.csv")
census_2000<-read.csv("LTDB_Std_2000_fullcount.csv")



census2020 <- census2020 %>%
  select(whitehhs, pop20,TRTID2010)


census2020<- census2020 %>%
  mutate(pwhitehh=whitehhs/pop20)

census_2020<-census_2020 %>%
  mutate(mwhite= case_when(pwhitehh>0.5~"Majority",
                           TRUE~"not majority"))

#Filter to PA
census_PA<-census2020%>%
  filter(startsWith(as.character(TRTID2010), "42"))
census_PA<-census_PA %>%
  mutate(mwhite= case_when(pwhitehh>0.5~"Majority",
                           TRUE~"not majority"))


# PA_tracts<- filter(census2020,substr(TRTID10,1,2)=="42")

census_PA10<-census_2000 %>%
  filter(startsWith(as.character(TRTID10), "42"))

census_PA10 <- census_PA10 %>%
  select(NHWHT00, POP00,TRTID10)


census_PA10<- census_PA10 %>%
  mutate(pwhitehh=NHWHT00/POP00)

census_PA10<-census_PA10 %>%
  mutate(mwhite= case_when(pwhitehh>0.5~"Majority",
                           TRUE~"not majority"))

census_PA_joined<-left_join(census_PA,census_PA10, by=c("TRTID2010"="TRTID10"))

census_PA_joined<-census_PA_joined %>%
  select(TRTID2010, pwhitehh.x, pwhitehh.y, mwhite.x, mwhite.y)

census_PA_joined<-census_PA_joined %>%
  mutate(transition= case_when(mwhite.x=="Majority" & mwhite.y=="Majority"~"Stable",
                               TRUE~"Not Stable"))


## Calculate some stats
# mean of each tract from 2020
census2020 <- read.csv("LTDB_Std_2020_fullcount.csv")

# mean<- census_2020 %>% summarize(mean=mean(pop20,na.rm=TRUE))
summary(census_2020$pop20)

mean<- census_2020%>%
  filter(!is.na(pop20)) %>%
  summarize(mean=mean(pop20))

census2020$state<-census2020$TRTID2010 %>% substr(1,2)

census2020_groupby<-census2020 %>%
  group_by(state) %>%
  summarize(mean_pop=mean(pop20,na.rm=TRUE))

#official solution
#summary_table<-census%>% mutate(stateID=substr(TRTID2010,1,2))%>%
#  group_by(stateID)%>% summarize(mean_pop=mean(pop20,na.rm=TRUE))


census_PAsumm<-census_PA %>%
  group_by(mwhite)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n))

# Double or multiple group by see my FARS processing code
