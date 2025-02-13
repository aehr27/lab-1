library(tidyverse)
library(ggplot2)
library(sf)

p.counties <- "./County_Boundaries/County_Boundaries.shp"
p.stations <-"./Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse(d.stations)

d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()

d.counties <- d.counties %>% sf::st_make_valid()

d.counties %>% sf::st_is_valid()



#T.1.1
        
d.counties %>%
  group_by(STATEFP10) %>%
  mutate(
    StateTotalArea = ALAND10+AWATER10,
    CountyPercent = (ALAND10/StateTotalArea) * 100
  )

#T.1.2

d.counties %>%
  group_by(STATEFP10) %>%
  mutate(
    WaterProp = AWATER10 / (ALAND10 + AWATER10)
  ) %>%
  slice_max(WaterProp, n=1)

#answer: Kent County,DE -  District of Columbia,DC -  St. Mary's County, MD -  Cayuga County, NY -  Dauphin County,PA -  Poquoson city, VA - Jefferson County, WV

#T.1.3

d.counties %>%
  group_by(STATEFP10) %>%
  count()

#answer: DE - 3, DC - 1, MD - 24, NY - 20, PA - 43, VA - 102, WV - 14

  
#T.1.4

d.stations %>%
  mutate(NameLength = nchar(STATION_NA)) %>%
  slice_min(NameLength, n=1)

#answer - ABRAM CREEK AT OAKMONT, WV and DRAGON SWAMP AT MASCOT, VA



#T.2.1

d.counties %>% 
  ggplot(aes(x=ALAND10, y=AWATER10, colour=factor(STATEFP10)))+
  geom_point()+
  labs(
    title="Relationship Between Land and Water Area per County",
    x="Land Area",
    y="Water Area"
  )
  
#T.2.2

d.stations %>%
  ggplot(aes(x=Drainage_A))+
  geom_histogram(binwidth=1000, fill="light pink")+
  labs(
    title="Drainage Area for Monitoring Stations",
    x="Drainage Area",
    y="Frequency"
  )



#T.3.1

fun3.1 <- function(vector){
  if(!is.numeric(vector)){
    stop("Error: numeric values only please!! :)")
  }
  A <- list(
    mean=mean(vector),
    median=median(vector),
    maximum=max(vector),
    minimum=min(vector)
  )
  B <- sort(vector)
  results <- list(
    Stats=A, 
    Sorted=B
  )
  return(results)
}

#Tests

vector <- c(1, 0, -1)
results <- fun3.1(vector)
print(results)

vector <- c(10, 100, 1000)
results <- fun3.1(vector)
print(results)

vector <- c(.1, .001, 1e8)
results <- fun3.1(vector)
print(results)

vector <- c("a", "b", "c")
results <- fun3.1(vector)
print(results)



#T.4.1

p.states <- "./tl_2024_us_state/tl_2024_us_state.shp"

d.states <- sf::read_sf(p.states)

glimpse(d.states)

st_crs(d.states)

sf.states <- st_transform(d.states, st_crs(d.stations))

stations_with_state <- st_join(d.stations, sf.states, join = st_intersects)

glimpse(stations_with_state) 

stations_with_state %>%
  group_by(STATEFP) %>%
  count()

#answer: DE - 2, DC - 3, MD - 31, NY - 4, PA - 35, VA - 37, WV - 10
#Some stations are labeled as belonging to one state, but they actually fall outside that states boundary

#T.4.2

ny.counties <- d.counties %>%
  filter(STATEFP10 == 36)

print(ny.counties)

ny.avg <- ny.counties %>%
  mutate(co.area = ALAND10+AWATER10) %>%
  summarize(
    total.ny = sum(co.area),
    countys = n(),
    avg = total.ny/countys)

print(ny.avg)

#answer: 2090000000

#T.4.3

avg.st.drainage <- stations_with_state %>%
  group_by(STATEFP) %>%
  summarize(
    st.drainage = sum(Drainage_A),
    stations = n(),
    avg = st.drainage/stations
  ) %>%
  slice_max(avg, n=1)

print(avg.st.drainage)

#answer: PA - state avg = 3584

