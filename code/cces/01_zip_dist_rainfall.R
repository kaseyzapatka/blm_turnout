
## read protest data

protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(EVENT_DATE > "2020-05-25",
         EVENT_DATE <= "2020-06-07",
         ASSOC_ACTOR_1 == "BLM: Black Lives Matter") %>% 
  mutate(id = row_number())

## read zipcode spatial data
zips <- readOGR("./raw_data/tl_2020_us_zcta510", "tl_2020_us_zcta510")


## find zipcode each protest was in
pings  <- SpatialPoints(protests[,c('LONGITUDE','LATITUDE')], proj4string = zips@proj4string)
protests$GEOID10 <- over(pings, zips)$GEOID10

## read rainfall data
rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup() %>%
  mutate(id = row_number(),
         lon = ifelse(lon > 180, -360 + lon, lon))

## find zip code that each weather station is in
pings  <- SpatialPoints(rainfall[,c('lon','lat')], proj4string = zips@proj4string)
rainfall$GEOID10 <- over(pings, zips)$GEOID10

# for zipcode with weather stations, that zip code's relative rainfall is the mean of all weather stations
rainfall_z <- rainfall %>% 
  group_by(GEOID10) %>% 
  summarize(rel = mean(rel))

###############################
## download zip spatial data? why am I doing this again? who knows
zipsshp <- tigris::zctas()

## keep the GEOID, the area (for pop dens) and the centroid coordinates
area <- as.data.table(dplyr::select(zipsshp, GEOID10, area = ALAND10,
                                    lat = INTPTLAT10,
                                    lon = INTPTLON10))[,c(1:4)] %>%
  mutate(area = as.numeric(area) * 0.00000038610) %>% 
  mutate_at(vars(lat, lon), as.numeric)

## grab the population and racial breakdown of all zips in the coutnry using 2019 ACS data
pop <- census_race_ethnicity("zcta", 2019)

## merge census data to zip code area, centroid data
zip_data <- full_join(select(pop, GEOID10 = GEOID, population, nh_black), area) %>% 
  mutate(pop_dens = population / area)


###########################
## isolate the zipcodes with weather stations in them - we don't need to "find" the closest station to tehm
zr1 <- inner_join(zip_data, rainfall_z)

## isolate the zipcodes that *dont* have a weather station
zr2 <- filter(zip_data, !(GEOID10 %in% rainfall_z$GEOID10))

############################
## turn zip code centroids into spatial data (the zip codes without weather stations, that is)
zip_points_rain <- SpatialPoints(
  select(zr2, x = lon, y = lat)
)
## turn weahter site data into spatial
weather_sites <- SpatialPoints(
  select(rainfall, x = lon, y = lat)
)

## find closest weather station to center of each zip code
tree <- createTree(coordinates(weather_sites))
inds <- knnLookup(tree, newdat = coordinates(zip_points_rain), k = 1)

zr2 <- left_join(cbind(zr2, inds),
                   select(rainfall, id, rel),
                   by = c("inds" = "id")) %>% 
  select(-inds)

### recombine the zipcode with a waether station to the zips we had to find the closest station for

zip_data <- bind_rows(zr1, zr2)

####################
## turn *all* zip codes into spatial data
zip_points_protest <- SpatialPoints(
  select(zip_data, x = lon, y = lat)
)
## turn protest locations into spatial data
protest_sites <- SpatialPoints(
  select(protests, x = LONGITUDE, y = LATITUDE)
)

## find closest protest to each zip code centroid
tree <- createTree(coordinates(protest_sites))
inds <- knnLookup(tree, newdat = coordinates(zip_points_protest), k = 1)

zip_data <- left_join(cbind(zip_data, inds),
                   select(protests, id, LONGITUDE, LATITUDE),
                   by = c("inds" = "id"))

## calculate distance between each zip code centroid and it's closest protest
zip_data$dist <- pointDistance(select(zip_data, lon, lat),
                               select(zip_data, LONGITUDE, LATITUDE), lonlat = T) * 0.000621371

## set distance = 0 if a protest occured anywhere in the zip code
zip_data <- mutate(zip_data, dist = ifelse(GEOID10 %in% protests$GEOID10, 0, dist)) %>% 
  select(-inds, -LONGITUDE, -LATITUDE)

## remove all objects other than "zip_code" from memory (this is a helper function)
cleanup("zip_data")

## keep observations where relative rainfall, population density, share black, and distance to protest are here
zip_data <- zip_data[complete.cases(select(zip_data, dist, rel, pop_dens, nh_black)),]

m1 <- lm(log(dist + 1) ~ log(rel + 1), zip_data)
m2 <- lm(log(dist + 1) ~ log(rel + 1) + log(pop_dens+1), zip_data)
m3 <- lm(log(dist + 1) ~ log(rel + 1) + log(pop_dens+1) + log(nh_black+1), zip_data)

## find the predicted distance for each zip code to the closest protest, based on the characteristics in m3
zip_data$predicted_distance <- fitted.values(m3)
saveRDS(zip_data, "temp/zip_data.rds")
##########################################
