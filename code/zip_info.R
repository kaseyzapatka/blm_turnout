
## read protest data

protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(EVENT_DATE > "2020-05-25",
         EVENT_DATE <= "2020-06-07",
         ASSOC_ACTOR_1 == "BLM: Black Lives Matter") %>% 
  mutate(id = row_number())

protests$GEOID <- NA

rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup() %>%
  mutate(id = row_number(),
         lon = ifelse(lon > 180, -360 + lon, lon))

rainfall$GEOID <- NA

## read zipcode spatial data
zips <- zctas(class = "sp")
pings  <- SpatialPoints(protests[,c('LONGITUDE','LATITUDE')], proj4string = zips@proj4string)
protests$hold <- over(pings, zips)$GEOID

protests <- mutate(protests,
                   GEOID = ifelse(is.na(GEOID), hold, GEOID))

pings  <- SpatialPoints(rainfall[,c('lon','lat')], proj4string = zips@proj4string)
rainfall$hold <- over(pings, zips)$GEOID

rainfall <- mutate(rainfall,
                   GEOID = ifelse(is.na(GEOID), hold, GEOID))

############################################################
area <- as.data.table(dplyr::select(zips@data, GEOID10, area = ALAND10,
                                    lat = INTPTLAT10,
                                    lon = INTPTLON10))[,c(1:4)] %>%
  mutate(area = as.numeric(area) * 0.00000038610) %>% 
  mutate_at(vars(lat, lon), as.numeric)

pop <- census_race_ethnicity("zcta", 2019)

## merge census data to zip code area, centroid data
zip_data <- full_join(select(pop, GEOID10 = GEOID, population, nh_black), area) %>% 
  mutate(pop_dens = population / area)

############################
## turn zip code centroids into spatial data (the zip codes without weather stations, that is)
zip_points_rain <- SpatialPoints(
  select(zip_data, x = lon, y = lat)
)
## turn weahter site data into spatial
weather_sites <- SpatialPoints(
  select(rainfall, x = lon, y = lat)
)

## find closest weather station to center of each zip code
tree <- createTree(coordinates(weather_sites))
inds <- knnLookup(tree, newdat = coordinates(zip_points_rain), k = 1)

zip_data <- left_join(cbind(zip_data, inds),
                      select(rainfall, id, rel),
                      by = c("inds" = "id")) %>% 
  select(-inds)

### recombine the zipcode with a waether station to the zips we had to find the closest station for

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
zip_data <- zip_data %>% 
  select(-inds, -LATITUDE, -LONGITUDE)


zip_data <- mutate(zip_data,
                     dist = ifelse(GEOID10 %in% protests$GEOID, 0, dist))


zip_data <- left_join(zip_data,
                      rainfall %>% 
                        group_by(GEOID10 = GEOID) %>% 
                        summarize(rel2 = mean(rel)))%>% 
  mutate(rel = ifelse(is.na(rel2), rel, rel2)) %>% 
  select(-rel2)
# 
# zip_data$to_2020 <- rnorm(nrow(zip_data), mean = 0.5, sd = 0.1)
# 
# zip_data <- zip_data[complete.cases(select(zip_data, dist, rel, pop_dens, nh_black)), ]
# 
# m1 <- lm(log(dist+1) ~ log(rel+1) + log(pop_dens + 1) + log(nh_black + 1), zip_data)
# 
# zip_data$predicted_distance <-m1$fitted.values
# 
# saveRDS(zip_data, "temp/zip_data.rds")
