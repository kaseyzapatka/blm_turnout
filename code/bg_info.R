
## read protest data

protests <- read_xlsx("raw_data/protests/USA_2020_2021_Apr30.xlsx") %>%
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
for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  if(!file.exists(paste0("temp/bgs_", s, ".rds"))){
  zips <- block_groups(state = s, class = "sp")
  pings  <- SpatialPoints(protests[,c('LONGITUDE','LATITUDE')], proj4string = zips@proj4string)
  protests$hold <- over(pings, zips)$GEOID
  
  protests <- mutate(protests,
                     GEOID = ifelse(is.na(GEOID), hold, GEOID))
  
  
  area <- as.data.table(dplyr::select(zips@data, GEOID, area = ALAND,
                                      lat = INTPTLAT,
                                      lon = INTPTLON))[,c(1:4)] %>%
    mutate(area = as.numeric(area) * 0.00000038610) %>% 
    mutate_at(vars(lat, lon), as.numeric)
  
  pop <- census_race_ethnicity("block group", 2019, state = s)
  
  ## merge census data to zip code area, centroid data
  zip_data <- full_join(select(pop, GEOID = GEOID, population, nh_black), area) %>% 
    mutate(pop_dens = population / area)
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
  
  saveRDS(zip_data, paste0("temp/bgs_", s, ".rds"))
  }}


#################################### read protest data

protests <- read_xlsx("raw_data/protests/USA_2020_2021_Apr30.xlsx") %>%
  filter(EVENT_DATE > "2020-05-25",
         EVENT_DATE <= "2020-06-07",
         ASSOC_ACTOR_1 == "BLM: Black Lives Matter") %>% 
  mutate(id = row_number())

protests$GEOID <- NA

## read zipcode spatial data
for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  zips <- block_groups(state = s, class = "sp")
  pings  <- SpatialPoints(protests[,c('LONGITUDE','LATITUDE')], proj4string = zips@proj4string)
  protests$hold <- over(pings, zips)$GEOID
  
  protests <- mutate(protests,
                     GEOID = ifelse(is.na(GEOID), hold, GEOID))
  
}

saveRDS(protests, "temp/protest_bgs.rds")

#######################################################

files <- list.files(path = "temp/", pattern = "^bgs_[0-9]", full.names = T)


all_tracts <- rbindlist(lapply(files, readRDS))

all_tracts <- mutate(all_tracts,
                     dist = ifelse(GEOID %in% readRDS("temp/protest_bgs.rds")$GEOID, 0, dist))


saveRDS(all_tracts, "temp/bgs_data.rds")
