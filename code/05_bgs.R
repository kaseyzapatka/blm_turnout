
bgs <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56)$state_code), function(s){
  bgs <- block_groups(s)
  
  bgs$centroid <- st_centroid(bgs$geometry)
  
  bgs$longitude <- unlist(lapply(c(1:nrow(bgs)), function(r){bgs$centroid[[r]][1]}))
  bgs$latitude <- unlist(lapply(c(1:nrow(bgs)), function(r){bgs$centroid[[r]][2]}))
  
  bgs <- st_drop_geometry(bgs) %>% 
    select(-centroid)
  
  bgs_shp <- SpatialPoints(
    select(bgs, x = longitude, y = latitude)
  )
  
  
  protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
    filter(EVENT_DATE > "2020-05-25",
           EVENT_DATE <= "2020-06-07")
  
  ## turn protest locs into spatial object
  protest_sites <- SpatialPoints(
    select(protests, x = LONGITUDE, y = LATITUDE)
  )
  
  
  tree <- createTree(coordinates(protest_sites))
  
  ## find 5 closest protests to each voter
  inds <- knnLookup(tree, newdat = coordinates(bgs_shp), k = 1)
  
  bgs <- cbind(bgs, inds) %>%
    rename(actual = inds)
  
  bgs <- left_join(
    bgs,
    as.data.frame(protest_sites@coords) %>%
      mutate(actual = row_number())
  ) %>%
    rename(actual_x = x,
           actual_y = y) %>%
    dplyr::select(-actual)
  
  bgs$dist <- pointDistance(select(bgs, longitude, latitude),
                            select(bgs, actual_x, actual_y), lonlat = T) * 0.000621371
  
  ################
  rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
    ungroup() %>%
    mutate(id = row_number(),
           lon = ifelse(lon > 180, -360 + lon, lon))
  
  ## turn weather sites into spatial object
  weather_sites <- SpatialPoints(
    select(rainfall, x = lon, y = lat)
  )
  
  ## find closest weather station to each voter
  tree <- createTree(coordinates(weather_sites))
  inds <- knnLookup(tree, newdat = coordinates(bgs_shp), k = 1)
  
  
  bgs <- left_join(cbind(bgs, inds),
                      rainfall,
                      by = c("inds" = "id")) %>%
    mutate(rel = precip_2020 / precip_historical) %>% ## divide 2020 rainfall by historical rainfall
    select(-inds, -lat, -lon)
  
  
  return(bgs)
  
}))

bgs <- select(bgs, GEOID, rel, dist)

census <- readRDS("../regular_data/census_bgs_19.rds")

bgs <- left_join(bgs, census)

bgs$state <- factor(substring(bgs$GEOID, 1, 2))

summary(lm(dist ~ I(log(rel + 1)) + pop_dens + nh_white + nh_black + median_income + unem + some_college +
             median_age, bgs))

summary(lm(dist ~ I(log(rel + 1)), filter(bgs, is.finite(rel), is.finite(pop_dens))))

library(AER)

m1 <- ivreg(vap ~ dist + pop_dens + nh_white + nh_black + median_income + unem + some_college +
              median_age | relr, data = bgs)
