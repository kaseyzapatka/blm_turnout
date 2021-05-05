
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
saveRDS(bgs, "temp/bg1.rds")

################################
bgs <- readRDS("temp/bg1.rds")
to <- readRDS("temp/bg_blm.rds") %>% 
  select(-state)
bgs <- inner_join(bgs, to)

cvap <- vroom("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>% 
  filter(lntitle == "Total") %>% 
  select(GEOID = geoid, cvap = cvap_est) %>% 
  mutate(GEOID = substring(GEOID, 8))

bgs <- left_join(bgs, cvap) %>% 
  mutate_at(vars(starts_with("General")), ~ . / cvap)

bgs$lnd <- log(bgs$dist + 1)
bgs$lnrel <- log(bgs$rel + 1)

m1 <- ivreg(General_2020_11_03 ~ lnd + nh_black +
              nh_white + median_age +
              log(pop_dens) + median_income + some_college +
              General_2018_11_06 +
              General_2016_11_08 +
              General_2014_11_04 +
              General_2012_11_06| . - lnd + lnrel,
            data = filter(bgs, cvap != 0, is.finite(pop_dens)))

summary(m1, diagnostics = T)
summary(lm(lnd ~ lnrel, filter(bgs, is.finite(pop_dens))))
summary(lm(lnd ~ lnrel + log(pop_dens+1) + log(nh_black+1), filter(bgs, is.finite(pop_dens))))
m2 <- lm(lnd ~ lnrel + log(pop_dens+1) + log(nh_black+1), filter(bgs, is.finite(pop_dens)))

bgs$pred <- predict(m2, bgs)

m3 <- lm(General_2020_11_03 ~ pred *log(pop_dens) + nh_black +
           nh_white + median_age +
           log(pop_dens) + median_income + some_college +
           General_2018_11_06 +
           General_2016_11_08 +
           General_2014_11_04 +
           General_2012_11_06,
         data = filter(bgs, cvap != 0, is.finite(pop_dens)))
summary(m3)
############################################


protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(EVENT_DATE > "2020-05-25",
         EVENT_DATE <= "2020-06-07")
