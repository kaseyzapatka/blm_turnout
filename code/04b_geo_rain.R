library(gstat)
ptm <- proc.time()
tracts <- readOGR("raw_data/USA_Tracts", "USA_Tracts")

rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup()

rainfall$rainfall_id <- group_indices(rainfall, as.character(lon), as.character(lat))

rainfall <- rainfall %>% 
  mutate(lon = ifelse(lon > 180, -360 + lon, lon)) %>% 
  filter(year == 2020,
         is.finite(z))
#####################################################################################################

rainsp <- SpatialPoints(filter(rainfall[,c(3, 2)]), proj4string = CRS(proj4string(tracts)))

rainsp <- SpatialPointsDataFrame(rainsp, rainfall)

#############################

hold <- tracts

for(s in unique(filter(fips_codes, state_code < 57)$state_code)){
  if(!(file.exists(paste0("temp/tracts_interp_", s, ".rds")))){
    tracts <- subset(hold, STATE_FIPS == s)
    print(s)
    
    r <- raster(tracts, ncols = 100, nrows = 100)
    ##############################
    
    gs <- gstat(formula=z~1, locations=rainsp)
    idw <- interpolate(r, gs)
    
    districts.reprojected <- spTransform(tracts, CRS(projection(idw)))
    
    extracted.values <- raster::extract(idw, districts.reprojected)
    
    avs <- sapply(extracted.values, mean)
    
    tracts@data$z <- avs
    saveRDS(tracts@data, paste0("temp/tracts_interp_", s, ".rds"))
    proc.time() - ptm 
  }
}


hh <- left_join(
  fortify(tracts),
  tracts@data %>% 
    mutate(id = rownames(tracts@data))
)

ggplot(hh, aes(x = long, y = lat, group = group, fill = z)) + geom_polygon()
