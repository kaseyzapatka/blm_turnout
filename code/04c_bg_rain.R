library(gstat)

tt <- block_groups("HI", county = "Kalawao", class = "sp")

rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup()

rainfall$rainfall_id <- group_indices(rainfall, as.character(lon), as.character(lat))

rainfall <- rainfall %>% 
  mutate(lon = ifelse(lon > 180, -360 + lon, lon)) %>% 
  filter(year == 2020,
         is.finite(z))
#####################################################################################################

rainsp <- SpatialPoints(filter(rainfall[,c(3, 2)]), proj4string = CRS(proj4string(tt)))

rainsp <- SpatialPointsDataFrame(rainsp, rainfall)


for(s in unique(filter(fips_codes, state_code < 57)$state_code)){
  bgs <- block_groups(state = s, class = "sp")
  
  print(s)
  
  r <- raster(bgs, ncols = 100, nrows = 100)
  ##############################
  
  gs <- gstat(formula=z~1, locations=rainsp)
  idw <- interpolate(r, gs)
  
  districts.reprojected <- spTransform(bgs, CRS(projection(idw)))
  
  extracted.values <- raster::extract(idw, districts.reprojected)
  
  avs <- sapply(extracted.values, mean)
  
  bgs@data$z <- avs
  saveRDS(bgs@data, paste0("temp/bgs_interp_", s, ".rds"))
  
}


hh <- left_join(
  fortify(bgs),
  bgs@data %>% 
    mutate(id = rownames(bgs@data))
)

ggplot(hh, aes(x = long, y = lat, group = group, fill = z)) + geom_polygon()
