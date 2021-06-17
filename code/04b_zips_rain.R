zips <- readOGR("raw_data/tl_2020_us_zcta510", "tl_2020_us_zcta510")


rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup()

rainfall$rainfall_id <- group_indices(rainfall, as.character(lon), as.character(lat))

rainfall <- rainfall %>% 
  mutate(lon = ifelse(lon > 180, -360 + lon, lon)) %>% 
  filter(year == 2020,
         is.finite(z))
#####################################################################################################

rainsp <- SpatialPoints(filter(rainfall[,c(3, 2)]), proj4string = CRS(proj4string(zips)))

rainsp <- SpatialPointsDataFrame(rainsp, rainfall)

#############################

r <- raster(zips, ncols = 1000, nrows = 1000)
##############################

gs <- gstat(formula=z~1, locations=rainsp)
idw <- interpolate(r, gs)

districts.reprojected <- spTransform(zips, CRS(projection(idw)))

extracted.values <- raster::extract(idw, districts.reprojected)

avs <- sapply(extracted.values, mean)

zips@data$z <- avs
saveRDS(zips@data, "temp/zips_interp_rain.rds")


z2 <- subset(zips, substring(GEOID10, 1, 1) == "1")

hh <- left_join(
  fortify(z2),
  z2@data %>% 
    mutate(id = rownames(z2@data))
)

ggplot(hh, aes(x = long, y = lat, group = group, fill = z)) + geom_polygon()
