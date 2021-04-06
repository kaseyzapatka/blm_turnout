
protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(month(EVENT_DATE) == 6)

voters <- readRDS("temp/oh_coded.rds")

vps <- SpatialPoints(
  voters %>%
    select(x = longitude, y = latitude)
)

####################
protest_sites <- SpatialPoints(
  select(protests, x = LONGITUDE, y = LATITUDE)
)

tree <- createTree(coordinates(protest_sites))
inds <- knnLookup(tree, newdat = coordinates(vps), k = 1)


voters <- cbind(voters, inds) %>%
  rename(actual = V1)

voters <- left_join(
  voters,
  as.data.frame(protest_sites@coords) %>%
    mutate(actual = row_number())
) %>%
  rename(actual_x = x,
         actual_y = y) %>%
  dplyr::select(-actual)

voters$dist <- pointDistance(select(voters, longitude, latitude),
                             select(voters, actual_x, actual_y), lonlat = T) * 0.000621371

rm(inds, protest_sites, tree)
##########################

rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
  ungroup() %>%
  mutate(id = row_number(),
         lon = ifelse(lon > 180, -360 + lon, lon))

weather_sites <- SpatialPoints(
  select(rainfall, x = lon, y = lat)
)

tree <- createTree(coordinates(weather_sites))
inds <- knnLookup(tree, newdat = coordinates(vps), k = 1)


voters <- left_join(cbind(voters, inds),
                    rainfall,
                    by = c("V1" = "id")) %>%
  mutate(rel = precip_2020 / precip_historical)
##########################

voters <- voters %>%
  mutate(v12 = general_11_06_2012 != "",
         v14 = general_11_04_2014 != "",
         v16 = general_11_08_2016 != "",
         v18 = general_11_06_2018 != "",
         v20 = general_11_03_2020 != "",
         dem = party_affiliation == "D",
         age = 2020 - year(date_of_birth)) %>%
  select(-starts_with("general_"),
         -city, -state, -street, -zip)
##################################

bgs <- as(tigris::block_groups("OH", year = 2018), "Spatial")

pings  <- SpatialPoints(voters[ , c('longitude','latitude')], proj4string = bgs@proj4string)

voters$GEOID <- over(pings, bgs)$GEOID


voters <- left_join(voters,
                    readRDS("../regular_data/census.RDS"))

pd <- pop_dens("block group", 2018, "OH")

voters <- left_join(voters, pd)
# ##################################

f1 <- v20 ~ dist + dem + age +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

f2 <- v20 ~ dist + dist2 + dem + age +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

f3 <- v20 ~ I(dist < 5) + dem + age +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

# tenp <- voters %>% group_by(county = county_number) %>% sample_frac(0.1) %>%
#   mutate(county = as.factor(county),
#          dist2 = dist * dist)
# saveRDS(tenp, "temp/tenp_oh.rds")

tenp <- readRDS("temp/tenp_oh.rds")

models1 <- lapply(c(f1, f2, f3), function(f){
  m <- lm(f, data = tenp)
})

# 
# ses_cl <- list(
#   summary(lm.cluster(formula = f1, data = tenp, cluster = tenp$county))[ , 2],
#   summary(lm.cluster(formula = f2, data = tenp, cluster = tenp$county))[ , 2],
#   summary(lm.cluster(formula = f3, data = tenp, cluster = tenp$county))[ , 2]
# )
# save(models1, ses_cl, file = "temp/oh_models.RData")
# 
# ############################################
# states <- tigris::counties(state = "OH", year = 2018, cb = T)
# 
# map <- ggplot() + 
#   coord_sf() +
#   geom_point(data = protests, aes(x = LONGITUDE, y = LATITUDE), color = "red", size = 2) +
#   labs(color = "Miles to Closest June Protests") +
#   geom_sf(data = states, fill = NA) +
#   labs(x = NULL, y = NULL) + 
#   theme(axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.background = element_blank(),
#         legend.key=element_blank(),
#         legend.key.width = unit(1.5, "cm"),
#         text = element_text(family = "LM Roman 10", size = 12))
# map
# saveRDS(map, "temp/OH_map_1.rds")
# ########################


marg <- ggeffects::ggeffect(models1[[1]], "dist",
                            vcov.fun = "vcovCR", vcov.type = "CR0", 
                            vcov.args = list(cluster = tenp$county))

marg2 <- ggeffects::ggeffect(models1[[2]], "dist",
                             vcov.fun = "vcovCR", vcov.type = "CR0", 
                             vcov.args = list(cluster = tenp$county))

tot_marg <- bind_rows(
  mutate(marg, Model = "Linear"),
  mutate(marg2, Model = "Squared")
)

p1 <- ggplot() + 
  geom_histogram(aes(x = dist, y = ..count../600000), position="identity", linetype=1,
                 fill="gray60", data = tenp, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = Model), data = tot_marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = Model), alpha=0.25, data = tot_marg) +
  xlab("Miles from Closest Protest") +
  ylab("Predicted 2018 Turnout ") + scale_x_continuous() +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "Notes: Distribution of distance from closest protest shown at bottom.
Individual-Level Covariates: Party affiliation; age; registration date.
Block-Group Covariates: Median income; share with some college; population density.
Also includes county fixed-effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(xlim = c(0, 40))
saveRDS(p1, "temp/oh_mef_1.rds")
