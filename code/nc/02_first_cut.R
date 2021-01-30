
protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(month(EVENT_DATE) == 6,
         ADMIN1 == "North Carolina")


voters <- readRDS("temp/nc_geo.rds")

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

voters <- filter(voters, dist < 1000)

rm(inds, protest_sites, tree, vps)
##########################

hist <- fread("E:/rolls/north_carolina/ncvhis_Statewide_011621/ncvhis_Statewide.txt") %>% 
  dplyr::select(ncid, election_lbl)


ids <- data.table(ncid = unique(voters$ncid)) %>% 
  mutate(v12 = ncid %in% filter(hist, election_lbl == "11/06/2012")$ncid,
         v14 = ncid %in% filter(hist, election_lbl == "11/04/2014")$ncid,
         v16 = ncid %in% filter(hist, election_lbl == "11/08/2016")$ncid,
         v18 = ncid %in% filter(hist, election_lbl == "11/06/2018")$ncid,
         v20 = ncid %in% filter(hist, election_lbl == "11/03/2020")$ncid)

voters <- left_join(voters, ids) %>% 
  mutate(black = race_code == "B" & ethnic_code != "HL",
         white = race_code == "W" & ethnic_code != "HL",
         female = sex_code == "F",
         dem = party_cd == "DEM")
rm(ids)
##################################

bgs <- readOGR("../regular_data/nc_bgs",
               "tl_2019_37_bg")

pings  <- SpatialPoints(voters[ , c('longitude','latitude')], proj4string = bgs@proj4string)

voters$GEOID <- over(pings, bgs)$GEOID

# c1 <- rbindlist(lapply(unique(filter(fips_codes, state == "NC")$county_code), function(c){
#   get_basic_census_stats("block group", 2019, "NC", county = c)
# }))
#   
# c2 <- pop_dens("block group", state = "NC", year = 2019)
# 
# 
# census <- full_join(c1, c2)
# saveRDS(census, "temp/census_nc.rds")

voters <- left_join(voters, census)
##################################

f1 <- v20 ~ dist + dem + black + white + age + female +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

f2 <- v20 ~ dist + dist2 + dem + black + white + age + female +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

f3 <- v20 ~ I(dist < 5) + dem + black + white + age + female +
  v18 + v16 + v14 + 
  median_income +
  pop_dens +
  some_college +
  county

# tenp <- voters %>% group_by(county = county_desc) %>% sample_frac(0.1) %>%
#   mutate(county = as.factor(county),
#          dist2 = dist * dist)
# saveRDS(tenp, "temp/tenp_nc.rds")
# 
# tenp <- readRDS("temp/tenp_nc.rds")

models1 <- lapply(c(f1, f2, f3), function(f){
  m <- lm(f, data = tenp)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = tenp, cluster = tenp$county))[ , 2],
  summary(lm.cluster(formula = f2, data = tenp, cluster = tenp$county))[ , 2],
  summary(lm.cluster(formula = f3, data = tenp, cluster = tenp$county))[ , 2]
)
save(models1, ses_cl, file = "temp/nc_models.RData")
##############################################

stargazer(models1,
          header = F,
          type = "text", notes.align = "l",
          # covariate.labels = c("2018", "Treated", "Years Since Latest Incarceration",
          #                      "2018 $\\times$ Treated",
          #                      "2018 $\\times$ Years Since",
          #                      "Treated $\\times$ Years Since",
          #                      "2018 $\\times$ Treated $\\times$ Years Since"),
          dep.var.labels.include = FALSE,
          title = "\\label{tab:tab-dind} General Election Turnout, 2010 {--} 2018",
          table.placement = "H",
          omit.stat = c("f", "ser"),
          table.layout = "-cm#-t-a-s-n",
          out = "./temp/bigreg.tex",
          out.header = F,
          omit = c("county"),
          notes = "TO REPLACE",
          se = ses_cl)

j <- fread("./temp/bigreg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{4}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered by county) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) + 1 - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)


write.table(j, "./temp/dind_reg.tex", quote = F, col.names = F,
            row.names = F)


############################################
states <- tigris::counties(state = "NC", year = 2018, cb = T)

map <- ggplot() + 
  coord_sf() +
  geom_point(data = protests, aes(x = LONGITUDE, y = LATITUDE), color = "red", size = 2) +
  labs(color = "Miles to Closest June Protests") +
  geom_sf(data = states, fill = NA) +
  labs(x = NULL, y = NULL) + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank(),
        legend.key.width = unit(1.5, "cm"),
        text = element_text(family = "LM Roman 10", size = 12))
map
saveRDS(map, "temp/nc_map_1.rds")
########################

marg <- ggeffects::ggeffect(models1[[1]], "dist")

marg2 <- ggeffects::ggeffect(models1[[2]], "dist")

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
  ylab("2018 Turnout ") + scale_x_continuous() +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "Notes: Distribution of distance from closest protest shown at bottom.
Individual-Level Covariates: Party affiliation; race; age; gender; registration date.
Block-Group Covariates: Median income; share with some college; population density.
Also includes county fixed-effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
saveRDS(p1, "temp/nc_mef_1.rds")
