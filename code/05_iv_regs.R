library(RSQLite)
library(rgdal)
library(data.table)
library(tidyverse)
library(ivreg)
library(biglm)

# 
# db_final <- dbConnect(SQLite(), "temp/national_file_post20_blm.db")
# 
# nat <- rbindlist(lapply(dbListTables(db_final), function(tab){
#   print(tab)
#   
#   d <- dbGetQuery(db_final, paste0("select * from [", tab, "]"))
#   
#   d$rel_black <- d$black * d$rel
#   d$dist_black <- d$black * d$dist
#   
#   d$rel_dem <- d$dem * d$rel
#   d$dist_dem <- d$dem * d$dist
#   
#   m1 <- ivreg(General_2020_11_03 ~ dist +
#                 male + age + dem + white + black +
#                 pop_dens + median_income + some_college +
#                 General_2018_11_06 +
#                 General_2016_11_08 +
#                 General_2014_11_04 +
#                 General_2012_11_06 | . - dist + rel,
#               data = d)
#   
#   
#   print(summary(m1))
#   return(d)
# }))
# 
# 
# saveRDS(nat, "temp/nat_blm.rds")
# 
# saveRDS(nat %>% 
#           group_by(GEOID) %>% 
#           summarize_at(vars(starts_with("General")), sum),
#         "temp/bgs_blm.rds")

nat <- readRDS("temp/nat_blm.rds")

nat <- nat %>% 
  mutate(rel_black = ifelse(black, rel, 0),
         dist_black = ifelse(black, dist, 0),
         rel_dem = ifelse(dem, rel, 0),
         dist_dem = ifelse(dem, dist, 0))

states <- unique(nat$state)

m1 <- biglm(dist ~ rel + pop_dens + nh_black,
            filter(nat, state == states[1]))

for(i in c(2:length(states))){
  m1 <- update(m1,
              filter(nat, state == states[i]))
}
summary(m1)
nat$pred <- predict(m1, newdata = nat)

m2 <- biglm(General_2020_11_03 ~ pred +
              male + age + dem + white + black +
              pop_dens + median_income + some_college +
              General_2018_11_06 +
              General_2016_11_08 +
              General_2014_11_04 +
              General_2012_11_06,
            filter(nat, state == states[1]))

for(i in c(2:length(states))){
  m2 <- update(m2,
               filter(nat, state == states[i]))
}

summary(m2)

save(m1, m2, file = "temp/regs.rdata")

# 
# m1 <- ivreg(General_2020_11_03 ~ dist +
#               male + age + dem + white + black +
#               pop_dens + median_income + some_college +
#               General_2018_11_06 +
#               General_2016_11_08 +
#               General_2014_11_04 +
#               General_2012_11_06 + state | . - dist + rel,
#             data = nat)
# summary(m1)
# m2 <- ivreg(General_2020_11_03 ~ dist + dist_black +
#               male + age + dem + white + black +
#               pop_dens + median_income + some_college +
#               General_2018_11_06 +
#               General_2016_11_08 +
#               General_2014_11_04 +
#               General_2012_11_06 + state | . - dist - dist_black + rel + rel_black,
#             data = nat)
# summary(m2)
# m3 <- ivreg(General_2020_11_03 ~ dist + dist_dem +
#               male + age + dem + white + black +
#               pop_dens + median_income + some_college +
#               General_2018_11_06 +
#               General_2016_11_08 +
#               General_2014_11_04 +
#               General_2012_11_06 + state | . - dist - dist_dem + rel + rel_dem,
#             data = nat)
# summary(m3)
# save(m1, m2, m3, file = "temp/full_ivs.rdata")