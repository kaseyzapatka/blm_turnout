files <- list.files("temp", pattern = "^ballots_by_bg", full.names = T)

tract_ballots <- rbindlist(lapply(files, readRDS)) %>% 
  filter(!grepl("NA", GEOID)) %>% 
  mutate(GEOID = substring(GEOID, 1, 11)) %>% 
  group_by(GEOID) %>% 
  summarize_all(sum)

census_data <- readRDS("../regular_data/census_tracts_19.rds")

tract_ballots <- left_join(tract_ballots, census_data)

cvap <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/Tract.csv") %>% 
  filter(lntitle == "Total") %>% 
  select(GEOID = geoid, cvap = cvap_est) %>% 
  mutate(GEOID = substring(GEOID, 8))

tract_ballots <- left_join(tract_ballots, cvap)  

tract_ballots$turnout <- tract_ballots$ballots / tract_ballots$cvap
tract_ballots$turnout_16 <- tract_ballots$ballots_16 / tract_ballots$voters
tract_ballots$turnout_18 <- tract_ballots$ballots_18 / tract_ballots$voters
tract_ballots$share_dem <- tract_ballots$democrats / tract_ballots$voters
tract_ballots$share_rep <- tract_ballots$republicans / tract_ballots$voters

###################################

tract_info <- readRDS("temp/tracts_data.rds") %>% 
  select(GEOID, rel, dist)

tract_ballots <- left_join(tract_ballots, tract_info)

tract_z <- rbindlist(lapply(list.files("temp", pattern = "^tracts_interp", full.names = T), readRDS)) %>% 
  mutate(GEOID = paste0(STATE_FIPS, CNTY_FIPS, TRACT)) %>% 
  select(GEOID, z)

tract_ballots <- left_join(tract_ballots, tract_z)

tract_ballots <- tract_ballots[complete.cases(select(tract_ballots, -share_non_citizen,
                                                     -share_moved, -share_no_car)), ]

tract_ballots$state <- substring(tract_ballots$GEOID, 1, 2)

tract_ballots$lndist <- log(tract_ballots$dist + 1)
tract_ballots$lnrel <- log(tract_ballots$rel + 1)
tract_ballots$lnpopdens <- log(tract_ballots$pop_dens)
tract_ballots$lnblack <- log(tract_ballots$nh_black + 1)

#####################################################################
# 
# tracts <- readOGR("C:/Users/morrisk/Downloads/USA_Tracts", "USA_Tracts")
# 
# tracts@data <- select(tracts@data, STATE_FIPS, CNTY_FIPS, TRACT) %>% 
#   mutate(GEOID = paste0(STATE_FIPS, CNTY_FIPS, TRACT)) %>% 
#   select(GEOID)
# 
# tracts@data <- left_join(tracts@data, tract_ballots)
# 
# tracts <- subset(tracts, !is.na(z))
# 
# tract_nbq <- poly2nb(tracts)
# tract_nbq_w <- nb2listw(tract_nbq, zero.policy = T)
# 
# cleanup(c("tracts", "tract_nbq_w"))
# 
# save(tracts, tract_nbq_w, file = "temp/tracts_for_spatial.RData")
# 
# tract_s1 <- lagsarlm(lndist ~ z, tracts, tract_nbq_w, zero.policy = T)

#####################################################################

model1 <- lm(lndist ~ z + log(pop_dens) + log(nh_black+1), tract_ballots)
summary(model1)

model2_tracts <- ivreg(turnout ~ lndist + lnblack + nh_white +
                         share_dem + share_rep + some_college + median_income + median_age +
                         lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                       data = filter(tract_ballots, turnout <= 1.5))

summary(model2_tracts, diagnostics = T)

#########################

fs <- lm(lndist ~ lnrel + share_rep + lnblack + median_age + nh_white +
           share_dem + some_college + median_income +
           lnpopdens + state,
         data = filter(tract_ballots, turnout < 1.5))

tract_ballots$pred <- predict(fs, tract_ballots)


coefs <- rbindlist(lapply(unique(bg_ballots$state), function(s){
  model2 <-  lm(turnout ~ pred + share_rep + lnblack + median_age + nh_white +
                  share_dem + some_college + median_income +
                  lnpopdens,
                data = filter(tract_ballots, turnout < 1.5, state == s))
  
  j <- data.table(confint(model2)) %>% 
    filter(row_number() == 2)
  
  colnames(j) <- c("lower", "upper")
  
  j <- j %>% 
    mutate(estimate = (lower + upper) / 2,
           state = s)
  return(j)
}))

ggplot(filter(coefs, state != "11"), aes(x = reorder(state, estimate), y = estimate)) +
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper))

