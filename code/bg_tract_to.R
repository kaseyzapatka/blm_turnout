### THIS SCRIPT TAKES THE RAW L2 VOTER FILES AND DUMPS THEM IN AN SQL DATABASE FOR EASIER QUERYING

### start by connecting to an (empty, at first) SQL database where we'll dump the voter files
db <- dbConnect(SQLite(), "D:/national_file_post20.db")
tabs <- dbListTables(db)

db_hist <- dbConnect(SQLite(), "D:/national_file_post20_history.db")

bg_ballots <- lapply(tabs, function(s){
  if(file.exists(paste0("temp/ballots_by_bg_", s, ".rds"))){
    print(paste0(s, " Already Processed"))
  }else{
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Parties_Description,
                                         Voters_FIPS from [", s, "]")) %>% 
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>% 
      select(LALVOTERID, GEOID, Parties_Description)
    
    hist <- dbGetQuery(db_hist, paste0("select * from ", s, "_history_20"))
    
    tt <- left_join(tt, hist) %>% 
      group_by(GEOID) %>% 
      summarize(ballots = sum(General_2020_11_03 != "" & !is.na(General_2020_11_03) & General_2020_11_03 != "N"),
                ballots_18 = sum(General_2018_11_06 != "" & !is.na(General_2018_11_06) & General_2018_11_06 != "N"),
                ballots_16 = sum(General_2016_11_08 != "" & !is.na(General_2016_11_08) & General_2016_11_08 != "N"),
                voters = n(),
                republicans = sum(Parties_Description == "Republican", na.rm = T),
                democrats = sum(Parties_Description == "Democratic", na.rm = T))
    
    saveRDS(tt, paste0("temp/ballots_by_bg_", s, ".rds"))
  }
})

files <- list.files("temp", pattern = "^ballots_by_bg", full.names = T)

bg_ballots <- rbindlist(lapply(files, readRDS)) %>% 
  filter(!grepl("NA", GEOID))

census_data <- readRDS("../regular_data/census_bgs_19.rds")

bg_ballots <- left_join(bg_ballots, census_data)

cvap <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>% 
  filter(lntitle == "Total") %>% 
  select(GEOID = geoid, cvap = cvap_est) %>% 
  mutate(GEOID = substring(GEOID, 8))

bg_ballots <- left_join(bg_ballots, cvap)  

###################################

bg_info <- readRDS("temp/bgs_data.rds") %>% 
  select(GEOID, rel, dist)

bg_ballots <- left_join(bg_ballots, bg_info)

bg_z <- rbindlist(lapply(list.files("temp", pattern = "^bgs_interp", full.names = T), readRDS)) %>% 
  select(GEOID, z)

bg_ballots <- left_join(bg_ballots, bg_z)

bg_ballots$lndist <- log(bg_ballots$dist + 1)
bg_ballots$turnout <- bg_ballots$ballots / bg_ballots$cvap
bg_ballots$turnout_16 <- bg_ballots$ballots_16 / bg_ballots$voters
bg_ballots$turnout_18 <- bg_ballots$ballots_18 / bg_ballots$voters
bg_ballots$share_dem <- bg_ballots$democrats / bg_ballots$voters
bg_ballots$share_rep <- bg_ballots$republicans / bg_ballots$voters
bg_ballots$state <- substring(bg_ballots$GEOID, 1, 2)

bg_ballots$lnrel <- log(bg_ballots$rel + 1)
bg_ballots$lnpopdens <- log(bg_ballots$pop_dens)
bg_ballots$lnblack <- log(bg_ballots$nh_black + 1)
bg_ballots$median_income <- bg_ballots$median_income / 10000

bg_ballots <- bg_ballots[complete.cases(select(bg_ballots, -share_non_citizen,
                                               -share_moved, -share_no_car)), ]


model2_bgs <- ivreg(turnout ~ lndist + nh_black + nh_white + latino +
                      share_dem + share_rep + some_college + median_income + median_age +
                      lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                    data = filter(bg_ballots, turnout < 1.5))

summary(model2_bgs, diagnostics = T)

cleanup(c("bg_ballots", "model2_bgs"))

##################################
##################################
##################################
##################################

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

tract_ballots$pred <- predict(model1, tract_ballots)
tract_ballots$state <- substring(tract_ballots$GEOID, 1, 2)

tract_ballots$lndist <- log(tract_ballots$dist + 1)
tract_ballots$lnrel <- log(tract_ballots$rel + 1)
tract_ballots$lnpopdens <- log(tract_ballots$pop_dens)
tract_ballots$lnblack <- log(tract_ballots$nh_black + 1)
tract_ballots$median_income <- tract_ballots$median_income / 10000


model2_tracts <- ivreg(turnout ~ lndist + nh_black + nh_white + latino +
                         share_dem + share_rep + some_college + median_income + median_age +
                         lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                       data = filter(tract_ballots, turnout <= 1.5))

summary(model2_tracts, diagnostics = T)

summary(lm(lndist ~ z + nh_black + nh_white + latino +
             share_dem + share_rep + some_college + median_income + median_age +
             lnpopdens + turnout_16 + turnout_18 + state,
           data = filter(tract_ballots, turnout <= 1.5)))

summary(lm(lndist ~ z + nh_black + nh_white + latino +
             share_dem + share_rep + some_college + median_income + median_age +
             lnpopdens + turnout_16 + turnout_18 + state,
           data = filter(bg_ballots, turnout <= 1.5)))

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


######################
state_shapes <- get_acs("state", variables = "B17001_002",
                        geometry = T, shift_geo = T) %>% 
  select(-estimate)

state_shapes <- left_join(state_shapes,
                          coefs, by = c("GEOID" = "state"))

state_shapes$e2 <- state_shapes$estimate < 0

gg <- ggplot() +
  geom_sf(aes(), data = filter(state_shapes), fill = "white") +
  geom_sf(aes(fill = e2), data = filter(state_shapes, !is.na(e2))) +
  coord_sf(datum = NA) + theme_bc(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5)) +
  scale_fill_manual(values = c("light green", "light blue", "pink")) +
  geom_point(aes(x=2300000, y = -450000), fill = "light green", shape = 22, size = 5) +
  annotate(geom = "text", x = 2500000, y = -450000, label = "DC", family = "BentonSans") +
  labs(x = NULL, y = NULL, fill = "New Law") +
  ggtitle("Sean Morales-Doyle's Handiwork")

gg

########################
rob.fit1        <- coeftest(model2_bgs, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(model2_tracts, function(x) vcovHC(x, type="HC0"))
summ.fit1 <- summary(model2_bgs, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit2 <- summary(model2_tracts, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)

stargazer(model2_bgs, model2_tracts, type = "text", omit = "state",
          column.labels = c("Block Group-Level", "Tract-Level"),
          dep.var.labels.include = FALSE,
          covariate.labels = c("Log(Distance + 1)",
                               "Share Black",
                               "Share White",
                               "Share Latino",
                               "Share Democrats",
                               "Share Republicans",
                               "Share with Some College",
                               "Median Income (dollarsign10,000s)",
                               "Median Age",
                               "Log(Population Density + 1)",
                               "Turnout in 2016",
                               "Turnout in 2018"), 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"]), 
          table.layout = "-cm#-t-a-s-n",
          table.placement = "H",
          notes = "TO REPLACE",
          out.header = F,
          add.lines = list(c("State Fixed Effects", "Yes", "Yes"),
                           c(rownames(summ.fit1$diagnostics)[1], 
                             pvalue(summ.fit1$diagnostics[1, "p-value"], add_p = T), 
                             pvalue(summ.fit2$diagnostics[1, "p-value"], add_p = T)), 
                           c(rownames(summ.fit1$diagnostics)[2], 
                             pvalue(summ.fit1$diagnostics[2, "p-value"], add_p = T), 
                             pvalue(summ.fit2$diagnostics[2, "p-value"], add_p = T))),
          title = "\\label{tab:bg-tract-to} Neighborhood Turnout in 2020",
          out = "temp/raw_reg_bg_tract.tex")

j <- fread("temp/raw_reg_bg_tract.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{3}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors in parentheses.}}}"

j <- j %>%
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>%
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{!}{.45\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(5.1, nrow(j) + 1 - 0.01))) %>%
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>%
  arrange(n) %>%
  select(-n)

write.table(j, "./temp/tract_bg_to.tex", quote = F, col.names = F,
            row.names = F)
