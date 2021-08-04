### THIS SCRIPT TAKES THE RAW L2 VOTER FILES AND DUMPS THEM IN AN SQL DATABASE FOR EASIER QUERYING
# 
# ### start by connecting to an (empty, at first) SQL database where we'll dump the voter files
# db <- dbConnect(SQLite(), "D:/national_file_post20.db")
# tabs <- dbListTables(db)
# 
# db_hist <- dbConnect(SQLite(), "D:/national_file_post20_history.db")
# 
# bg_ballots <- lapply(tabs, function(s){
#   if(file.exists(paste0("temp/ballots_by_bg_", s, ".rds"))){
#     print(paste0(s, " Already Processed"))
#   }else{
#     print(paste0("Processing ", s))
#     code_good <- unique(filter(fips_codes, state == s)$state_code)
#     tt <- dbGetQuery(db, paste0("select LALVOTERID,
#                                          Residence_Addresses_CensusTract,
#                                          Residence_Addresses_CensusBlockGroup,
#                                          Parties_Description,
#                                          Voters_FIPS from [", s, "]")) %>% 
#       mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
#                             str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
#                             Residence_Addresses_CensusBlockGroup)) %>% 
#       select(LALVOTERID, GEOID, Parties_Description)
#     
#     hist <- dbGetQuery(db_hist, paste0("select * from ", s, "_history_20"))
#     
#     tt <- left_join(tt, hist) %>% 
#       group_by(GEOID) %>% 
#       summarize(ballots = sum(General_2020_11_03 != "" & !is.na(General_2020_11_03) & General_2020_11_03 != "N"),
#                 ballots_18 = sum(General_2018_11_06 != "" & !is.na(General_2018_11_06) & General_2018_11_06 != "N"),
#                 ballots_16 = sum(General_2016_11_08 != "" & !is.na(General_2016_11_08) & General_2016_11_08 != "N"),
#                 voters = n(),
#                 republicans = sum(Parties_Description == "Republican", na.rm = T),
#                 democrats = sum(Parties_Description == "Democratic", na.rm = T))
#     
#     saveRDS(tt, paste0("temp/ballots_by_bg_", s, ".rds"))
#   }
# })

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
  select(GEOID, dist)

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

bg_ballots$lnpopdens <- log(bg_ballots$pop_dens)
bg_ballots$lnblack <- log(bg_ballots$nh_black + 1)
bg_ballots$median_income <- bg_ballots$median_income / 10000

bg_ballots <- bg_ballots[complete.cases(select(bg_ballots, -share_non_citizen,
                                               -share_moved, -share_no_car)), ]


model2_bgs <- ivreg(turnout ~ lndist + nh_black + nh_white + latino +
                      share_dem + share_rep + some_college + median_income + median_age +
                      lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                    data = filter(bg_ballots, turnout < 1.5))

model3_bgs <- ivreg(turnout ~ lndist * nh_black + nh_white + latino +
                      share_dem + share_rep + some_college + median_income + median_age +
                      lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                    data = filter(bg_ballots, turnout < 1.5))

summary(model2_bgs, diagnostics = T)

# saveRDS(bg_ballots, "C:/Users/morrisk/Dropbox/blm_turnout/full_bg_data.rds")

cleanup(c("bg_ballots", "model2_bgs", "model3_bgs"))

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
  select(GEOID, dist)

tract_ballots <- left_join(tract_ballots, tract_info)

tract_z <- rbindlist(lapply(list.files("temp", pattern = "^tracts_interp", full.names = T), readRDS)) %>% 
  mutate(GEOID = paste0(STATE_FIPS, CNTY_FIPS, TRACT)) %>% 
  select(GEOID, z)

tract_ballots <- left_join(tract_ballots, tract_z)

tract_ballots <- tract_ballots[complete.cases(select(tract_ballots, -share_non_citizen,
                                                     -share_moved, -share_no_car)), ]

tract_ballots$state <- substring(tract_ballots$GEOID, 1, 2)

tract_ballots$lndist <- log(tract_ballots$dist + 1)
tract_ballots$lnpopdens <- log(tract_ballots$pop_dens)
tract_ballots$lnblack <- log(tract_ballots$nh_black + 1)
tract_ballots$median_income <- tract_ballots$median_income / 10000

# saveRDS(tract_ballots, "C:/Users/morrisk/Dropbox/blm_turnout/full_tract_data.rds")
model2_tracts <- ivreg(turnout ~ lndist + nh_black + nh_white + latino +
                         share_dem + share_rep + some_college + median_income + median_age +
                         lnpopdens + turnout_16 + turnout_18 + state |. -lndist + z,
                       data = filter(tract_ballots, turnout <= 1.5))

model3_tracts <- ivreg(turnout ~ lndist * nh_black + nh_white + latino +
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
##########
rob.fit1        <- coeftest(model2_bgs, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(model3_bgs, function(x) vcovHC(x, type="HC0"))
rob.fit3        <- coeftest(model2_tracts, function(x) vcovHC(x, type="HC0"))
rob.fit4        <- coeftest(model3_tracts, function(x) vcovHC(x, type="HC0"))

summ.fit1 <- summary(model2_bgs, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit2 <- summary(model3_bgs, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit3 <- summary(model2_tracts, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit4 <- summary(model3_tracts, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)

stargazer(model2_bgs, model3_bgs, model2_tracts, model3_tracts, type = "text", omit = "state",
          column.labels = c("Block Group-Level", "Tract-Level"),
          column.separate = c(2, 2),
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
                               "Turnout in 2018",
                               "Log(Distance + 1) \\times Share Black"), 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"], rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"]), 
          table.layout = "-cm#-t-a-s-n",
          table.placement = "H",
          notes = "TO REPLACE",
          out.header = F,
          add.lines = list(c("State Fixed Effects", "Yes", "Yes", "Yes", "Yes"),
                           c(rownames(summ.fit1$diagnostics)[1], 
                             pvalue(summ.fit1$diagnostics[1, "p-value"], add_p = T), 
                             pvalue(summ.fit2$diagnostics[1, "p-value"], add_p = T),
                             pvalue(summ.fit3$diagnostics[1, "p-value"], add_p = T),
                             pvalue(summ.fit4$diagnostics[1, "p-value"], add_p = T)), 
                           c(rownames(summ.fit1$diagnostics)[2], 
                             pvalue(summ.fit1$diagnostics[2, "p-value"], add_p = T), 
                             pvalue(summ.fit2$diagnostics[2, "p-value"], add_p = T),
                             pvalue(summ.fit3$diagnostics[2, "p-value"], add_p = T),
                             pvalue(summ.fit4$diagnostics[2, "p-value"], add_p = T))),
          title = "\\label{tab:bg-tract-to} Neighborhood Turnout in 2020",
          out = "temp/raw_reg_bg_tract.tex")

j <- fread("temp/raw_reg_bg_tract.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors in parentheses.}}}"

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

########################################

bg <- lm(lndist ~ z + nh_black + nh_white + latino +
              share_dem + share_rep + some_college + median_income + median_age +
              lnpopdens + turnout_16 + turnout_18 + state,
            data = filter(bg_ballots, turnout < 1.5))

t <- lm(lndist ~ z + nh_black + nh_white + latino +
           share_dem + share_rep + some_college + median_income + median_age +
           lnpopdens + turnout_16 + turnout_18 + state,
         data = filter(tract_ballots, turnout < 1.5))


stargazer(bg, t, type = "text", omit = "state",
          column.labels = c("Block Group-Level", "Tract-Level"),
          dep.var.labels.include = FALSE,
          covariate.labels = c("Rainfall z-score",
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
          table.layout = "-cm#-t-a-s-n",
          table.placement = "H",
          notes = "TO REPLACE",
          out.header = F,
          add.lines = list(c("State Fixed Effects", "Yes", "Yes")),
          title = "\\label{tab:bg-tract-dist} Neighborhood Distance to Protest in 2020",
          out = "temp/raw_dist_bg_tract.tex")

j <- fread("temp/raw_dist_bg_tract.tex", header = F, sep = "+")

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

write.table(j, "./temp/tract_bg_dist.tex", quote = F, col.names = F,
            row.names = F)

###############################

marg <- ggeffect(model3_tracts, terms = c("lndist[all]", "nh_black[0,0.25,0.75]"))

marg <- marg %>% 
  mutate(group = ifelse(group == 0, "0%",
                        ifelse(group == 0.25, "25%", "75%")))

p1 <- ggplot() + 
  geom_histogram(aes(x = lndist, y = ..count../18000), position="identity", linetype=1,
                 fill="gray60", data = tract_ballots, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = group), data = filter(marg, x < log(30))) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25, data = filter(marg, x < log(30))) +
  xlab("Predicted Distance to Protest (Miles)") +
  ylab("Predicted Turnout in 2020") + scale_x_continuous(breaks = c(log(1+1), log(2+1), log(5+1), log(10+1)),
                                                         labels = c("1", "2", "5", "10")) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(xlim = c(0, log(30)), ylim = c(0, 1)) +
  labs(caption = "Notes: Distribution of distance to protest shown at bottom.
Covariates: Racial characteristics, partisan characteristics, education, median income, population density,
median age, turnout in 2016 and 2018",
       color = "Share Black") +
  guides(fill = "none") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0))

saveRDS(p1, "temp/marg_eff_tract.rds")
