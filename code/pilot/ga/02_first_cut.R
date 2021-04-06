
protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
  filter(EVENT_DATE > "2020-05-25",
         EVENT_DATE <= "2020-06-07")


voters <- readRDS("temp/geocoded_roll.rds")

vps <- SpatialPoints(
  voters %>%
    select(x = longitude, y = latitude)
)

####################
protest_sites <- SpatialPoints(
  select(protests, x = LONGITUDE, y = LATITUDE)
)

tree <- createTree(coordinates(protest_sites))
inds <- knnLookup(tree, newdat = coordinates(vps), k = 5)

j <- lapply(c(1:5), function(i){

  actual <- inds[,i]

  v1 <- cbind(voters, actual) %>%
    select(voter_id, latitude, longitude, actual)

  v1 <- left_join(
    v1,
    as.data.frame(protest_sites@coords) %>%
      mutate(actual = row_number())
  ) %>%
    rename(actual_x = x,
           actual_y = y) %>%
    dplyr::select(-actual)

  v1$dist <- pointDistance(select(v1, longitude, latitude),
                           select(v1, actual_x, actual_y), lonlat = T) * 0.000621371
  return(v1)
})

voters$dist1 <- j[[1]]$dist
voters$dist2 <- j[[2]]$dist
voters$dist3 <- j[[3]]$dist
voters$dist4 <- j[[4]]$dist
voters$dist5 <- j[[5]]$dist

voters <- mutate(voters,
                 av_5 = (dist1 + dist2 + dist3 + dist4 + dist5) / 5) %>%
  select(-dist2, -dist3, -dist4, -dist5) %>%
  rename(dist = dist1)

cleanup(c("voters", "vps"))

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


#########################
history <- rbindlist(lapply(c(2014, 2016, 2018, 2020), function(y){
  j <- read_fwf(paste0("../rolls/georgia/history/", y, "_full/", y, ".txt"),
                fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1),
                           col_names =
                             c("county", "voter_id", "date",
                               "type", "party", "absentee",
                               "provisional", "supplemental")),
                col_types = "cccccccc") %>%
    dplyr::select(voter_id, party, date)
})) %>%
  filter(date %in% c(20141104,
                     20161108,
                     20181106,
                     20201103,
                     20200609))


voters <- voters %>%
  mutate(v20 = voter_id %in% as.integer(filter(history, date == 20201103)$voter_id),
         v20p = voter_id %in% as.integer(filter(history, date == 20200609)$voter_id),
         v18 = voter_id %in% as.integer(filter(history, date == 20181106)$voter_id),
         v16 = voter_id %in% as.integer(filter(history, date == 20161108)$voter_id),
         v14 = voter_id %in% as.integer(filter(history, date == 20141104)$voter_id),
         black = race == "Black not of Hispanic Origin",
         white = race == "White not of Hispanic Origin",
         female = gender == "F",
         age = 2020 - dob,
         dem = party == "D")

voters$inter <- voters$dist * voters$black
voters$ins2 <- voters$rel * voters$black

saveRDS(voters, "temp/ga_reg_data.rds")

##################################

voters <- readRDS("temp/ga_reg_data.rds")

voters$inter_av <- voters$av_5 * voters$black
voters$ins2 <- voters$rel * voters$black


m1 <- ivreg(v20 ~ dist + black +
              dem + white + age + female +
              v18 + v16 + v14 +
              median_income +
              pop_dens +
              some_college +
              reg_date |. -dist + rel, data = voters)

m2 <- ivreg(v20 ~ dist + black + inter +
              dem + white + age + female +
              v18 + v16 + v14 +
              median_income +
              pop_dens +
              some_college +
              reg_date |. -dist + rel -inter + ins2, data = voters)

m3 <- ivreg(v20 ~ av_5 + black +
              dem + white + age + female +
              v18 + v16 + v14 +
              median_income +
              pop_dens +
              some_college +
              reg_date |. -av_5 + rel, data = voters)

m4 <- ivreg(v20 ~ av_5 + black + inter_av5 +
              dem + white + age + female +
              v18 + v16 + v14 +
              median_income +
              pop_dens +
              some_college +
              reg_date |. -av_5 + rel -inter + ins2, data = voters)

stargazer(m1, m2, m3, m4, type = "text")


# ses_cl <- list(
#   summary(lm.cluster(formula = f1, data = tenp, cluster = tenp$county))[ , 2],
#   summary(lm.cluster(formula = f2, data = tenp, cluster = tenp$county))[ , 2],
#   summary(lm.cluster(formula = f3, data = tenp, cluster = tenp$county))[ , 2]
#   )
#
# save(models1, ses_cl, file = "temp/ga_models.RData")
# ##############################################
#
# stargazer(models1,
#           header = F,
#           type = "text", notes.align = "l",
#           # covariate.labels = c("2018", "Treated", "Years Since Latest Incarceration",
#           #                      "2018 $\\times$ Treated",
#           #                      "2018 $\\times$ Years Since",
#           #                      "Treated $\\times$ Years Since",
#           #                      "2018 $\\times$ Treated $\\times$ Years Since"),
#           dep.var.labels.include = FALSE,
#           title = "\\label{tab:tab-dind} General Election Turnout, 2010 {--} 2018",
#           table.placement = "H",
#           omit.stat = c("f", "ser"),
#           table.layout = "-cm#-t-a-s-n",
#           out = "./temp/bigreg.tex",
#           out.header = F,
#           omit = c("county"),
#           notes = "TO REPLACE",
#           se = ses_cl)
#
# j <- fread("./temp/bigreg.tex", header = F, sep = "+")
#
# note.latex <- "\\multicolumn{4}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered by county) in parentheses.}}}"
#
# j <- j %>%
#   mutate(n = row_number(),
#          V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
#          V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>%
#   filter(!grepl("Note:", V1))
#
# insert1 <- "\\resizebox{1\\textwidth}{!}{%"
# insert2 <- "}"
#
# j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) + 1 - 0.01))) %>%
#   mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>%
#   arrange(n) %>%
#   select(-n)
#
#
# write.table(j, "./temp/dind_reg.tex", quote = F, col.names = F,
#             row.names = F)
#
#
# ############################################
# states <- tigris::counties(state = "GA", year = 2018, cb = T)
#
# map <- ggplot() +
#   geom_point(data = protests, aes(x = LONGITUDE, y = LATITUDE), color = "red", size = 2) +
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
# saveRDS(map, "temp/ga_map_1.rds")
########################

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
  ylab("Predicted 2018 Turnout") + scale_x_continuous() +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "Notes: Distribution of distance from closest protest shown at bottom.
Individual-Level Covariates: Party affiliation; race; age; gender; registration date.
Block-Group Covariates: Median income; share with some college; population density.
Also includes county fixed-effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(xlim = c(0, 40))
saveRDS(p1, "temp/ga_mef_1.rds")
