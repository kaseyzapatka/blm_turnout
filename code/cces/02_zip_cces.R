##########################
zip_data <- readRDS("temp/zip_data.rds")

zip_data <- zip_data[complete.cases(select(zip_data, dist, rel, pop_dens, nh_black)), ]

m1_dist <- lm(log(dist+1) ~ log(rel+1), zip_data)

zip_data$pred <- predict(m1_dist)
############################
cces <- fread("../regular_data/cces/CCES20_Common_OUTPUT.csv") %>% 
  mutate(id = V1,
         diagnosed = CC20_309a_5 == 2,
         died = CC20_309b_4 == 2,
         died = ifelse(is.na(died), 0, died),
         lookupzip = str_pad(lookupzip, width = 5, side = "left", pad = "0")) %>% 
  select(ideo = CC20_340a,
         approval = CC20_320a,
         birthyr,
         gender,
         educ,
         race,
         diagnosed,
         died,
         voted = CC20_401,
         faminc_new,
         commonpostweight,
         state = inputstate,
         lookupzip,
         party = CC20_360,
         police = CC20_307,
         starts_with("CC20_334"),
         id,
         lookupzip) %>% 
  mutate(voted = voted == 5) %>% 
  mutate_at(vars(gender, educ, race, state, party), as.factor) %>% 
  mutate(white = race == 1,
         black = race == 2,
         strong = ifelse(approval %in% c(1, 4), 1, 0),
         dem = party == 2,
         pos_police = police %in% c(1, 2))

income_lu <- data.frame("faminc_new" = c(1:16),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                                     90000, 110000, 135000, 175000, 225000, 300000, 425000, 500000))

cces <- left_join(cces, income_lu)

cces$police_lib_1 <- as.integer(cces$CC20_334a == 1)
cces$police_lib_2 <- as.integer(cces$CC20_334b == 1)
cces$police_lib_3 <- as.integer(cces$CC20_334c == 2)
cces$police_lib_4 <- as.integer(cces$CC20_334d == 1)
cces$police_lib_5 <- as.integer(cces$CC20_334e == 1)
cces$police_lib_6 <- as.integer(cces$CC20_334f == 1)
cces$police_lib_7 <- as.integer(cces$CC20_334g == 1)

cces$p2 <- cces$police_lib_1 +
  cces$police_lib_2 +
  cces$police_lib_3 +
  cces$police_lib_4 +
  cces$police_lib_5 +
  cces$police_lib_6 +
  cces$police_lib_7

libber <- select(cces, id, starts_with("police_lib"))
libber <- libber[complete.cases(libber), ]

alpha <- alpha(select(libber, -id))
alpha <- alpha$total$raw_alpha
alpha

factors <- factanal(select(libber, -id), factors = 1, scores = "regression")

libber$police_factor <- factors$scores

cces <- left_join(cces, select(libber, id, police_factor))

cces <- left_join(cces, zip_data, by = c("lookupzip" = "GEOID10"))

cces$lnd <- log(cces$dist+1)
cces$lnrel <- log(cces$rel+1)
cces$ideo <- factor(cces$ideo)

m1 <- lm(voted ~ pred*ideo + birthyr + gender + educ + white+pop_dens+nh_black+income,
         data = cces, weight = commonpostweight)

summary(m1)
h <- ggeffect(m1, terms = c("lnd", "dem")) %>% 
  filter(group %in% c(1, 4, 7)) %>% 
  mutate(group = ifelse(group == "1", "Very Liberal",
                        ifelse(group == 4, "Middle of the Road",
                               "Very Conservative")))

h$group <- factor(h$group, levels = c("Very Liberal", "Middle of the Road", "Very Conservative"))

h$x <- exp(h$x) -1

ggplot(h) +
  geom_rug(aes(x = exp(pred)-1), sides="b", data = sample_frac(cces, 0.01)) +
  geom_line(aes(x = x, y = predicted, color = group)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.25) +
  theme_bc(base_family = "LM Roman 10") +
  labs(x = "Predicted Distance from BLM Protest (as function of rainfall)",
       y = "Predicted Turnout",
       color = "Ideology",
       fill = "Ideology",
       caption = "First state covariates: relative rainfall, population density, Black share of population.
Second stage covariates: Predicted distance from protest, age, gender, education. state.") +
  # coord_cartesian(xlim = c(0, 50), ylim = c(0.85, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("blue", "green", "red")) +
  scale_color_manual(values = c("blue", "green", "red"))

#####################################################
m2 <- lm(police_factor ~ pred*dem + birthyr + gender + educ + white + state,
         data = cces, weight = commonweight)

summary(m2)
h <- ggeffect(m2, terms = c("pred", "dem"))


ggplot(h) +
  geom_rug(aes(x = pred), sides="b", data = sample_frac(cces, 0.01)) +
  geom_line(aes(x = x, y = predicted, color = group)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.25) +
  theme_bc(base_family = "LM Roman 10") +
  labs(x = "Predicted Distance from BLM Protest (as function of rainfall)",
       y = "Predicted Turnout",
       color = "Ideology",
       fill = "Ideology",
       caption = "First state covariates: relative rainfall, population density, Black share of population.
Second stage covariates: Predicted distance from protest, age, gender, education. state.") +
  # coord_cartesian(xlim = c(0, 50), ylim = c(0.85, 1)) +
  scale_y_continuous(labels = scales::percent)




summary(lm(log(dist + 1) ~ log(rel + 1), zip_data))
summary(lm(log(dist + 1) ~ log(rel + 1) + log(pop_dens + 1) + log(nh_black + 1), zip_data))
