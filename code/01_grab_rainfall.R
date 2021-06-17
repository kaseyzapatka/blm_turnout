

rainfall_june <- rbindlist(lapply(c(1948:2020), function(y){
  rbindlist(lapply(c(1:7), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")

    cpc_prcp(paste0(as.character(y), "-06-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-06-", d))
  }))
}))


rainfall_may <- rbindlist(lapply(c(1948:2020), function(y){
  rbindlist(lapply(c(25:31), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")

    cpc_prcp(paste0(as.character(y), "-05-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-05-", d))
  }))
}))

saveRDS(bind_rows(rainfall_may, rainfall_june), "temp/national_rainfall_2000_2020_raw.rds")

rainfall <- readRDS("temp/national_rainfall_2000_2020_raw.rds")

rainfall$date <- as.Date(rainfall$date)

historical <- rainfall %>%
  filter((month(date) == 5 & day(date) > 25)|
           (month(date) == 6 & day(date) <= 7)) %>% 
  group_by(year = year(date), lat, lon) %>%
  summarize(precip = sum(precip))

historical <- historical %>% 
  group_by(lat, lon) %>% 
  mutate(sd = sd(precip),
         mean = mean(precip),
         z = (precip - mean) / sd)

saveRDS(historical, "temp/rainfall_processed_2000_2020.rds")

####################################################

rainfall <- rbindlist(lapply(c(2000:2020), function(y){
  rbindlist(lapply(c(1:31), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")

    cpc_prcp(paste0(as.character(y), "-01-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-01-", d))
  }))
}))

saveRDS(rainfall, "temp/national_rainfall_2000_2020_raw_jan.rds")

rainfall <- readRDS("temp/national_rainfall_2000_2020_raw_jan.rds")


historical <- rainfall %>% 
  mutate(year = substring(date, 1, 4)) %>% 
  group_by(year, lat, lon) %>% 
  summarize(precip = sum(precip)) %>% 
  mutate(g = ifelse(year == "2020", "precip_2020", "precip_historical")) %>% 
  group_by(g, lat, lon) %>% 
  summarize(precip = mean(precip)) %>% 
  pivot_wider(id_cols = c("lat", "lon"), names_from = "g", values_from = "precip")

saveRDS(historical, "temp/rainfall_processed_2000_2020_jan.rds")