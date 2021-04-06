

roll <- rbindlist(lapply(list.files("E:/rolls/ohio/2021_01_22", full.names = T), fread))

colnames(roll) <- clean_names(roll)

roll <- roll %>% 
  select(county_number, date_of_birth, party_affiliation,
         street = residential_address1, city = residential_city,
         state = residential_state, zip = residential_zip,
         general_11_03_2020,
         general_11_06_2018,
         general_11_08_2016,
         general_11_04_2014,
         general_11_06_2012,
         general_11_02_2010)

roll <- geocode(roll)

saveRDS(roll, "temp/oh_coded.rds")

