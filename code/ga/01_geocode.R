
rolls <- fread("E:/rolls/georgia/ga_20201102/GA 2020-11-02/Georgia_Daily_VoterBase.txt")


rolls <- rolls %>% 
  select(county = COUNTY_CODE,
         voter_id = REGISTRATION_NUMBER,
         status = VOTER_STATUS,
         street1 = RESIDENCE_HOUSE_NUMBER,
         street2 = RESIDENCE_STREET_NAME,
         street3 = RESIDENCE_STREET_SUFFIX,
         city = RESIDENCE_CITY,
         zip = RESIDENCE_ZIPCODE,
         dob = BIRTHDATE,
         reg_date = REGISTRATION_DATE,
         race = RACE_DESC,
         gender = GENDER,
         cd = CONGRESSIONAL_DISTRICT,
         party = PARTY_LAST_VOTED) %>% 
  mutate(state = "GA")

rolls <- clean_streets(rolls, c("street1", "street2", "street3"))


rolls <- geocode(rolls)

saveRDS(rolls, "temp/geocoded_roll.rds")
