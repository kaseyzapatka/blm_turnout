
roll <- fread("E:/rolls/north_carolina/roll_full_20210101.csv",
              select = c("county_desc", "voter_reg_num", "ncid", "status_cd",
                         "voter_status_reason_desc", "last_name", "first_name", "midl_name",
                         "house_num", "street_dir", "street_name", "street_type_cd",
                         "res_city_desc", "state_cd", "zip_code", "race_code", "ethnic_code",
                         "party_cd", "sex_code", "age", "registr_dt", "cancellation_dt"), fill = T, header = T)


actives <- filter(roll, status_cd %in% c("A", "I"))
rm(roll)
actives <- clean_streets(actives, c("house_num", "street_dir", "street_name",
                                    "street_type_cd"))

actives <- geocode(rename(actives, state = state_cd, city = res_city_desc, zip = zip_code))

saveRDS(actives, "temp/nc_geo.rds")
