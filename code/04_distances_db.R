

## connect to voter file sql database
db <- dbConnect(SQLite(), "D:/national_file_post20.db")
db_final <- dbConnect(SQLite(), "D:/national_file_post20_blm.db")
tabs <- dbListTables(db_final)

for(s in dbListTables(db)){
  print(s)
  code_good <- unique(filter(fips_codes, state == s)$state_code)
  if(!(s %in% tabs)){
    ## read in protest locations
    protests <- read_xlsx("raw_data/protests/USA_2020_Nov14.xlsx") %>%
      filter(EVENT_DATE > "2020-05-25",
             EVENT_DATE <= "2020-06-07")
    
    ## pull in voter info
    voters <- dbGetQuery(db, paste0("select * from [", s, "]")) %>% 
      rename(latitude = Residence_Addresses_Latitude,
             longitude = Residence_Addresses_Longitude) %>% 
      filter(!is.na(latitude), !is.na(longitude)) %>% 
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>% 
      select(-Voters_FIPS, -Residence_Addresses_CensusTract,
             -Residence_Addresses_CensusBlockGroup, -starts_with("Commercial"),
             -Voters_Active, -Voters_OfficialRegDate, -Residence_Addresses_CensusBlock)
    
    ## turn voter lats / longs into spatial object
    vps <- SpatialPoints(
      voters %>%
        select(x = longitude, y = latitude)
    )
    
    ####################
    
    ## turn protest locs into spatial object
    protest_sites <- SpatialPoints(
      select(protests, x = LONGITUDE, y = LATITUDE)
    )
    
    
    tree <- createTree(coordinates(protest_sites))
    
    ## find 5 closest protests to each voter
    inds <- knnLookup(tree, newdat = coordinates(vps), k = 5)
    
    j <- lapply(c(1:5), function(i){
      
      actual <- inds[,i]
      
      v1 <- cbind(voters, actual) %>%
        select(LALVOTERID, latitude, longitude, actual)
      
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
                     av_5 = (dist1 + dist2 + dist3 + dist4 + dist5) / 5) %>% # create average distance to 5 closest
      select(-dist2, -dist3, -dist4, -dist5) %>%
      rename(dist = dist1) ## keep closest protest
    
    cleanup(c("voters", "vps", "s", "tabs", "db_final"))
    
    ##########################
    
    ## read in rainfall data (from 01_grab_rainfall.R)
    rainfall <- readRDS("temp/rainfall_processed_2000_2020.rds") %>%
      ungroup() %>%
      mutate(id = row_number(),
             lon = ifelse(lon > 180, -360 + lon, lon))
    
    ## turn weather sites into spatial object
    weather_sites <- SpatialPoints(
      select(rainfall, x = lon, y = lat)
    )
    
    ## find closest weather station to each voter
    tree <- createTree(coordinates(weather_sites))
    inds <- knnLookup(tree, newdat = coordinates(vps), k = 1)
    
    
    voters <- left_join(cbind(voters, inds),
                        rainfall,
                        by = c("inds" = "id")) %>%
      mutate(rel = precip_2020 / precip_historical) %>% ## divide 2020 rainfall by historical rainfall
      select(-inds, -lat, -lon)
    
    # RSQLite::dbWriteTable(db, name = s, value = k, append = F, overwrite = T)  ## write to db. overwrite.
    
    historydb <- dbConnect(SQLite(), "D:/national_file_post20_history.db")
    
    hist <- dbGetQuery(historydb, paste0("select * from [", s, "_history_20]")) %>% 
      mutate_at(vars(starts_with("Gen")), ~ifelse(. == "", 0, 1))
    
    voters <- left_join(voters, hist, by = c("state", "LALVOTERID"))
    
    voters <- voters %>% 
      mutate(male = Voters_Gender == "M",
             age = Voters_Age,
             dem = Parties_Description == "Democratic",
             white = EthnicGroups_EthnicGroup1Desc == "European",
             black = EthnicGroups_EthnicGroup1Desc == "Likely African-American") %>% 
      select(-Voters_Gender, -Parties_Description,
             -EthnicGroups_EthnicGroup1Desc)
    
    dens <- pop_dens("block group", year = 2019, state = s)
    census <- readRDS("../regular_data/census_bgs_19.rds")
    census <- left_join(census, dens) %>% 
      select(GEOID, pop_dens, median_income, some_college)
    
    voters <- left_join(voters, census) %>% 
      select(-latitude, -longitude, -Voters_Age)
    
    dbWriteTable(db_final, value = voters, name = s, overwrite = T, append = F) 
  }
}