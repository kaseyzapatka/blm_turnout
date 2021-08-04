### THIS SCRIPT TAKES THE RAW L2 VOTER FILES AND DUMPS THEM IN AN SQL DATABASE FOR EASIER QUERYING

### start by connecting to an (empty, at first) SQL database where we'll dump the voter files
db <- dbConnect(SQLite(), "D:/national_file_post20.db")

## grab file names
j <- list.files("E:/national/post_2020", full.names = T,
                pattern = "*.tab", include.dirs = T, recursive = T)

## keep only demographic files, not history files
files <- j[grepl("DEMOGRAPHIC", j)]

tabs <- dbListTables(db)

for(f in files){
  s <- substring(f, 28, 29)
  print(s)
  
  if(!(s %in% c(tabs, "CA"))){ ## need to do CA separately since its so big
    k <- fread(f, sep = "\t",
               select = c("LALVOTERID", "Voters_Active", ## these are the variables we want
                              "Residence_Addresses_CensusTract",
                              "Residence_Addresses_CensusBlockGroup",
                              "Residence_Addresses_CensusBlock",
                              "Residence_Addresses_Latitude",
                              "Residence_Addresses_Longitude",
                              "Voters_Gender", "Voters_Age", "Parties_Description",
                              "EthnicGroups_EthnicGroup1Desc",
                              "Voters_OfficialRegDate", "US_Congressional_District",
                              "Voters_FIPS",
                              "CommercialData_EstimatedHHIncome",
                              "CommercialData_Education")) %>% 
      mutate(state = s)
    
    
    RSQLite::dbWriteTable(db, name = s, value = k, append = F, overwrite = T)  ## write to db
  }
}
#####

heads <- vroom("D:/national/post_2020/VM2--CA--2021-02-19/VM2--CA--2021-02-19-DEMOGRAPHIC_0.csv",
               n_max = 1)
h2 <- c(1:ncol(heads))
h3 <- c("LALVOTERID", "Voters_Active", ## these are the variables we want
        "Residence_Addresses_CensusTract",
        "Residence_Addresses_CensusBlockGroup",
        "Residence_Addresses_CensusBlock",
        "Residence_Addresses_Latitude",
        "Residence_Addresses_Longitude",
        "Voters_Gender", "Voters_Age", "Parties_Description",
        "EthnicGroups_EthnicGroup1Desc",
        "Voters_OfficialRegDate", "US_Congressional_District",
        "Voters_FIPS",
        "CommercialData_EstimatedHHIncome",
        "CommercialData_Education")
heads <- h2[colnames(heads) %in% h3]

c1 <- fread("D:/national/post_2020/VM2--CA--2021-02-19/VM2--CA--2021-02-19-DEMOGRAPHIC_0.csv",
            sep = "\t",
            select = h3)

c2 <- fread("D:/national/post_2020/VM2--CA--2021-02-19/VM2--CA--2021-02-19-DEMOGRAPHIC_1.csv",
            sep = "\t", header = F,
            select = heads)
colnames(c2) <- colnames(c1)

c3 <- fread("D:/national/post_2020/VM2--CA--2021-02-19/VM2--CA--2021-02-19-DEMOGRAPHIC_2.csv",
            sep = "\t", header = F,
            select = heads)
colnames(c3) <- colnames(c1)

ca <- unique(bind_rows(c1, c2, c3)) %>% 
  mutate(state = "CA")

RSQLite::dbWriteTable(db, name = "CA", value = ca, append = F, overwrite = T)  ## write to db
