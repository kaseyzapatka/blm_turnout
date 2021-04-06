### THIS SCRIPT TAKES THE RAW L2 VOTER FILES AND DUMPS THEM IN AN SQL DATABASE FOR EASIER QUERYING


### start by connecting to an (empty, at first) SQL database where we'll dump the voter files
db <- dbConnect(SQLite(), "D:/national_file_post20_history.db")

##grab all the file names
j <- list.files("D:/national/post_2020", full.names = T,
                pattern = "*.tab", include.dirs = T, recursive = T)

## drop the demographic files - only interested in history here
files <- j[grepl("HISTORY", j)]

## make list of tables already in database
tabs <- dbListTables(db)


for(f in files){
  s <- substring(f, 28, 29) ## take state 2 letter abbr from file name
  print(s)
  
  if(!(paste0(s, "_history_20") %in% tabs)){ ## only do this loop if table not already in db
    k <- fread(f, sep = "\t",
                      select = c("LALVOTERID", ## these are the only elections we care about for now
                      "General_2020_11_03",
                      "General_2018_11_06",
                      "General_2016_11_08",
                      "General_2014_11_04",
                      "General_2012_11_06",
                      "General_2010_11_02")) %>%  
      mutate(state = s) ## generate another column for state
    RSQLite::dbWriteTable(db, name = paste0(s, "_history_20"), value = k, append = F, overwrite = T)  #write table to db
  }
}
