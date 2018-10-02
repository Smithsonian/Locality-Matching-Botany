library(RSQLite)

#Table with known locations from EMu
location_database <- read.csv("data/CollCountryLocDate_Unique.csv", header = TRUE, encoding = "UTF-8")
location_database$ID <- seq.int(nrow(location_database))

#cleanup strings
location_database$location <- stringr::str_replace_all(location_database$location, "\n", "")
location_database$location <- stringr::str_replace_all(location_database$location, "\t", "")

loc1 <- dbConnect(RSQLite::SQLite(), "data/nmnh.sqlite")

n <- dbExecute(loc1, "CREATE TABLE locations(ID INTEGER PRIMARY KEY, collector, year INTEGER, country, location, location_count INTEGER)")
n <- dbExecute(loc1, 'CREATE INDEX loc_id ON locations(ID)')
n <- dbExecute(loc1, 'CREATE INDEX partstr ON locations(collector)')
n <- dbExecute(loc1, 'CREATE INDEX colyear ON locations(year)')
n <- dbExecute(loc1, 'CREATE INDEX loccountry ON locations(country)')
n <- dbExecute(loc1, 'CREATE INDEX precloc ON locations(location)')
n <- dbExecute(loc1, 'CREATE INDEX loccount ON locations(location_count)')




dbWriteTable(loc1, "locations", location_database, append = TRUE)

dbDisconnect(loc1)
