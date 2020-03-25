library(DBI)
library(RMySQL)
library(dbplyr)
library(dplyr)

db_pswd <- Sys.getenv("db_pass")
db_usr <- "eniak"
db_hostname <- Sys.getenv("ip_address")
db_port <- as.numeric(Sys.getenv("db_port"))
db_name <- Sys.getenv("db_name")

db_con <- dbConnect(MySQL(),
          user = db_usr,
          password = db_pswd,
          dbhost = db_hostname,
          dbname = db_name
)

#dbCreateTable(db_con, "mtcars_test", mtcars)
#dbWriteTable(db_con, "mtcars_test_1", mtcars)

tabla <- db_con %>% 
  tbl(in_schema("db_name", "mtcars_test"))

RMySQL::dbDisconnect(db_con)
  
