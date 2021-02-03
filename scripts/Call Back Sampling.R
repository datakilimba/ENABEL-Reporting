library(tidyverse)
library(xlsx)
library(magrittr)
library(RPostgres)

con = DBI::dbConnect(odbc::odbc(),"PostgreSAKiRP")
waeo = dbReadTable(con,"waeos") %>% 
  filter(active == 1)
district = dbReadTable(con, "districts")
ward = dbReadTable(con, "wards")