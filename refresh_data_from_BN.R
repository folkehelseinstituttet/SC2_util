#############################################
## Maintaned by Jon Bråte - jon.brate@fhi.no
## 
## This script will fetch the latest data
## from the BioNumerics database containing
## Sars-CoV-2 sequencing results. 
##
## NB: This script will not retrieve all info
## on Spike mutations.
## NB: This script can only be run on a
## FHI Windows laptop.
#############################################

pacman::p_load(tidyverse, readxl, stringr, lubridate, odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "sql-bn-covid19",
                 Database = "BN_Covid19")

BN <- tbl(con, "ENTRYTABLE") %>% 
  rename("Dekning_Artic" = PROSENTDEKNING_GENOM,
         "Dekning_Swift" = COVERAGE_BREADTH_SWIFT,
         "Dekning_Nano" = DEKNING_NANOPORE,
         "Dekning_Eksterne" = COVERAGE_BREATH_EKSTERNE) %>% 
  collect()

save(BN, file = "N:/Virologi/JonBrate/Prosjekter/BN.RData")
