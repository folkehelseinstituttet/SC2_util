#############################################
## Maintaned by Jon Bråte - jon.brate@fhi.no
## 
## This script checks which Illumina plates 
## of MIK SARS-CoV-2 samples have not been 
## submitted to ENA.
##
#############################################

# Load packages
pacman::p_load(tidyverse, readxl, stringr)

# Read data from BioNumerics ----------------------------------------------
# Remember to run the script refresh_data_from_BN.R first
try(load(file = "/mnt/N/Virologi/JonBrate/Prosjekter/BN.RData"))
try(load(file = "N:/Virologi/JonBrate/Prosjekter/BN.RData"))

# Filter BN and display available plates
BN %>% 
  # Convert empty strings to NA
  mutate_all(list(~na_if(.,""))) %>% 
  # Select relevant columns
  select(KEY, GISAID_EPI_ISL, PROVE_TATT, SEKV_OPPSETT_SWIFT7, SEQUENCEID_SWIFT, ENA_RUN) %>% 
  # Keep only samples with Gisaid ID
  filter(str_detect(GISAID_EPI_ISL, "^EPI")) %>% 
  # Skip samples already submitted to ENA
  filter(is.na(ENA_RUN)) %>% 
  # Keep only NSC_MIK data
  filter(str_detect(SEKV_OPPSETT_SWIFT7, "^MIK")) %>% 
  # Get the different plates
  select(SEKV_OPPSETT_SWIFT7) %>%
  distinct() %>%
  View("Swift_MIK")

# Then check the description in the AR document for ENA submission