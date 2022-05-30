#############################################
## Maintaned by Jon Bråte - jon.brate@fhi.no
## 
## This script takes a list of desensitized
## fastq files, with matching GISAID id's, 
## and creates a submission sheet to send to 
## Elixir Norway. 
##
#############################################

pacman::p_load(tidyverse, filesstrings)

# Set originating labs and adresses ---------------------------------------
lab_lookup_table <- tribble(
  ~`Lab code`, ~`Lab`, ~`Lab address`,
  0,	"Norwegian Institute of Public Health, Department of Virology",	"P.O.Box 222 Skoyen, 0213 Oslo, Norway",
  1,	"Ostfold Hospital Trust - Kalnes, Centre for Laboratory Medicine, Section for gene technology and infection serology", "P.O.Box 300, N-1714 Graalum, Norway",
  2,	"Akershus University Hospital, Department for Microbiology and Infectious Disease Control",	"P.O.Box 1000, N-1478 Loerenskog, Norway",
  3,	"Oslo University Hospital, Department of Microbiology",	"P.O.Box 4956 Nydalen, N-0424 Oslo, Norway",
  4,	"Furst Medical Laboratory",	"Soeren Bulls vei 25, N-1051 Oslo, Norway",
  5,	"Innlandet Hospital Trust, Division Lillehammer, Department for Medical Microbiology", "P.O.Box 990, N-2629 Lillehammer, Norway",
  6,	"Medical Microbiology Unit, Department for Laboratory Medicine, Drammen Hospital, Vestre Viken Health Trust", "P.O.Box 800, N-3004 Drammen, Norway",
  7,	"Vestfold Hospital, Toensberg, Department of Microbiology",	"P.O.Box 2168, N-3103 Toensberg, Norway",
  8,	"Unilabs Laboratory Medicine", "Leirvollen 19, N-3736 Skien, Norway",
  9, NA, NA,
  10,	"Hospital of Southern Norway - Kristiansand, Department of Medical Microbiology",	"P.O.Box 416 Lundsiden, N-4604 Kristiansand, Norway",
  11,	"Dept. of Medical Microbiology, Stavanger University Hospital, Helse Stavanger HF", "P.O.Box 8100, N-4068 Stavanger, Norway",
  12,	"Haukeland University Hospital, Dept. of Microbiology",	"P.O.Box 1400, N-5021 Bergen, Norway",
  13,	"Haugesund Hospital, laboratory for Medical Microbiology", "P.O.Box 2170, N-5504 Haugesund, Norway",
  14,	"Foerde Hospital, Department of Microbiology", "P.O.Box 1000, N-6807 Foerde, Norway",
  15,	"Department of Medical Microbiology - section Molde, Molde Hospital",	"Parkveien 84, N-6407 Molde, Norway",
  16,	"Department of Medical Microbiology, St. Olavs hospital",	"P.O.box 3250 Torgarden, N-7006 Trondheim, Norway",
  17,	"Levanger Hospital, laboratory for Medical Microbiology", "P.O.box 333, N-7601 Levanger, Norway",
  18,	"Nordland Hospital - Bodo, Laboratory Department, Molecular Biology Unit", "P.O.Box 1480, N-8092 Bodo, Norway",
  19,	"University Hospital of Northern Norway, Department for Microbiology and Infectious Disease Control",	"P.O.Box 56, N-9038 Tromsoe, Norway",
  20, NA, NA,
  21,	NA, NA,
  22,	"Department of medical microbiology, section Aalesund, Aalesund Hospital", "N-6026 Aalesund, Norway",
  23,	NA, NA,
  24, "Department Medical Microbiology, Baerum Hospital, Vestre Viken Health Trust", "P.O.Box 800, N-3004 Drammen, Norway",
  25,	"Telemark Hospital Trust – Skien, Dept. of Medical Microbiology",	"P.O.Box 2900 Kjørbekk, N-3710 Skien",
  26,	"Unilabs Laboratory Medicine", "Silurveien 2 B, N-0380 Oslo, Norway",
  27,	"Oslo Helse", "Hegdehaugsveien 36, 0352 Oslo"
) %>%
  mutate(`Lab code` = as.character(`Lab code`))

# Define lookup function to decide originating lab and address ------------
lookup_function <- function(metadata) {
  for (row in seq_along(metadata$INNSENDER)) {
    if (metadata[row,]$INNSENDER == 0){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[1,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[1,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 1){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[2,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[2,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 2){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[3,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[3,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 3){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[4,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[4,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 4){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[5,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[5,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 5){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[6,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[6,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 6){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[7,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[7,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 7){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[8,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[8,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 8){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[9,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[9,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 9){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[10,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[10,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 10){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[11,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[11,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 11){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[12,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[12,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 12){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[13,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[13,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 13){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[14,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[14,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 14){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[15,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[15,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 15){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[16,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[16,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 16){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[17,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[17,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 17){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[18,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[18,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 18){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[19,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[19,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 19){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[20,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[20,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 20){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[21,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[21,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 21){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[22,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[22,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 22){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[23,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[23,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 23){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[24,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[24,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 24){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[25,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[25,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 25){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[26,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[26,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 26){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[27,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[27,]$`Lab address`
    } else if (metadata[row,]$INNSENDER == 27){
      metadata[row,]$covv_orig_lab <- lab_lookup_table[28,]$Lab
      metadata[row,]$covv_orig_lab_addr <- lab_lookup_table[28,]$`Lab address`
    }
  }
  return(metadata)
}


# Read BioNumerics --------------------------------------------------------
# Remember to refresh BN first
load(file = "N:/Virologi/JonBrate/Prosjekter/BN.RData")

# Convert empty strings to NA
BN <- BN %>% mutate_all(list(~na_if(.,""))) %>% 
  # Convert NA as strings to true NA
  mutate_all(list(~na_if(.,"NA"))) %>% 
  # Remove already submitted samples
  filter(is.na(ENA_RUN)) %>% 
  # Keep only samples submitted to Gisaid
  filter(str_detect(GISAID_EPI_ISL, "^EPI"))

  # Swift_MIK ---------------------------------------------------------------

# Create list of desensitized file names
# The directory "desensitized-files/desensitized-not-ready-for-Elixir/" contains
# the desensitized files exported from TSD
desensitized_files <- 
  as_tibble(list.files(path = "Swift_MIK_desensitized-files/desensitized-not-ready-for-Elixir/",
                       pattern = "fastq.gz")) 

# Get R1 and R2 on the same rows
desensitized_files_paired <- desensitized_files %>%
  # Add column "pair" with "R1" and "R2" on every second row
  mutate(pair = rep(c("R1_desensitized", "R2_desensitized"), 
                    nrow(desensitized_files) / 2),
         # Add a unique number to each sample (every two rows). 
         # This will be used for spreading later
         key = rep(1:(nrow(desensitized_files) / 2), each = 2)) %>% 
  # Spread using the unique numbers as IDs
  pivot_wider(id_cols = key, names_from = pair, values_from = value) %>% 
  # Remove the unique numbers
  select(-key) %>% 
  # Remove "desensitized" to create original filenames
  mutate("R1" = str_remove(R1_desensitized, "\\.desensitized")) %>% 
  mutate("R2" = str_remove(R2_desensitized, "\\.desensitized"))

# Add BN "SEQUENCEID_SWIFT" to match with metadata files
desensitized_files_paired_seqid <- desensitized_files_paired %>% 
  add_column("tmp" = "OUS-") %>% 
  # Extract the SEQUENCEID from the filenames
  separate(R1_desensitized, into = c("tmp2"), sep = "_", remove = FALSE) %>% 
  # Create the final SEQUENCEID
  unite("SEQUENCEID_SWIFT", c(tmp, tmp2), sep = "", remove = T)

# Match with BN
desensitized_files_paired_joined <- left_join(
  desensitized_files_paired_seqid,
  BN,
  by = "SEQUENCEID_SWIFT") %>% 
  # Drop samples without Gisaid Accession (because we clean more samples than should be submitted)
  filter(str_detect(GISAID_EPI_ISL, "^EPI"))

# Create Sample Alias and Isolate for ENA
desensitized_files_paired_joined_alias <- desensitized_files_paired_joined %>% 
  add_column("Prefix" = "SARS-CoV-2/human/Norway/",
             "version" = "/1") %>%
  # Trekke ut sifrene fra 5 og til det siste fra BN KEY
  mutate("unique" = str_sub(KEY, start = 1, end = -1)) %>%
  # add year
  mutate("year" = paste0("/", str_sub(PROVE_TATT, 1, 4))) %>% 
  unite("Sample Alias", c(Prefix, unique, year, version), sep = "", remove = F) %>% 
  unite("Isolate", c(Prefix, unique, year), sep = "", remove = T) %>% 
  add_column("covv_orig_lab" = NA,
             "covv_orig_lab_addr" = NA) %>% 
  # Endre Trøndelag til Trondelag
  mutate("FYLKENAVN" = str_replace(FYLKENAVN, "Tr\xf8ndelag", "Trondelag")) %>%
  # Endre Møre og Romsdal
  mutate("FYLKENAVN" = str_replace(FYLKENAVN, "M\xf8re", "More")) %>%
  # Endre Sør
  mutate("FYLKENAVN" = str_replace(FYLKENAVN, "S\xf8r", "Sor"))

# Then create the rest of the needed columns
# Lage den endelige rekkefølgen på kolonnene (Sample Alias først) og evt ta bort noen vi ikke trenger
tmp <- desensitized_files_paired_joined_alias %>% 
  add_column("Folder" = "Unknown",
             "Submitter" = "jonbra",
             "Type" = "betacoronavirus",
             "World region" = "Europe",
             "Country" = "Norway",
             "Host" = "Human",
             "Specimen source" = "Unknown",
             "Sequencing technology" = "Illumina Swift Amplicon SARS-CoV-2 protocol at Norwegian Sequencing Centre",
             "Assembly method" = "Assembly by reference based mapping using Bowtie2 with iVar majority rules consensus",
             "Authors" = "Mona Holberg-Petersen, Lise Andresen, Cathrine Fladeby, Mariann Nilsen, Teodora Plamenova Ribarska, Pål Marius Bjørnstad, Gregor D. Gilfillan, Arvind Yegambaram Meenakshi Sundaram, Kathrine Stene-Johansen, Kamilla Heddeland Instefjord, Hilde Elshaug, Garcia Llorente Ignacio, Jon Bråte, Pedersen Benedikte Nevjen, Line Victoria Moen, Rasmus Riis Kopperud, Hilde Vollan, Olav Hungnes, Karoline Bragstad",
             "INSERT_SIZE" = 500,
             "LIBRARY_SELECTION" = "PCR",
             "LIBRARY_SOURCE" = "VIRAL RNA",
             "LIBRARY_STRATEGY" = "AMPLICON",
             "INSTRUMENT" = "Illumina NovaSeq 6000",
             "Submitting lab" = "Norwegian Institute of Public Health, Department of Virology",
             "Submitting lab address" = "P.O.Box 222 Skoyen, 0213 Oslo, Norway",
             "Originating lab" = "Oslo University Hospital, Department of Microbiology",
             "Originating lab address" = "P.O.Box 4956 Nydalen, N-0424 Oslo, Norway")

df_final <- tmp %>% 
  select(`Sample Alias`, 
         R1_desensitized, 
         R2_desensitized, 
         R1, 
         R2, 
         KEY, 
         "Plate" = SEKV_OPPSETT_SWIFT7,
         Folder,
         GISAID_EPI_ISL,
         Isolate,
         Submitter,
         Type,
         "Collection date" = PROVE_TATT,
         `World region`,
         Country,
         "County" = FYLKENAVN,
         Host,
         `Specimen source`,
         `Sequencing technology`,
         `Assembly method`,
         "Originating lab" = covv_orig_lab,
         "Originating lab address" = covv_orig_lab_addr,
         `Submitting lab`,
         `Submitting lab address`,
         Authors,
         INSERT_SIZE,
         LIBRARY_SELECTION,
         LIBRARY_SOURCE,
         LIBRARY_STRATEGY,
         INSTRUMENT) %>% 
  drop_na(County)

# Write file
write_tsv(df_final, file = "Submitted_files/2022.04.04-Swift_MIK_metadatafile_for_Elixir.tsv")

# Move the files 
library(filesstrings)

# Get only the file names
desensitized_names <- c(df_final$R1_desensitized, df_final$R2_desensitized)

# Change file paths:
file.move(paste0("Swift_MIK_desensitized-files/desensitized-not-ready-for-Elixir/", desensitized_names), "Swift_MIK_desensitized-files/ready_for_Elixir/2022.04.04/")
