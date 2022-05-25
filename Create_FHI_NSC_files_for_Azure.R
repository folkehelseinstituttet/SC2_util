#############################################
## Maintaned by Jon Bråte - jon.brate@fhi.no
## 
## This script creates input files for 
## cleaning FHI NSC Illumina samples on
## Azure. Run the script 
## Find_FHI_plates_for_ENA_submission.R first.
##
## Example usage:
## Rscript Create_FHI_NSC_files_for_Azure.R FHI415
#############################################

# Load packages
pacman::p_load(tidyverse, readxl, stringr)

# Read data from BioNumerics ----------------------------------------------
try(load(file = "/mnt/N/Virologi/JonBrate/Prosjekter/BN.RData"))

# List relevant folders to search through
# Search the N: disk for consensus sequences
dirs_fhi <- c(list.dirs("/mnt/N/Virologi/NGS/1-NGS-Analyser/1-Rutine/2-Resultater/SARS-CoV-2/1-Illumina_NSC_FHI/2021/rawfastq/", recursive = FALSE),
              list.dirs("/mnt/N/Virologi/NGS/1-NGS-Analyser/1-Rutine/2-Resultater/SARS-CoV-2/1-Illumina_NSC_FHI/2022/rawfastq/", recursive = FALSE))

args = commandArgs(trailingOnly=TRUE)
plate <- args[1]

# Pick out relevant folder
dir <- dirs_fhi[grep(paste0(plate, "\\b"), dirs_fhi)]
  
# Select only IDs for the relevant plate
IDs_per_plate <- BN %>% 
  filter(SEKV_OPPSETT_SWIFT7 == plate)
  
# Search the N: disk for for rawfastq filenames
filepaths <- list.files(dir, 
                        full.names = TRUE, 
                        recursive = TRUE, 
                        pattern = "\\.gz$")

# Match with SEQUENCE IDs from BN
filepaths_to_keep <- 
  as_tibble(filepaths[which((gsub("_.*","", basename(filepaths))) %in% IDs_per_plate$SEQUENCEID_SWIFT)])
  
# Get R1 and R2 on the same rows
filepaths_to_keep_df <- filepaths_to_keep %>% 
  # Add column "pair" with "R1" and "R2" on every second row
  mutate(pair = rep(c("R1", "R2"), nrow(filepaths_to_keep) / 2),
         # Add a unique number to each sample (every two rows). 
         # To be used for spreading later
         key = rep(1:(nrow(filepaths_to_keep) / 2), each = 2)) %>%
  # Spread using the unique numbers as IDs
  pivot_wider(id_cols = key, names_from = pair, values_from = value) %>% 
  # Remove the unique numbers
  select(-key) %>% 
  # Create "sample"
  mutate(sample = str_remove(basename(R1), "_R._001\\.fastq\\.gz")) %>% 
  # Create "fastq_1" and "fastq_2" with mount filepaths
  mutate("fastq_1" = str_c("/hera-fileshare/input/", basename(R1), sep = ""),
         "fastq_2" = str_c("/hera-fileshare/input/", basename(R2), sep = "")) %>% 
  # Make the final column selection
  select(sample,
         fastq_1,
         fastq_2)
  
# Sjekke at riktig R1 og R2 er paret
for_test <- filepaths_to_keep_df %>% 
  mutate(r1 = str_remove(basename(fastq_1), "_R._001\\.fastq\\.gz"),
         r2 = str_remove(basename(fastq_2), "_R._001\\.fastq\\.gz")) %>% 
  select(r1, r2)

# Make one samplesheet for each file pair (line). 
# The files are named according to Plate number and a number for each line in the filepaths list

# First clean up old files, if any. 
for (file in list.files("/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/", pattern = "csv$|tar$|txt$", full.names = TRUE)) {
  file.remove(file)
}

# Check that the files are correctly paired before writing
if (identical(for_test$r1, for_test$r2)) {
  for (i in seq_along(filepaths_to_keep_df$sample)) {
    # Write as csv
    outfile <- paste0("/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/",
                      format(Sys.time(), 
                             "%Y-%m-%d"), 
                      "-",
                      tolower(plate),
                      "-",
                      i,
                      "-samplesheet.csv")
    write_csv(filepaths_to_keep_df[i,], file = outfile)
  }
}

# Package these into a tarball
tarfiles <- list.files(path = "/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/",
                       pattern = "csv$",
                       full.names = TRUE)
tar(tarfile = "/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/samplesheets.tar",
    files = tarfiles)

# Remove the csv files
for (file in list.files("/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/", pattern = "csv$", full.names = TRUE)) {
  file.remove(file)
}

# Write the list of filepaths for moving from N to Azure
# Remove "/mnt/N/" from the filepaths
filepaths_to_keep <- filepaths_to_keep %>% 
  mutate(new_path = str_remove(value, "/mnt/N/"))

outfile_2 <- paste0("/mnt/N/Virologi/JonBrate/Prosjekter/Elixir-ENA_submission/Azure_samplesheets/",
                    format(Sys.time(), 
                           "%Y-%m-%d"), 
                    "_",
                    plate,
                    "_input_files.txt")
write_lines(filepaths_to_keep$new_path, file = outfile_2)
