###################################
### THE DATA INCUBATOR DEMO PROJECT
### Gabriel Walker
### May 2019
###################################

library(tidyverse)
library(janitor)

#################
### DATA CLEANING
#################

### 
# Read in the climate and trade data, obtained from:
# https://gain.nd.edu/our-work/country-index/download-data/
# https://www.kaggle.com/unitednations/global-commodity-trade-statistics/
###

setwd("C:/Users/Gabriel/Desktop")

# First read in the vulnerability and readiness metadata.

meta_v <- read_csv("climate_vulnerability/metadata_vuln.csv") %>% 
  clean_names()
meta_r <- read_csv("climate_vulnerability/metadata_read.csv") %>% 
  clean_names()

###
# Read in all of the vulnerability indicators (11 files).
###

csv_v <- list.files("climate_vulnerability/vulnerability/", pattern = "*.csv", recursive = TRUE)

setwd("C:/Users/Gabriel/Desktop/climate_vulnerability/vulnerability/")
raw_v <- lapply(csv_v, read_csv)

# Paste all of the vulnerability tibbles together with a labeled "vulnerability" column.

df_v <- tibble()

for (n in 1:length(csv_v)) {
  vulnerability <- str_remove(csv_v[n], ".csv")
  
  output <- as_tibble(raw_v[[n]]) %>% 
    gather(key = "year", value = vulnerability, `1995`:`2015`) %>% 
    clean_names()
    
  if (n == 1) df_v <- output
  
  else df_v <- left_join(df_v, output, by = "year")
}

###
# Read in all of the vulnerability indicators (11 files).
###





#################
### 
#################