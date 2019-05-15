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

# First read in the vulnerability and readiness metadata.

meta_v <- read_csv("climate_vulnerability/metadata_vuln.csv") %>% 
  clean_names()
meta_r <- read_csv("climate_vulnerability/metadata_read.csv") %>% 
  clean_names()

###
# Read in all of the vulnerability indicators, gain, and readiness scores.
###

csv_v <- list.files("climate_vulnerability/vulnerability/", pattern = "*.csv", recursive = TRUE)
csv_g <- list.files("climate_vulnerability/gain/", pattern = "*.csv", recursive = TRUE)
csv_r <- list.files("climate_vulnerability/readiness/", pattern = "*.csv", recursive = TRUE)

setwd("C:/Users/Gabriel/Desktop/climate_vulnerability/vulnerability/")
raw_v <- lapply(csv_v, read_csv)
setwd("C:/Users/Gabriel/Desktop/climate_vulnerability/gain/")
raw_g <- lapply(csv_g, read_csv)
setwd("C:/Users/Gabriel/Desktop/climate_vulnerability/readiness/")
raw_r <- lapply(csv_r, read_csv)

# Combine the CSV names and files.

csv <- c(csv_v, csv_g, csv_r)
raw <- c(raw_v, raw_g, raw_r)

# Paste all of the data together.

df <- tibble()

for (n in 1:length(raw)) {
  
  output <- as_tibble(raw[[n]]) %>% 
    gather(key = "year", value = x, `1995`:`2015`) %>% 
    clean_names()
  
  colnames(output) <- c("iso3", "name", "year", str_remove(csv[n], ".csv"))
    
  if (n == 1) {
    df <- output
  }
  
  else {
    output <- output %>% 
      select(-iso3, -name, -year)
    
    df <- cbind(df, output) %>% 
      as_tibble()
  }
}

###
# Read in all of the indicators.
###

setwd("climate_vulnerability/indicators")
csv_i <- list.files(pattern = "*score.csv", recursive = TRUE)

raw_i <- lapply(csv_i, read_csv)

# Paste all of the indicator data together.

df_i <- tibble()

for (n in 1:length(raw_i)) {
  
  output <- as_tibble(raw_i[[n]]) %>% 
    gather(key = "year", value = indicator, `1995`:`2015`) %>% 
    clean_names()
  
  colnames(output) <- c("iso3", "name", "year", str_remove(csv_i[n], "/score.csv"))
  
  if (n == 1) {
    df_i <- output
  }
  
  else {
    output <- output %>% 
      select(-iso3, -name, -year)
    
    df_i <- cbind(df_i, output) %>% 
      as_tibble()
  }
}

# Clean the variable names of the indicators.

colnames(df_i) <- c("iso3", "name", "year", "gdp", "hdi",
                    "doing_business", "biome_distribution", "marine", "natcap", "eco_footprint",
                    "biome_protected", "intnl_environmental", "cereal", "pop_change",
                    "food_import", "rural", "ag_capacity", "child_malnutrition", "political_stability",
                    "corruption_control", "regulatory_quality", "rule_of_law", "warm",
                    "flood", "urban", "age_ratio", "trade_quality", "roads", "deaths", "vector_disease",
                    "health_external", "slum", "medical_staff", "sanitation", "hydro", "sea_level",
                    "energy_imports", "sea_pop", "electricity", "disaster_prep", "inequality", "ict",
                    "education", "innovation", "runoff", "groundwater", "fresh_water", "water_dependency",
                    "dam_cap", "potable", "pop")

# Combine the two datasets for a full tidy collection.

df_i <- df_i %>% 
  select(-iso3, -name, -year)

df <- cbind(df, df_i) %>% 
  as_tibble()

# Save the cleaned data.

write_rds(df, "df.rds")

#################
### 
#################