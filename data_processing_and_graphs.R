##########################################
### THE DATA INCUBATOR PROPOSED PROJECT
### Gabriel Walker
### May 2019
###########################################

library(tidyverse)
library(janitor)

# Read in the climate vulnerability data.

setwd("C:/Users/Gabriel/Desktop")

raw_gain <- read_csv("climate_vulnerability/gain/gain.csv")
raw_gain_delta <- read_csv("climate_vulnerability/gain/gain_delta.csv")
df_gain_trend <- read_csv("climate_vulnerability/trends/gain.csv") %>% 
  clean_names()

# Tidy the data.

df_gain <- gather(raw_gain, key = "year", value = "gain", `1995`:`2015`)
df_gain_delta <- gather(raw_gain_delta, key = "year", value = "gain", `1995`:`2015`)

# Read in the trade data.

df_trade <- read_csv("trade/commodity_data.csv")

# Select only the past six years and commodities and countries of interest.
# Cuba, Libya, Sweden, and the US are all in the bottom ten downward-trending countries in terms of gain score.

cereals <- df_trade %>% 
  filter(year <= 2015 & year >= 2009) %>% 
  filter(country_or_area %in% c("Cuba", "Libya", "Sweden", "USA")) %>% 
  filter(category == "10_cereals")

# See trends for the US.

usa <- df_trade %>% 
  filter(year <= 2015 & year >= 1995) %>% 
  filter(country_or_area == "USA") %>% 
  filter(category == "all_commodities")

usa_gain <- 