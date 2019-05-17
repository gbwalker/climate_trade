###################################
### THE DATA INCUBATOR DEMO PROJECT
### Gabriel Walker
### May 2019
###################################

library(tidyverse)
library(janitor)
library(stringr)
library(pls)

#################
### DATA CLEANING
#################

### 
# Read in the climate data obtained from:
# https://gain.nd.edu/our-work/country-index/download-data/
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
# df <- read_rds("df.rds")

### 
# Read in the trade data, obtained from:
# https://www.kaggle.com/unitednations/global-commodity-trade-statistics/
###

setwd("C:/Users/Gabriel/Desktop")
trade_raw <- read_csv("trade/commodity_data.csv")

# Use only countries and areas that have climate data.

trade <- trade_raw %>%
  
  # Pick only the BRICS countries to use for the demo.
  
  filter(country_or_area %in% c("Brazil", "Russian Federation", "India", "China",
                                "South Africa", "USA")) %>% 
  
  # Get the first word of the place name.
  
  mutate(short_name = str_extract(country_or_area, "\\w+")) %>%
  
  # Make a column for iso3.
  
  mutate(iso3 = case_when(
    country_or_area == "Brazil" ~ "BRA",
    country_or_area == "Russian Federation" ~ "RUS",
    country_or_area == "India" ~ "IND",
    country_or_area == "China" ~ "CHN",
    country_or_area == "South Africa" ~ "ZAF",
    country_or_area == "USA" ~ "USA",
  )) %>% 
  
  # Adjust the country name of the US to match between datasets.
  
  mutate(country = case_when(
    country_or_area == "USA" ~ "United States",
    TRUE ~ country_or_area
  )) %>% 
  select(-country_or_area) %>% 

  # Fix the appearance of the trade category variable.

  mutate(category = str_remove(category, "\\d\\d_")) %>% 
  mutate(category = str_replace_all(category, "_", " "))

# trade <- read_rds("trade.rds")

# Select only the countries of interest in the climate data.

df_small <- df %>% 
  filter(name %in% unique(trade$country))

###########
### PREPARE
###########

### Prepare the data for the PLS regression.

# Just use the United States as a demo.

df_us <- df_small %>% 
  filter(name == "United States") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-iso3, -name)

# Merge the two datasets.

trade_us <- trade %>% 
  filter(country == "United States" & category == "all commodities") %>% 
  filter(year < 2016 & year >= 1995) %>% 
  
  # Focus only on exports.
  
  filter(flow == "Export") %>% 
  left_join(df_us, by = "year") %>% 
  
  # Drop non-numeric data.
  
  select(-comm_code, -commodity, -flow, -weight_kg, -quantity_name, -quantity, -category, -short_name, -iso3, -country, -health_external, -year)

# Add a t-1 predictor to emulate a time series.

trade_us <- trade_us %>% 
  mutate(tm1 = c(trade_us$trade_usd[2:nrow(trade_us)], NA))

# Split into training and test data.

training <- trade_us[2:nrow(trade_us), ]
testing <- trade_us[1, ]

###################
### FUTURE SCENARIO
###################

# Generate a future scenario for 2025 based on user input.

# First generate the business as usual scenario as a linear estimation of the past five years.

recent <- trade_us[1:5,2:67] %>% 
  mutate(year = c(2015:2011))

# Predict the result.

# Save a resulting vector.

future <- c()

for (var in names(recent)) {
  
  m <- lm(get(var) ~ year, data = recent)
  future <- c(future, predict(m, newdata = tibble(year = 2025)))
}

#########
### MODEL
#########


# Fit the PLS model.

mod_pls <- plsr(trade_usd ~ .,
                10,
                data = training,
                validation = "LOO")

# plot(RMSEP(mod_pls))

# Find the optimal number of components.

n <- selectNcomp(mod_pls, method = "onesigma", plot = TRUE)

# Predict the new trade amount.

for (y in 1:5) {
  print(predict(mod_pls, ncomp = y, newdata = testing))
}
  
RMSEP(mod_pls, newdata = testing)
