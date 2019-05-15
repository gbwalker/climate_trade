##########################################
### THE DATA INCUBATOR PROPOSED PROJECT
### Gabriel Walker
### May 2019
###########################################

library(tidyverse)
library(janitor)
library(extrafont)

### Read in the climate and trade data, obtained from:
# https://gain.nd.edu/our-work/country-index/download-data/
# https://www.kaggle.com/unitednations/global-commodity-trade-statistics/

# Read in the climate vulnerability data.

raw_gain_delta <- read_csv("climate_vulnerability/gain/gain_delta.csv")
raw_gdp <- read_csv("climate_vulnerability/indicators/gdp/score.csv")

# Read in the six-year trend value.

df_gain_trend <- read_csv("climate_vulnerability/trends/gain.csv") %>%
  clean_names() %>%
  mutate(country_or_area = name) %>%
  select(-name)

# Tidy the data.

df_gain_delta <- gather(raw_gain_delta, key = "year", value = "gain", `1995`:`2015`) %>%
  clean_names()
df_gdp <- gather(raw_gdp, key = "year", value = "gdp", `1995`:`2015`) %>%
  clean_names()

# Read in the trade data.

setwd("C:/Users/Gabriel/Desktop")

df_trade <- read_csv("trade/commodity_data.csv")

### Make two introductory plots.
# First of the climate vulnerability of the top oil exporters, and
# second of US exposure.

# Select only 2016 to see the top crude oil exporters (in terms of amount).

df_trade %>%
  filter(year == 2016) %>%
  filter(flow == "Export") %>%
  filter(commodity == "Petroleum oils, oils from bituminous minerals, crude") %>%

  # Merge in the trend figures.

  left_join(df_gain_trend, by = "country_or_area") %>%
  filter(weight_kg > 200000000) %>%
  filter(!is.na(weight_kg)) %>%
  arrange(desc(weight_kg)) %>%
  mutate(sign = case_when(
    sign == -1 ~ "More vulnerable",
    sign == 1 ~ "Less vulnerable",
    TRUE ~ "Negligible change"
  )) %>%

  # Plot the top exporters.

  ggplot(aes(reorder(country_or_area, -weight_kg), log(weight_kg), fill = sign)) +
  geom_histogram(stat = "identity") +
  scale_fill_manual(values = c("seagreen", "indianred1", "gray70")) +
  coord_cartesian(ylim = c(20, 26)) +
  expand_limits(y = c(20, 30)) +
  labs(y = "Crude oil exports (log bil. kg) in 2016", title = "Most of 2016’s top crude oil exporters are \n becoming less vulnerable to climate risk.") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .2),
    text = element_text(size = 16, family = "LM Roman 10")
  ) +
  guides(fill = guide_legend(title = "Change in climate \n vulnerability, \n 2009–2015"))

# Second plot of US trends.
# See trends for the US.

usa <- df_trade %>%
  filter(year <= 2015 & year >= 1995) %>%
  filter(country_or_area == "USA") %>%
  filter(category %in% c("10_cereals")) %>%
  filter(flow == "Import")

# Join the climate vulnerability data.

usa_gain <- df_gain_delta %>%
  filter(name == "United States") %>%
  left_join(df_gdp, by = c("name", "year")) %>%
  select(year, gain, gdp) %>%
  mutate(year = as.numeric(year))

usa <- left_join(usa, usa_gain, by = "year") %>%
  mutate(value = log(trade_usd / gdp))

# Create a graph of gain vs. the commodities.

ggplot(usa, aes(year, value, group = commodity, color = commodity)) +
  geom_smooth(se = FALSE, method = "loess", color = "gray70") +
  geom_line(color = "skyblue4", size = 1.5, aes(year, gain)) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1,
      name = "Climate vulnerability score",
      breaks = waiver()
    )
  ) +
  labs(
    x = "Year",
    y = "GDP-weighted value of different cereal imports (log mil. $)",
    title = "U.S. imports of cereals have risen as climate vulnerability score worsened."
  ) +
  theme(
    axis.title.y = element_text(color = "gray50", face = "bold"),
    axis.title.y.right = element_text(color = "skyblue4", face = "bold"),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16, family = "LM Roman 10")
  )
