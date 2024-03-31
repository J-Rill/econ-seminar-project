library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Loading in dataset
df <- read.csv('owid-co2-data.csv')

# Filteirng to G7 countries only
g7_countries <- c("United States", "Canada", "Germany", "France", "Italy", "United Kingdom", "Japan")

filtered_df <- df[df$country %in% g7_countries,]

filtered_df$year <- as.Date(paste0(filtered_df$year, "-01-01"))

test_df <- filtered_df %>%
  filter(year >= as.Date('1950-01-01'))

ggplot(data=test_df, aes(x=year, y=co2_growth_prct, color=country)) + geom_line() + geom_point()
filtered_df$growth