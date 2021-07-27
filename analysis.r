library(tidyverse)
library(ggplot2)

norway_data <- read_csv("/Users/ryanlee/Documents/Projects/carbon-emissions-data/raw/norway_table_emissions.csv")
sweden_data <- read_csv("/Users/ryanlee/Documents/Projects/carbon-emissions-data/raw/sweden_table_emissions.csv")

#Scatterplots
ggplot(norway_data, aes(x = Year, y = Emissions)) + geom_point()
ggplot(sweden_data, aes(x = Year, y = Emissions)) + geom_point()

#Regressions of total emissions
norway_total_emissions_regression = lm(formula = Emissions ~ Year, data = norway_data)
sweden_total_emissions_regression = lm(formula = Emissions ~ Year, data = sweden_data)

#Regression of emissions per GDP
names(norway_data)[10] = 'gdp'
norway_gdp_emissions_regression = lm(formula = Emissions ~ Year / gdp, data = norway_data)

names(sweden_data)[10] = 'gdp'
sweden_gdp_emissions_regression = lm(formula = Emissions ~ Year / gdp, data = sweden_data)

#Scatterplots (emissions per GDP)
ggplot(norway_data, aes(x = Year, y = Emissions / gdp)) + geom_point()
ggplot(sweden_data, aes(x = Year, y = Emissions / gdp)) + geom_point()

#Regressions of emissions per capita
norway_emissions_per_capita_regression = lm(formula = Emissions ~ Year / Population, data = norway_data)
summary(norway_emissions_per_capita_regression)

sweden_emissions_per_capita_regression = lm(formula = Emissions ~ Year / Population, data = sweden_data)
summary(sweden_emissions_per_capita_regression)
