library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(spData)
library(sf)
library(wbstats)
library(sp)
library(rworldmap)
library(gapminder)
library(plotly)
library(countrycode)
library(tmap)




# Import excel file Cell phone & broadband
cellphone <- read_excel("cellphonep100.xlsx")
broadband <- read_excel("broadbandp100.xlsx")

#add country code to dfs
broadband[37,1] <- "Central African Republic"
cellphone$iso2c <- countrycode(cellphone$country, "country.name", "iso2c")
broadband$iso2c <- countrycode(broadband$country, "country.name", "iso2c")

#Add continents to dataframe cellphones & broadband
cellphone_continent <- cellphone %>% left_join(unique(gapminder[,1:2]), by = "country")
broadband_continent <- broadband %>% left_join(unique(gapminder[,1:2]), by = "country")


#Match years in cellphone to years in broadband (1998 - 2010)
cellphone_continent <- cellphone_continent[,c(-(2:34), -48) ]

#Make cellphone & broadband long
cellphone_continent_long <- cellphone_continent %>% gather(year, cellphones_p_100, -country, -continent, -iso2c)
broadband_continent_long <- broadband_continent %>% gather(year, broadband_p_100, -country, -continent, -iso2c)

#combine broadband & cellphone
combinedcb <- cellphone_continent_long %>% left_join(broadband_continent_long)

#Clean types
combinedcb[4:6] <- lapply(combinedcb[4:6], as.numeric)


#Add population & gdp per capita to df
population <- wb(indicator = "SP.POP.TOTL", startdate = 1998, enddate = 2010, country = "countries_only")
gdp <- wb(indicator = "NY.GDP.PCAP.CD", startdate = 1998, enddate = 2010, country = "countries_only")
colnames(population)[2] <- "year"
colnames(gdp)[2] <- "year"
population[2:3] <- lapply(population[2:3], as.numeric)
gdp[2:3] <- lapply(gdp[2:3], as.numeric)
combinedcb <- left_join(combinedcb, population[,c(2, 3, 6)], by = c("iso2c", "year"))
colnames(combinedcb)[7] <- "population"
combinedcb <- left_join(combinedcb, gdp[,c(2, 3, 6)], by = c("iso2c", "year"))
colnames(combinedcb)[8] <- "gdp_per_capita"


#Create map data

#Add geospatial data to combinedcb df
map_data <- inner_join(world, combinedcb[,c(2, 4, 5:6, 8)], by = c("iso_a2" = "iso2c"))

#Change map view to interactive
tmap_mode("view")

#Put name of country first in map_data
map_data <- map_data %>%
  select(name_long, everything())

#Filter map data for cellphone per 100
map_data %>%
  filter(year == 2010, continent == "Africa") %>%
  tm_shape() +
  tm_fill(title = "Cellphones per 100 (2010)</br>Source: Gapminder & World Bank", col = "cellphones_p_100", popup.vars = c("Cellphones per 100: " = "cellphones_p_100")) +
  tm_borders(col = "grey40") +
  tm_view(text.size.variable = TRUE)

