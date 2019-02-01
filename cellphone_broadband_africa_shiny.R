#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


# Define UI for application that draws graphs for Cellphone & broadband availability on the African continent
ui <- fluidPage(
   
   # Application title
   titlePanel("Cellphone and broadband availability in Africa"),
   
   # Sidebar with year input and button to draw the plot
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId =  "year",
                     label = "Choose the year/s",
                     choices = c(2004:2010),
                     multiple = T),
         actionButton(inputId = "button", label = "Update graph")
      
        
   ),
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("cellphone_graph")
      )
   ) 
)

subset(combinedcb, year == 2003)

# Define server logic required to draw a the plot
server <- function(input, output) {


   output$cellphone_graph <- renderPlotly({
     
     y <- eventReactive(input$button, {
       input$year
     })
     #Check to only draw plot if at least one year is chosen.
     if (!is.null(y())) {
      # draw the plot with selected years for countries in Africa
      ggplotly((combinedcb %>%
       filter(continent == "Africa", year == y()) %>%
       
       ggplot(aes(cellphones_p_100, broadband_p_100, size = population, color = gdp_per_capita, 
                  text = paste("</br>Country: ", country, "</br>Cellph. p. 100: ", round(cellphones_p_100, digits = 2),
                               "</br>Broadb. p. 100: ", round(broadband_p_100, digits = 2), "</br>Population", population,
                               "</br>GDP p. capita: ", round(gdp_per_capita, digits = 0)))) +
       geom_point(alpha = (2/3)) +
       xlab("Cellphones per 100") +
       ylab("Broadband per 100") +
       ylim(0,7) +
       theme_bw() +
       labs(title = "Broadband & cellphone availability in African countries", caption = "Source: Gapminder & World Bank") +
       scale_color_distiller(palette = "Greens") +
       facet_wrap(~ year)), tooltip = "text")
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

