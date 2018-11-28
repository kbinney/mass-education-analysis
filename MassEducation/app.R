#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(readxl)
library(janitor)
library(knitr)
library(shinythemes)
library(plotly)

colleges <- readOGR("colleges/COLLEGES_PT.shp")
schools <- readOGR("schools/SCHOOLS_PT.shp")
districts <- readOGR("schooldistricts/SCHOOLDISTRICTS_POLY.shp")

# Have to convert shapefile to R - I have no idea why
schools_transformed <- spTransform(schools, CRS("+proj=longlat +ellps=GRS80"))
colleges_transformed <- spTransform(colleges, CRS("+proj=longlat +ellps=GRS80"))
districts_transformed <- spTransform(districts, CRS("+proj=longlat +ellps=GRS80"))

# massachusetts borders
bounding_box <- c(-74.1054,41.1389,-69.6605,43.0038)

all_data <- read_rds("joined_data")


# Define UI for application that draws a histogram

ui <- navbarPage(
  "Massachusetts Schools",
  theme = shinytheme("superhero"),
  # Add info panel
  tabPanel("Data Information",
           fluidPage(
             titlePanel("Information regarding this data set"),
             h2("Where is the data from?"),
             p(
               "All data is collected from the Massachusetts department of eduction. You can see the data yourself. "
             )
           )
  ),
  # Add education page with sidebar and plot
  tabPanel("Who's in school?",
           fluidPage(
             titlePanel("How does the demographic make up of a school impact test scores?"),
             sidebarLayout(
                sidebarPanel(
                    selectInput("grade", "Choose a grade:",
                                  choices = c("third" = "3", 
                                              "fourth" = "4", 
                                              "fifth" = "5",
                                              "sixth" = "6", 
                                              "seventh" = "7", 
                                              "eighth" = "8")),
                    selectInput("level", "Choose achievement level:", 
                                  choices = c("Exceeded Expectations" = "exceeds_percent", 
                                              "Met Expectations" = "met_percent",
                                              "Partially Met Expectations" = "partially_percent", 
                                              "Did Not Meet Expectations" = "not_meeting_percent"))
                                  
             ),
             mainPanel(plotOutput("gradePlot"))
           )
          )
  ),
  # Add gender page with sidebar and plot
  tabPanel("Map", 
           fluidPage(
             leafletOutput("map")
           )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$gradePlot <- renderPlot({
     all_data %>% 
       filter(grade == input$grade) %>% 
       ggplot(aes_string(x = input$level, color = "subject")) +
       geom_histogram(position = "identity", bins = 8, fill = "white", alpha = 0.5) +
       xlab("Percent of students") + 
       ylab("Number of districts") +
       ggtitle("How are students in Massachusetts doing?")
   })
  
   output$map <- renderLeaflet({
     school_types <- levels(schools_transformed@data$TYPE_DESC)
     pal <- colorFactor(palette = "Accent",
                        levels = school_types)
     leaflet(schools_transformed) %>% 
       addProviderTiles("CartoDB") %>% 
       # draw on districts - This made html file too big for github to deal with nicely and would have been difficult to email so removing for now
       #addPolygons(data = districts_transformed,
       #opacity = 0.1, fill = FALSE) %>% 
       # put school locations on top
       addCircleMarkers(radius = 3,
                        opacity = 1,
                        label = ~NAME,
                        color = ~pal(TYPE_DESC)) %>% 
       addLegend(position = "bottomright",
                 pal = pal, 
                 values = school_types) %>% 
       setMaxBounds(lng1 = bounding_box[1],
                    lng2 = bounding_box[3],
                    lat1 = bounding_box[2],
                    lat2 = bounding_box[4])

   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

