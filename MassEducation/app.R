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

colleges <- readOGR("colleges/COLLEGES_PT.shp")
schools <- readOGR("schools/SCHOOLS_PT.shp")
districts <- readOGR("schooldistricts/SCHOOLDISTRICTS_POLY.shp")

# Have to convert shapefile to R - I have no idea why
schools_transformed <- spTransform(schools, CRS("+proj=longlat +ellps=GRS80"))
colleges_transformed <- spTransform(colleges, CRS("+proj=longlat +ellps=GRS80"))
districts_transformed <- spTransform(districts, CRS("+proj=longlat +ellps=GRS80"))

# massachusetts borders
bounding_box <- c(-74.1054,41.1389,-69.6605,43.0038)

mcas_data_3 <- read_excel("MCAS_data/2018-3-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))

mcas_data_4 <- read_excel("MCAS_data/2018-4-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))

mcas_data_5 <- read_excel("MCAS_data/2018-5-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))

mcas_data_6 <- read_excel("MCAS_data/2018-6-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))

mcas_data_7 <- read_excel("MCAS_data/2018-7-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))

mcas_data_8 <- read_excel("MCAS_data/2018-8-NextGenMCAS.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(subject = parse_factor(subject, levels = NULL))


# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("superhero"),
   # Application title
   titlePanel("Massachusetts School Data"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      # Show a plot of the generated distribution
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("Plot by Level", 
                            sidebarPanel(
                     selectInput("dataset", "Choose a grade:", 
                                 choices = c("third", "fourth",
                                             "fifth", "sixth",
                                             "seventh", "eighth")),
                     selectInput("level", "Choose achievement level:",
                                 choices = c("Exceeded Expectations",
                                             "Met Expectations",
                                             "Partially Met Expectations",
                                             "Did Not Meet Expectations"))
                   ), plotOutput("gradePlot")),
                   tabPanel("Achievement over time", plotOutput("timePlot")),
                   tabPanel("Schools Map", leafletOutput("map"))
       )
     )
   )
  )
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "third" = mcas_data_3,
           "fourth" = mcas_data_4,
           "fifth" = mcas_data_5,
           "sixth" = mcas_data_6,
           "seventh" = mcas_data_7,
           "eighth" = mcas_data_8)
  })
  
  levelCol <- reactive({
    switch(input$level,
           "Exceeded Expectations" = "e_percent",
           "Met Expectations" = "m_percent",
           "Partially Met Expectations" = "pm_percent",
           "Did Not Meet Expectations" =  "nm_percent")
  })
   
   output$gradePlot <- renderPlot({
     datasetInput() %>% 
       #filter(district_name == input$district) %>% 
       ggplot(aes_string(x = levelCol(), color = "subject")) +
       geom_histogram(position = "identity", bins = 8, fill = "white", alpha = 0.5) +
       xlab("Percent of students") + 
       ylab("Number of districts") +
       ggtitle("How are students in Massachusetts doing?")
   })
  
   
   output$timePlot <- renderPlot({
     
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

