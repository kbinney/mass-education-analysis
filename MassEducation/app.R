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
library(leaflet.extras)
library(rgdal)
library(readxl)
library(janitor)
library(knitr)
library(shinythemes)
library(plotly)
library(ggrepel)
library(sp)

#colleges <- readOGR("colleges/COLLEGES_PT.shp")
schools <- readOGR("schools/SCHOOLS_PT.shp")
#districts <- readOGR("schooldistricts/SCHOOLDISTRICTS_POLY.shp")

# Have to convert shapefile to R - I have no idea why
schools_transformed <- spTransform(schools, CRS("+proj=longlat +ellps=GRS80"))
#colleges_transformed <- spTransform(colleges, CRS("+proj=longlat +ellps=GRS80"))
#districts_transformed <- spTransform(districts, CRS("+proj=longlat +ellps=GRS80"))

# massachusetts borders
bounding_box <- c(-74.1054,41.1389,-69.6605,43.0038)

all_data <- read_rds("joined_data")
passing_percents <- read_rds("passing_percents.rds")

# We are interested in percent of students meeting or exceeding
# expectations in a given school for some of our plots. First step is thus to calculate
# that percentage.
# school_test_score_percents <- all_data %>% 
#   filter(achievement_level %in% c("met", "exceeds")) %>% 
#   group_by(district_code, school_code, num_students_testing, grade, subject) %>% 
#   # We need to first sum the students at the desired achievement level in 
#   # a particular grade, otherwise we add students at level once for 
#   # both each grade and each achievement level. We should only add
#   # this for each grade, as it is a grade level summary. 
#   # TODO: make better for percent of students taking particular test in science
#   summarize(grade_students = sum(students_at_level)) %>% 
#   ungroup() %>% 
#   group_by(district_code, school_code) %>% 
#   summarize(test_percent = 100 * sum(grade_students) / sum(num_students_testing))

# We want some of our demographic data to go from cols to variables for easier plotting
dem_data <- all_data %>% 
  gather(key = "race", value = "race_percent", african_american:multi_race_non_hispanic) %>% 
  gather(key = "gender", value = "gender_percent", males:females) %>% 
  mutate(economically_disadvantaged_percent = 100 * economically_disadvantaged / total_students,
         ell_students_percent = 100 * ell_students / total_students,
         swd_percent = 100 * swd / total_students) %>% 
  distinct()
modified_data <- passing_percents %>% 
  left_join(dem_data)
# Define UI for Education application
ui <- navbarPage(
  "Massachusetts Schools",
  #theme = shinytheme("superhero"),
  # Add info panel
  tabPanel("Data Information",
           fluidPage(
             titlePanel("Information regarding this data set"),
             h2("Where is the data from?"),
             p(
               "All data is collected from the Massachusetts department of eduction."
             )
           )
  ),
  # Add education page with sidebar and plot
  tabPanel("Basic Scores",
           fluidPage(
             titlePanel("Generally, how do students do on these tests?"),
             sidebarLayout(
                sidebarPanel(
                    selectInput("grade", "Choose a grade:",
                                  choices = c("Third" = "3", 
                                              "Fourth" = "4", 
                                              "Fifth" = "5",
                                              "Sixth" = "6", 
                                              "Seventh" = "7", 
                                              "Eighth" = "8",
                                              "Tenth" = "10",
                                              "High School (science)" = "HS"),
                                selected = "3",
                                multiple = TRUE),
                    selectInput("level", "Choose achievement level:", 
                                  choices = c("Exceeded Expectations" = "exceeds", 
                                              "Met Expectations" = "met",
                                              "Partially Met Expectations" = "partially", 
                                              "Did Not Meet Expectations" = "not_meeting"),
                                selected = "met",
                                multiple = TRUE)
                                  
             ),
             mainPanel(
               plotOutput("gradePlot"),
               DT::dataTableOutput("baseTable")
             )
           )
          )
  ),
  tabPanel("Demographics",
           fluidPage(
              titlePanel("Do demographics impact a school's test scores?"),
              sidebarLayout(
                  sidebarPanel(
                    selectInput("dem",
                                "What demographic are you interested in?",
                                choices = c("Race" = "race_percent",
                                            "Gender" = "gender_percent",
                                            "Economic Disadvantage" = "economically_disadvantaged_percent",
                                            "Special Needs" = "swd_percent",
                                            "English Language Learners" = "ell_percent"))
                  ),
                  mainPanel(plotOutput("demPlot")),
                position = "right"
            )
          )
  ),
  tabPanel("School Qualities",
           fluidPage(
             titlePanel("Are there other predictive factors at schools that impact test scores?"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("school",
                             "What school or district characteristic are you interested in?",
                             choices = c("Per Pupil Spending" = "spending",
                                         "Graduation Percent" = "grad_percent",
                                         "Average Class Size" = "avg_class_size",
                                         "Number of Students Restrained" = "students_restrained",
                                         "Average Teacher Salary" = "average_salary"))
               ),
               mainPanel(plotOutput("qualPlot")),
               position = "left"
             )
           )
  ),
  # Add gender page with sidebar and plot
  tabPanel("Map", 
           fluidPage(
             leafletOutput("map"),
             textOutput('selected_schools')
           )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##################################################################
  #                                                                #
  # BASIC SCORES PLOTTING                                          #
  #                                                                #
  ##################################################################
  # Collect data to examine and use in table and plot output
   filtered_data <- reactive({all_data %>% 
       filter(grade %in% input$grade,
              achievement_level %in% input$level) %>% 
       group_by(school_name, district_code, school_code, subject, num_students_testing, grade) %>% 
       # We need to first sum the students at the desired achievement level in 
       # a particular grade, otherwise we add students at level once for 
       # both each grade and each achievement level. We should only add
       # this for each grade, as it is a grade level summary. 
       # TODO: make better for percent of students taking particular test in science
       summarize(grade_students = sum(students_at_level)) %>% 
       ungroup() %>% 
       group_by(school_name, district_code, school_code, subject) %>% 
       summarize(percent = 100 * sum(grade_students) / sum(num_students_testing))
     })
   
   output$gradePlot <- renderPlot({
     # We take the inputs (grades and levels) to get the percent of students
     # at that grade and level in a particular school. We make sure to not 
     # summarize over different types of tests
     filtered_data() %>% 
       ggplot(aes(x = percent, color = subject)) +
       geom_histogram(position = "identity", 
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                      fill = "white", 
                      alpha = 0.5) +
       xlab("Percent of students") + 
       ylab("Number of districts") +
       ggtitle("How are students in Massachusetts doing?")
   })

   
   ##################################################################
   #                                                                #
   # DEMOGRAPHICS PLOTTING                                          #
   #                                                                #
   ##################################################################
   output$demPlot <- renderPlot({
     # There are a lot of repeated rows because of the number of different
     # demographic categories intially in the dataset. However, these rows
     # are mostly repeated for the sake of the choosen columns. Distinct 
     # eliminates these repeated rows, making the dataset faster to plot. I 
     # choose to also keep both gender and race if one of them is selected
     # in order to allow faceting by these characteristics. Alternatively,
     # I could have selected just one, but the additionally code complexity
     # did not seem worth it for marginal speed benefit.
     if (input$dem == "race_percent" | input$dem == "gender_percent"){
         fill = switch(input$dem,
                       "race_percent" = "race",
                       "gender_percent" = "gender")
         plot <- modified_data %>% 
           select(input$dem, percent_passing, fill) %>% 
           distinct() %>% 
           ggplot(aes_string(x = input$dem, y = "percent_passing")) +
           facet_wrap(as.formula(paste0("~", fill)))
     } else {
       plot <- modified_data %>% 
         select(input$dem, percent_passing) %>% 
         distinct() %>% 
         ggplot(aes_string(x = input$dem, y = "percent_passing"))
     }
     plot +
       geom_point() +
       xlab("Percent of students in school of given demographic category") + 
       ylab("Percent of students Meeting or Exceeding Expectations") +
       ggtitle("Do demographics impact schools test scores?")
   })
   ##################################################################
   #                                                                #
   # SCHOOL CHARACTERISTIC PLOTTING                                 #
   #                                                                #
   ##################################################################
   output$qualPlot <- renderPlot({
     # There are a lot of repeated rows because of the number of different
     # demographic categories intially in the dataset. However, these rows
     # are mostly repeated for the sake of the choosen columns. Distinct 
     # eliminates these repeated rows, making the dataset faster to plot. I 
     # choose to also keep both gender and race if one of them is selected
     # in order to allow faceting by these characteristics. Alternatively,
     # I could have selected just one, but the additionally code complexity
     # did not seem worth it for marginal speed benefit.
     if (input$school == "spending"){
       modified_data <- modified_data %>% 
         gather(key = "spending_type", value = "amount", 
                in_district_per_pupil_spending, total_per_pupil_spending) %>% 
         select(spending_type, amount, percent_passing)
       plot <- modified_data %>% 
         distinct() %>% 
         ggplot(aes(x = amount, y = percent_passing)) +
         facet_wrap(~spending_type)
     } else {
       plot <- modified_data %>% 
         select(input$school, percent_passing) %>% 
         distinct() %>% 
         ggplot(aes_string(x = input$school, y = "percent_passing"))
     }
     plot +
       geom_point() +
       xlab("School Characteristic") + 
       ylab("Percent of students Meeting or Exceeding Expectations") +
       ggtitle("Do demographics impact schools test scores?")
   })

   ##################################################################
   #                                                                #
   # MAP PLOTTING                                                   #
   #                                                                #
   ##################################################################
   
   output$map <- renderLeaflet({
     schools_transformed <- merge(schools_transformed,passing_percents, by="SCHID")
     passing_levels <- unique(schools_transformed@data$percent_passing)
     pal <- colorNumeric(palette = "BuPu",
                        domain = c(0, 100))
     leaflet(schools_transformed) %>% 
       addProviderTiles("CartoDB") %>% 
       # draw on districts - This made html file too big for github to deal with nicely and would have been difficult to email so removing for now
       #addPolygons(data = districts_transformed,
       #opacity = 0.1, fill = FALSE) %>% 
       # put school locations on top
       addCircleMarkers(radius = 1,
                        opacity = 0.5,
                        label = ~NAME,
                        color = ~pal(percent_passing)) %>% 
       addLegend(position = "bottomright",
                 pal = pal,
                 values = c(0, 100)) %>% 
       setMaxBounds(lng1 = bounding_box[1],
                    lng2 = bounding_box[3],
                    lat1 = bounding_box[2],
                    lat2 = bounding_box[4])
       # Way to select points on Leaflet map. See more explanation
       # at https://stackoverflow.com/questions/42528400/plot-brushing-or-accessing-drawn-shape-geometry-for-spatial-subsets-in-shiny-lea
       # addDrawToolbar(
       #   targetGroup='draw',
       #   polylineOptions=FALSE,
       #   markerOptions = FALSE,
       #   circleOptions = TRUE)  %>%
       # addLayersControl(overlayGroups = c('draw'), options =
       #                    layersControlOptions(collapsed=FALSE)) 

   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

