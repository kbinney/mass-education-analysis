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

# One tab is a map. To visualize the map, we need to read in the shape file and then
# convert it to R readable format. Furthermore, we join that data with testing data
# to visualize the spacial distribution of standardized testing scores. 

schools <- readOGR("schools/SCHOOLS_PT.shp")
schools_transformed <- spTransform(schools, CRS("+proj=longlat +ellps=GRS80"))

# Massachusetts borders, lat and long. Used to center leaflet map.

bounding_box <- c(-74.1054,41.1389,-69.6605,43.0038)

# I cleaned the data into 3 data frames ready for use here. 
# joined data contains all the raw information, including
# various school characteristics and all test scores. It can be used
# to allow people to download data nicely prejoined and formatted. 
# Passing percents is a much smaller data set consisting of school
# level passing percents across all tests. (Ie percent of students
# who met or exceeded expectations across all grades and test types)
# Finally, dem_data is precleaned data to be used for visualizing
# correlations between demographics and test scores. It has already
# joined the demographic information and the passing percents.

all_data <- read_rds("joined_data")
passing_percents <- read_rds("passing_percents.rds")
dem_data <- read_rds("full_demographic_data.rds")
school_data <- read_rds("school_data.rds")

# Define UI for Education application. I was interested in doing
# many tabs with different selections for each tab, and thus chose
# a navbar frame work with various whole page options. 

ui <- navbarPage(
  "Massachusetts Schools",
  theme = shinytheme("superhero"),
  # The default panel contains a summary of the data source, interesting findings, and a
  # guide to the app
  
  tabPanel("Home",
           fluidPage(
             titlePanel("Information regarding this data set"),
             h2("Where is the data from?"),
             p(
               "All data is collected from the Massachusetts department of eduction."
             )
           )
  ),
  # The next panel allows users to see a high level overview of how well students
  # perform on these tests. Users can subset the data by grade and test scores. I
  # chose to visualize the data with a histogram to allow others to see generally 
  # how schools across the state are doing - do lots of schools have high percentages
  # of students doing well? Doing poorly? Do more schools do better on math or language arts?
  # Does that differ across grades? I think a histogram is a good way to answer these questions
  
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
  
  # After seeing schools in the aggregate, visitors to my shiny app can start to 
  # pin down demographic characteristics associated with performance on test scores. 
  # I was hoping initially to be able to separate school level information by demographic
  # (ie how well are students in different racial groups doing at the same school). This
  # information proved very difficult to get from the Massachusetts department of education. 
  # It was much easier to get a schools test scores, and independently, its demographic 
  # information. I decided these relationships were still interesting. In this tab, 
  # I visualize demographics and test scores in a 2D scatter plot, seeing if percent of 
  # in a demographic category demostrate correlation with higher or lower passing test 
  # scores. I considered letting users choose which test types or scores or grades they
  # were interested in, but found this added significant complexity and time to the app. 
  # Thus, I choose to precalculate percent of students meeting or exceeding expectations
  # accross all grades and test subjects. 
  
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
  
  # My second to last tab demostrates school characteristics rather than demographic
  # characteristics. This tab contains things that may be changable. I also chose
  # to include graduation percent, even though it isn't as changable as say spending, 
  # to allow users to investigate if test scores are correlated with other measures of 
  # success. 
  
  tabPanel("School Qualities",
           fluidPage(
             titlePanel("Are there other predictive factors correlated with test scores?"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("school",
                             "What school or district characteristic are you interested in?",
                             choices = c("Per Pupil Spending" = "spending",
                                         "Graduation Percent" = "grad_percent",
                                         "Average Class Size" = "avg_class_size",
                                         "Average Teacher Salary" = "average_salary"))
               ),
               mainPanel(plotOutput("qualPlot")),
               position = "left"
             )
           )
  ),
  
  # My final tab is a map that contains points for all schools included in the MCAS data.
  # Schools are colored by their passing rates on MCAS testing, creating a spatial visualization
  # of one measure of school quality. Initally, I wanted to allow users to select regions on 
  # the map and then show data related to the selected schools. However, it turns out that
  # region selection on leaflet maps in shiny is basically not supported. The workarounds
  # I tried ended up making the map look significantly worse, and did not add much interesting
  # information.
  
  tabPanel("Map", 
           fluidPage(
             leafletOutput("map"),
             textOutput('selected_schools')
           )
  )
)


# Define server function logic to create visualizations and captions for
# the shiny app using inputs from the UI. Each tab's server logic
# is titled to divide the code more clearly.

server <- function(input, output) {
  
  ##################################################################
  #                                                                #
  # BASIC SCORES PLOTTING                                          #
  #                                                                #
  ##################################################################
  # Collect data to examine and use in plot output. We filter given
  # the user's input. Filtering can be direct because both grade 
  # and achievement level are already strings. 
  # We take the inputs (grades and levels) to get the percent of students
  # at that grade and level in a particular school. We make sure to not 
  # summarize over different types of tests, so visualization can be
  # colored by type of test
  
   filtered_data <- reactive({all_data %>% 
       filter(grade %in% input$grade,
              achievement_level %in% input$level) %>% 
       group_by(school_name, district_code, school_code, subject, num_students_testing, grade) %>% 
       
       # We need to first sum the students at the desired achievement level in 
       # a particular grade, otherwise we add students at level once for 
       # both each grade and each achievement level. We should only add
       # this for each grade, as it is a grade level summary. 
       
       summarize(grade_students = sum(students_at_level)) %>% 
       group_by(school_name, district_code, school_code, subject) %>% 
       summarize(percent = 100 * sum(grade_students) / sum(num_students_testing))
     })
   
   # Once we've formatted the data, it is relatively easy to plot. The historgram's
   # breaks are manually set to ensure we see bins for all possible percents.
   
   output$gradePlot <- renderPlot({
     filtered_data() %>% 
       ggplot(aes(x = percent, color = subject)) +
       geom_histogram(position = "identity", 
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                      fill = "white", 
                      alpha = 0.5) +
       xlab("Percent of students in given school with specified score(s)") + 
       ylab("Number of schools") +
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
         plot <- dem_data %>% 
           select(input$dem, percent_passing, fill) %>% 
           distinct() %>% 
           ggplot(aes_string(x = input$dem, y = "percent_passing")) +
           facet_wrap(as.formula(paste0("~", fill)))
     } else {
       plot <- dem_data %>% 
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
       modified_data <- school_data %>% 
         gather(key = "spending_type", value = "amount", 
                in_district_per_pupil_spending, total_per_pupil_spending) %>% 
         select(spending_type, amount, percent_passing)
       plot <- modified_data %>% 
         distinct() %>% 
         ggplot(aes(x = amount, y = percent_passing)) +
         facet_wrap(~spending_type)
     } else {
       plot <- school_data %>% 
         # TODO explain class size
         filter(avg_class_size > 0) %>% 
         select(input$school, percent_passing) %>% 
         distinct() %>% 
         ggplot(aes_string(x = input$school, y = "percent_passing"))
     }
     plot +
       geom_point() +
       geom_smooth(na.rm = TRUE, method = "lm", se = FALSE) +
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
     schools_transformed <- merge(schools_transformed,passing_percents, by="SCHID") %>% subset(!is.na(percent_passing))
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
       addLegend(position = "bottomleft",
                 pal = pal,
                 values = c(0, 100),
                 title = "Students passing MCAS") %>% 
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

