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
library(scales)

# One tab is a map. To visualize the map, we need to read in the shape file and then
# convert it to R readable format. Furthermore, we join that data with testing data
# to visualize the spacial distribution of standardized testing scores. 

schools <- readOGR("schools/SCHOOLS_PT.shp")
schools_transformed <- spTransform(schools, CRS("+proj=longlat +ellps=GRS80"))

# Massachusetts borders, lat and long. Used to center leaflet map.

bounding_box <- c(-74.1054,41.1389,-69.6605,43.0038)

# School characteristics choices are labeled separately 
# rather than just in input so I can use the label
# choices as names in my plot as well as plot selections.

characteristic_choices <- 
  c("Per Pupil Spending" = "spending",
  "Graduation Percent" = "grad_percent",
  "Average Class Size" = "avg_class_size",
  "Average Teacher Salary" = "average_salary")
# characteristic_titles <-
#   c("Teachers salaries may have a very slight positive correlation with test scores" = "average_salary",
#     "Some schools have high graduation rates, despite low passing rates on the MCAS" = "grad_percent")

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

# all_data <- read_rds("joined_data")
test_data <- read_rds("all_test_data.rds")
passing_percents <- read_rds("passing_percents.rds")
dem_data <- read_rds("full_demographic_data.rds")
school_data <- read_rds("school_data.rds")

# Define UI for Education application. I was interested in doing
# many tabs with different selections for each tab, and thus chose
# a navbar frame work with various whole page options. 

ui <- navbarPage(
  "Massachusetts Schools",
  theme = shinytheme("yeti"),
  
  # The default panel contains a summary of the data source, interesting
  # findings, and a guide to the app.
  
  tabPanel("Home",
           fluidPage(
             titlePanel("What is the quality of Public Schools in Massachusetts?"),
             h2("Major Findings"),
             h3("Massachusetts schools may be failing some of our most vulnerable students"),
             p(paste("Higher percentages of African American, Hispanic  or economically ",
                    "disadvantaged students is correlated with lower rates of passing ",
                    "scores on state testing in Massachusetts public schools. Smaller ", 
                    "class sizes, higher per pupil spending (both in and out of district), ",
                    "and higher teacher salaries may help struggling schools. These factors ",
                    "are correlated with higher percents of students achieving passing scores ",
                    "on the MCAS (state test). Schools in the greater Boston suburbs generally ",
                    "have higher rates of students meeting or achieving expectations on the ",
                    "MCAS while schools in urban areas tend not to do as well.")),
             h2("Background Information"),
             p(paste("The MCAS (Massachhusetts Comprehensive Assessment System) is a statewide student",
                     "assessment initiated as part of the Education Reform Act of 1993. The MCAS are ", 
                     "administered between 3rd and 10th grade in subjects including math, reading ", 
                     "comprehension, language arts, and science. Students must pass the MCAS in Math, ",
                     "ELA (english language arts) and in a science subject to graduate from High School.")),
            a("Learn more about the MCAS here", 
              href = "https://en.wikipedia.org/wiki/Massachusetts_Comprehensive_Assessment_System"),
            p(paste("\nIn recent years, Massachusetts has realized MCAS were not meeting state goals. ",
                    "The state is in the process of switching to new standards. In this app, tests ",
                    "before High School were administered under the old system. In the new tests,", 
                    "students are assigned to a level of achievement in addition to getting a ",
                    "numeric score. From best to worst, the levels are",
                    "Exceeds Expectations, Meets Expectations, Partially Meets ",
                    "Expecations and Not Meeting Expectations. The old system, under which High ", 
                    "School tests were administed, also gives both scores and levels. Students were classified ", 
                    "as Advanced, Proficient, Needs Improvement, and Warning and Failing. In this app ",
                    "the old scores were mapped to the new ones. Schools report percentages of students ",
                    "at each achievement level rather than the raw scores of enrolled students.")),
            a("Learn more about the new MCAS here", 
              href = "http://www.doe.mass.edu/mcas/nextgen/"),
            h2("Data Source"),
            p("All data used in this project was collected from the Massachussetts Department of ", 
              "Education. Test scores are from Spring 2018 testing. Other information  ", 
              "is from the 2016-2017 school year, as this is the most recent available data."),
            a("Access the Department of Education data reports here.", 
              href = "http://profiles.doe.mass.edu/state_report/")
        )
  ),
  
  # The next panel allows users to see a high level overview of how well
  # students perform on these tests. Users can subset the data by grade and test
  # scores. I chose to visualize the data with a histogram to allow others to
  # see generally how schools across the state are doing - do lots of schools
  # have high percentages of students doing well? Doing poorly? Do more schools
  # do better on math or language arts? Does that differ across grades? I think
  # a histogram is a good way to answer these questions
  
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
                                multiple = TRUE),
                    helpText("This histogram looks at scores on the 2018 ",
                             "MCAS (Massachhusetts Comprehensive Assessment System) ",
                             "test, ie the state standardized test. ", 
                             "The state divides student scores into four ",
                             "achievement levels, and reports the percentage ",
                             " of students at each level in a given school. ",
                             "Note, students only have to take ",
                             "and pass one science test, so the total number ",
                             "of schools taking a given science test may be lower than ",
                             "for other grades or subjects. Students must meet ",
                             "testing standards to graduate from high school. ",
                             "Students must meet or exceed expectations on ELA, ",
                             "Math, and a Science test, or partially ",
                             "meet expectations and complete a school level ",
                             "improvement plan. Note, MA is currently switching ",
                             "to new standards for MCAS testing. The High School ",
                             "tests are still under the old system. In this app, we ",
                             "convert the old standards to the new labels for ",
                             "ease of visibility. Read more about the change in testing ",
                             " standards on the home page.")
                    
             ),
             mainPanel(
               plotOutput("gradePlot"),
               p(paste("Students appear to perform better on ELA tests: More schools have higher percents",
                       " of students meeting or exceeding expectations on English Language Arts tests than",
                       " math tests. This trend may reverse slightly in 7th and 8th grade, with more schools",
                       " with higher percentages of students doing well on the math test. However, by high ", 
                       "school students are doing better on the ELA test again."))
             )
           )
          )
  ),
  
  # After seeing schools in the aggregate, visitors to my shiny app can start to
  # pin down demographic characteristics associated with performance on test
  # scores. I was hoping initially to be able to separate school level
  # information by demographic (ie how well are students in different racial
  # groups doing at the same school). This information proved very difficult to
  # get from the Massachusetts Department of Education. It was much easier to
  # get a school's test scores, and independently, its demographic information.
  # I decided these relationships were still interesting. In this tab, I
  # visualize demographics and test scores in a 2D scatter plot, seeing if the
  # percent of students in a demographic category are correlated with higher or
  # lower passing test scores. I considered letting users choose which test
  # types or scores or grades they were interested in, but found this added
  # significant complexity and time to the app. Thus, I choose to precalculate
  # percent of students meeting or exceeding expectations accross all grades and
  # test subjects and compared demographics with this "passing percent"
  
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
                                            "Students with Disabilities" = "swd_percent",
                                            "English Language Learners" = "ell_students_percent")),
                    checkboxInput("demBestFit", "Add best fit line", FALSE),
                    helpText("Here, you can investigate relationships between ",
                             "the demographics of students in a school. On the ",
                             "y axis is the percent of students (across all grade ",
                             "levels and test subjects) who met or exceeded ", 
                             "expectations on the 2018 MCAS tests.",
                             "The MCAS are the Massachhusetts Comprehensive Assessment System, ",
                             " ie the state standardized test. ", 
                             " The state divides student scores into four ",
                             " achievement levels, and reports the percentage ",
                             " of students at each level in a given school.")
                  ),
                  mainPanel(plotOutput("demPlot"),
                            textOutput("demText")),
                  position = "right"
            )
          )
  ),
  
  # My second to last tab demostrates school characteristics rather than
  # demographic characteristics. This tab contains things that may be changable.
  # I also chose to include graduation percent, even though it isn't as
  # changable as say spending, to allow users to investigate if test scores are
  # correlated with other measures of success.
  
  tabPanel("School Qualities",
           fluidPage(
             titlePanel("Are there other predictive factors correlated with test scores?"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("school",
                             "What school or district characteristic are you interested in?",
                             choices = characteristic_choices),
                 checkboxInput("schoolBestFit", "Add best fit line", FALSE),
                 helpText("Salary and Per Pupil Expenditures are calculated ",
                          "at a district rather than school level, but the plots ",
                          "have one point per school, as there is school level ",
                          "testing data. Charter Schools do not have spending  or teacher salary data. ",
                          "Only high schools are plotted for graduation rates. ",
                          "On the y axis of this chart is the percent of students ",
                          "in a given school who met or exceeded ", 
                          "expectations on the 2018 MCAS tests. ",
                          "The MCAS are the Massachhusetts Comprehensive Assessment System, ",
                          "ie the state standardized test. ", 
                          "The state divides student scores into four ",
                          "achievement levels, and reports the percentage ",
                          "of students at each level in a given school.")
               ),
               mainPanel(plotOutput("qualPlot"),
                         textOutput("qualText")),
               position = "left"
             )
           )
  ),
  
  # My final tab is a map that contains points for all schools included in the
  # MCAS data. Schools are colored by their passing rates on MCAS testing,
  # creating a spatial visualization of one measure of school quality. Initally,
  # I wanted to allow users to select regions on the map and then show data
  # related to the selected schools. However, it turns out that region selection
  # on leaflet maps in shiny is not supported. The workarounds I tried ended up
  # making the map look significantly worse, and did not add much interesting
  # information. Instead, I visualized the spatial distribution of schools'
  # performance on the MCAS tests. Each school is plotted, and the color of that
  # marker is dependent on the percentage of students meeting or exceeding
  # expectations on the 2018 MCAS tests.
  
  tabPanel("Map", 
           fluidPage(
             titlePanel("Schools in Massachusetts"),
             leafletOutput("map"),
             p(paste("By mapping all schools in Massachusetts, we can ",
                     "see the spatial distribution of schools and their ",
                     "test scores. The color of a marker is the percent ",
                     "students in a school who met or exceeded expectations ",
                     "on the MCAS. The MCAS are the Massachhusetts ",
                     "Comprehensive Assessment System, ",
                     "the state standardized test. ", 
                     "The state divides student scores into four ",
                     "achievement levels, and reports the percentage ",
                     "of students at each level in a given school. ",
                     "We can see that schools in the greater ",
                     "Boston suburbs tend to have the highest percent of ",
                     "students doing well on the tests. Urban and rural ",
                     "schools do less well. You can zoom in and out to ",
                     "identify schools of interest."))
           )
  )
)

# Define server function logic to create visualizations and captions for the
# shiny app using inputs from the UI. Each tab's server logic is titled to
# divide the code more clearly.

server <- function(input, output) {
  
  ##################################################################
  #                                                                #
  # BASIC SCORES PLOTTING                                          #
  #                                                                #
  ##################################################################
  
  # Collect data to examine and use in plot output. We filter given the user's
  # input. Filtering can be direct because both grade and achievement level are
  # already strings. We take the inputs (grades and levels) to get the percent
  # of students at that grade and level in a particular school. We make sure to
  # not summarize over different types of tests, so visualization can be colored
  # by type of test
  
   filtered_data <- reactive({test_data %>% 
       filter(grade %in% input$grade,
              achievement_level %in% input$level) %>% 
       group_by(school_name, school_code, subject, num_students_testing, grade) %>% 
       
       # We need to first sum the students at the desired achievement level in 
       # a particular grade, otherwise we add students at level once for 
       # both each grade and each achievement level. We should only add
       # this for each grade, as it is a grade level summary. 
       
       summarize(grade_students = sum(students_at_level)) %>% 
       group_by(school_name, school_code, subject) %>% 
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
     # are mostly repeated when considering chosen columns. Distinct 
     # eliminates these repeated rows, making the dataset faster to plot. 
     
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
     
     full_plot <- plot +
       geom_point() +
       xlab("Percent of students in school of given demographic category") + 
       ylab("Percent of students Meeting or Exceeding Expectations") +
       ggtitle("Do demographics impact schools test scores?")
     
     if (input$demBestFit) {
       full_plot <- full_plot +
         geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)
     }
     
     full_plot
     
   })
   
   # I created distinct explanatory texts for each graph charted. Switching by
   # chosen demogrpahic matches the explanatory text with the correct chart.
    
   output$demText <- renderText({
     race_text <- paste("Increased percentages of hispanic and/or ",
                        "African American students are correlated ",
                        "with lower percentages of students meeting ",
                        "or exceeding expecations on the 2018 MCAS. ",
                        "In contrast, increased percentages of white ",
                        "and/or Asian students are correlated with ",
                        "higher percentages of students doing well.")
     gender_text <- paste("Correlations with gender are highly influenced ",
                          "by a few outlier schools with disproportionally ",
                          "large male populations. These schools include ",
                          "Springfield Public Day Elementary and Middle Schools ",
                          "which are both small public schools supporting students ",
                          "with mental health disabilities.",
                          "The other schools include William McKinley in Boston, ",
                          "which supports students with special needs, William R ",
                          "Fallon in Lynn, also a school working especially with ",
                          "students with additional education needs, and the school ",
                          "for Exceptional Studies in Lawrence, with is also an ",
                          "alternative educational program.")
     economic_text <- paste("More economically disadvantaged ",
                            "students in a school is correlated with lower percents ",
                            "of students meeting or exceeding expectations on the MCAS. ",
                            "A few standout schools: Pheonix Charter Academy, with ",
                            "a 100% passing rate, and 43% disadvantaged students, ",
                            "the O'Bryant School of Math and Science in Boston, with ",
                            "almost 90% passing and 46% economically disadvantaged, ",
                            "North Quincy High in Quincy, with almost 90% passing and ",
                            "34% economically disadvantaged, and Quaboag Regional High ",
                            "with 87% passing and 32% economically disadvantaged.")
     disabilities_text <- paste("Schools with very high percentages of students ",
                                "with disabilities generally do poorly on the MCAS. ",
                                "The schools in the lower right corner of this graph ",
                                "are all schools that offer some form of alternative ",
                                "education: they may thus have an emphasis other than ",
                                "standardized testing.")
     ell_text <- paste("Schools with higher percentages with english language learners ",
                       "generally have lower percentages of students doing well on the MCAS.",
                       "Pheonix Charter Academy and the Boston International High School ",
                       "go against this trend: Pheonix has 60% ELL students and a 100% pass ",
                       "rate on the MCAS, and Boston has 92% ELL students and a 55% pass rate.")
     text <- switch(input$dem,
            "race_percent" = race_text,
            "gender_percent" = gender_text,
            "economically_disadvantaged_percent" = economic_text,
            "swd_percent" = disabilities_text,
            "ell_students_percent" = ell_text)
     text
     
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
     
     # Charter schools in the dataset do not report spending or teacher salary
     # so when one of these is the desired characteristic, I filter out these 
     # schools before plotting.
     
     if (input$school == "spending" | input$school == "average_salary") {
       school_data <- school_data %>% 
         filter(!is.na(average_salary))
     }
     
     if (input$school == "spending"){
       modified_data <- school_data %>% 
         gather(key = "spending_type", value = "amount", 
                in_district_per_pupil_spending, total_per_pupil_spending) %>% 
         select(spending_type, amount, percent_passing) %>% 
         filter(!is.na(amount))
       
       plot <- modified_data %>% 
         distinct() %>% 
         ggplot(aes(x = amount, y = percent_passing)) +
         facet_wrap(~spending_type)
     } else {
       
       # Only high schools have grad rates, so I filter out the other schools
       # when this is the selected input. I only do this filtering when grad
       # rates is selected to avoid filtering out schools for which other
       # information is available when other characteristics are selected
       
       if (input$school == "grad_percent") {
         school_data <- school_data %>% 
           filter(!is.na(grad_percent))
       }
      
       # A few districts (Medford, Sharon) don't have class size data
       # so I filter out na rows from these districts.
       
       if (input$school == "avg_class_size") {
         school_data <- school_data %>% 
           filter(!is.na(avg_class_size))
       }
       
       plot <- school_data %>% 
         select(input$school, percent_passing) %>% 
         distinct() %>% 
         ggplot(aes_string(x = input$school, y = "percent_passing"))
     }
     
     # The salary data leads to long axis tick labels on the x axis, so I format
     # with salary data rather than scientific notation and label the x axis
     # with the nice name (rather than formatted name) of the axis chosen
    
     full_plot <- plot +
       geom_point() +
       scale_x_continuous(labels = scales::comma) +
       theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
       xlab(names(characteristic_choices)[characteristic_choices == input$school]) + 
       ylab("Percent of students Meeting or Exceeding Expectations") +
       ggtitle("Which school characteristics are correlated with test scores?")
     
     if (input$schoolBestFit) {
       full_plot <- full_plot +
         geom_smooth(na.rm = TRUE, method = "lm", se = FALSE) 
     }
     
     full_plot
   })
   
   # Below each plot is a description of the content, including pointing out
   # interesting data points and explaining what may cause these apparant
   # outliers.
   
   output$qualText <- renderText({
     spending_text <- paste("Per pupil spending is calculated for both ",
                            "in district and out of district expenditures. ",
                            "Schools may have to pay to send students to ",
                            "schools out of district if they cannot meet ",
                            "the needs of the student, accounting for ",
                            "differences between in-district and total ",
                            "per pupil expenditures.")
     grad_text <- paste("Passing MCAS scores are correlated with higher grad ",
                        "rates, a good indication that MCAS are a reasonable ",
                        "measure of student success. However, a number ",
                        "of school have high graduation rates and low percents ",
                        "of students meeting or exceeding expectations on the MCAS.",
                        "This suggests that Massachusetts schools may not be ",
                        "equipping all our graduates with the skills they will need ",
                        "for life after college.")
     class_text <- paste("The importance of class size is debated in education.",
                        "Some believe smaller class sizes may improve educational ",
                         "quality, but the others have pointed out that there are ",
                         "challenges in achieving smaller class sizes that may ",
                         "mitigate any benefits of smaller classes. These challenges ",
                         "include procuring quality teachers, finding classroom space ",
                         "and additional employee costs")
     teacher_text <- paste("Teacher salaries in public schools are generally determined ",
                           "by contracts with unions. Some advocates of charter schools ",
                           "argue that the ability of charter schools, which generally ",
                           "do not contract with unions, to pay teachersbased on performance ",
                           "is very important to their success.")
     switch(input$school,
            "spending" = spending_text,
            "grad_percent" = grad_text,
            "avg_class_size" = class_text,
            "average_salary" = teacher_text) 
   })

   ##################################################################
   #                                                                #
   # MAP PLOTTING                                                   #
   #                                                                #
   ##################################################################
   
   # Initially, I planned to allow users to select schools on a map in order
   # to see schools in towns near them. However, unlike plot selection, there
   # is no clear way to select points on a map with leaflet. I still wanted to
   # visualize something spatially, so I chose to map schools, with the color
   # of the marker being the percent of students meeting or exceeding 
   # expectations on the MCAS. This visualization ended up being quite interesting
   # as there appear to be spatial divisions in how well schools do on MCAS tests.
   
   output$map <- renderLeaflet({
     
     # To visualize MCAS performance, I need to merge my spatial dataset and my
     # test score dataset. The spatial dataset contains more than just public
     # schools. Only public schools have test data, so I chose to remove schools
     # without testing data from the data set I visualize. When plotting each
     # school, I set the color of the school marker to be the percent of
     # students in that school who met or exceeded expectations on the 2018
     # MCAS. In the label that appears when a marker is hovered is the name of
     # the school and the actual percentage (rather than just color)
     
     schools_transformed <- merge(schools_transformed, passing_percents, by="SCHID") %>% 
       subset(!is.na(percent_passing))
     passing_levels <- unique(schools_transformed@data$percent_passing)
     pal <- colorNumeric(palette = "BuPu",
                        domain = c(0, 100))
     leaflet(schools_transformed) %>% 
       addProviderTiles("CartoDB") %>% 
       addCircleMarkers(radius = 1,
                        opacity = 0.5,
                        label = ~paste(sep = "",
                                      "Percent Passing: ",
                                      number(percent_passing, accuracy = 1),
                                      ", ", NAME),
                        color = ~pal(percent_passing)) %>% 
       addLegend(position = "bottomleft",
                 pal = pal,
                 values = c(0, 100),
                 title = "Students passing MCAS") %>% 
       
       # It is easy on leaflet to zoom or move outside the 
       # interesting area. Setting the bounds around MA
       # prevents this from occuring.
       
       setMaxBounds(lng1 = bounding_box[1],
                    lng2 = bounding_box[3],
                    lat1 = bounding_box[2],
                    lat2 = bounding_box[4])
   })
   
}

# Run the application 

shinyApp(ui = ui, server = server)

