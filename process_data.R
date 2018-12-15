library(tidyverse)
library(readxl)
library(janitor)
library(knitr)
library(fs)

# Downloading data from the massachusetts department of education is a bit
# tricky. To get different grade levels for test scores, I had to download
# different files. Luckily these files are mostly formatted the same, so I can
# read them in with a map. The column names aren't very informative, but the
# website explained they were the different test levels, so I looked up the
# names for each abbreviation and renamed the columns.

filenames <- dir_ls("final_project_data/new_data/") %>% 
  str_subset("mcas-grade")

mcas_data <-
  map_dfr(filenames, read_xlsx, skip = 1, .id = "file_index") %>%
  clean_names() %>%
  transmute(
    "school_name" = school_name,
    "school_code" = school_code,
    "subject" = subject,
    "exceeds" = e_number,
    "met" = m_number,
    "partially" = pm_number,
    "not_meeting" = nm_number,
    "num_students_testing" = no_of_students_included,
    "file_index" = file_index
  ) %>%
  
  # For some reason, map_dfr with .id is giving list indexes rather than file
  # names so we manually convert to path name. We can then parse out the grade
  # from the file name.
  
  mutate(
    file_index = parse_number(file_index),
    source = filenames[file_index],
    grade = str_remove(source, "final_project_data/new_data/mcas-grade-"),
    grade = str_remove(grade, ".xlsx")
  ) %>%
  
  # Most of the columns were assumed to be characters characters, so we need to
  # switch to them parse as numbers
  
  mutate(
    exceeds = parse_number(exceeds),
    met = parse_number(met),
    partially = parse_number(partially),
    not_meeting = parse_number(not_meeting),
    num_students_testing = parse_number(num_students_testing)
  ) %>%
  
  # The index and source were just used to identify the grade levels, so can be
  # omitted from future data analysis.
  
  select(everything(),-file_index,-source)

# The high school science and 10th grade MCAS (standardized test) data files
# have a different format, so we read in separately before joining.

special_filenames <- c("final_project_data/new_data/grade-10-reg.xlsx",
                       "final_project_data/new_data/hs-science.xlsx")  

# These files are a different format partially because the tests use an old
# scoring system. I convert for joining, and will note this conversion on my
# app. I also use the file name to figure out the year of students taking the
# test. The science test is taken by high schoolers in various years.

mcas_old_data <-
  map_dfr(special_filenames, read_xlsx, skip = 1, .id = "file_index") %>%
  clean_names() %>%
  transmute(
    "school_name" = school_name,
    "school_code" = school_code,
    "subject" = subject,
    "exceeds" = a_number,
    "met" = p_number,
    "partially" = ni_number,
    "not_meeting" = w_f_number,
    "num_students_testing" = student_included,
    "file_index" = file_index
  ) %>%
  mutate(
    file_index = parse_number(file_index),
    source = special_filenames[file_index],
    grade = str_remove(source, "final_project_data/new_data/"),
    grade = str_remove(grade, ".xlsx"),
    grade = case_when(grade == "grade-10-reg" ~ "10",
                      TRUE ~ "HS")
  ) %>%
  select(everything(), -file_index, -source)

# We join all testing data into a single data frame. The old and new mcas have
# different notation for test types, so I rename some of the subject variables.

all_test_data <- bind_rows(mcas_data, mcas_old_data) %>% 
  mutate(subject = case_when(subject == "ELA" ~ "ENGLISH LANGUAGE ARTS",
                             subject == "MATH" ~ "MATHEMATICS",
                             TRUE ~ subject)) %>% 
  
  # We would like to switch acheivement level to be an observation instead of a
  # variable so that we can easily group by achievement level
  
  gather(key = "achievement_level", "students_at_level", exceeds:not_meeting)
  

# I use just the test data in one tab of my app. Initially, I joined the test
# data to the rest of the demographics data, but that led to a very large data
# frame that slowed down the app. I thus save the test data alone to be read in
# by the app.

all_test_data %>% 
  write_rds("MassEducation/all_test_data.rds")

# After getting all the data for MCAS by school and grade level, we read in
# other information about schools. This information includes various
# demographics (race, gender, english as a second language, students with
# special needs) and school or district level characteristics, like spending,
# class size, and teacher salaries

# The first data frame contains details of class sizes, gender percents, english
# language learners, students with disabolities and students who are
# economically disadvantaged.

# The Medford and Sharon school districts and the Greenfield Commonwealth Virtual
# District have all 0s for each input. They appear not to have accurately
# provided this information, so information relating to these schools is filtered
# out of the dataset. These are the only districts with average class size of 0,
# so I use that to do my filtering.

class_size_gender <- read_excel("final_project_data/new_data/ClassSizebyGenPopulation.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(number_classes = parse_number(total_number_of_classes),
         avg_class_size = parse_number(average_class_size),
         num_students = parse_number(number_of_students),
         female_percent = parse_number(female_percent),
         ell_percent = parse_number(english_language_learner_percent),
         
         # The students with disabilities column has a few strange inputs, which I interpret as NA
         
         swd_percent = parse_number(students_with_disabilities_percent, na = c("", "NA", "#####")),
         ec_disad_percent = parse_number(economically_disadvantaged_percent)) %>% 
  select(school_code,
         number_classes,
         avg_class_size,
         num_students,
         ell_percent,
         swd_percent,
         ec_disad_percent) %>% 
  filter(avg_class_size > 0)

# I also looked at enrollment in school across standard racial groups. This was
# already nicely formatted.

race_gender_enrollment <- read_excel("final_project_data/new_data/enrollmentbyracegender-school.xlsx", skip = 3, trim_ws = TRUE) %>% 
  clean_names() %>% 
  rename(school_name = school, school_code = org_code) %>% 
  
  # The school code has an extra space to start. We need to remove to later join the data set
  
  mutate(school_code = str_trim(school_code)) %>% 
  select(everything(), -school_name)

# High schools have graudation rates, an interesting metric to compare with mcas
# testing data.

grad_rates <- read_excel("final_project_data/new_data/gradrates.xlsx", skip = 1) %>% 
  clean_names() %>% 
  
  # Most columns were automatically parsed as characters but were parsed as
  # numbers. In parsing as numbers, I also make some of the column names shorter.
  
  mutate(cohort_size = parse_number(number_in_cohort),
         grad_percent = parse_number(percent_graduated),
         in_school_percent = parse_number(percent_still_in_school),
         complete_non_grad_percent = parse_number(percent_non_grad_completers),
         hs_equiv_percent = parse_number(percent_h_s_equiv),
         drop_out_percent = parse_number(percent_dropped_out),
         excluded_percent = parse_number(percent_permanently_excluded)) %>% 
  select(school_code,
         cohort_size,
         grad_percent,
         in_school_percent,
         complete_non_grad_percent,
         hs_equiv_percent,
         drop_out_percent,
         excluded_percent)

# Spending is by district rather than school, which needs to be kept in mind
# when joining to school level data. Money columns are stored with $ signs,
# which need to be removed before we can parse the column as a number. Cleaning
# names here helps some, but many of the columns are not clear, so I rename them
# and remove an extra column that was empty. Student_equiv refers to the number
# of students who were in school full time, and accounts for students who arrive
# or leave partway through the year.

spending <- read_excel("final_project_data/new_data/PerPupilExpenditures.xlsx", skip = 1) %>% 
  clean_names() %>% 
  select(everything(), -x_1) %>% 
  mutate(in_district_expenses = parse_number(str_replace(in_district_expenditures , "$", "")),
         in_district_student_equiv = parse_number(total_in_district_ft_es),
         in_district_per_pupil_spending = parse_number(str_replace(in_district_expenditures_per_pupil, "$", "")),
         total_expenses = parse_number(str_replace(total_expenditures, "$", "")),
         total_student_equiv = parse_number(total_pupil_ft_es),
         total_per_pupil_spending = parse_number(str_replace(total_expenditures_per_pupil, "$", ""))
        ) %>% 
  select(district_name,
         district_code,
         in_district_expenses,
         in_district_student_equiv,
         in_district_per_pupil_spending,
         total_expenses,
         total_student_equiv,
         total_per_pupil_spending)

# Teacher salaries were collected as district level data. The salaries are
# marked with $ signs, which need to be removed before we can convert to numbers

salaries <- read_excel("final_project_data/new_data/TeacherSalaries.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(salary_totals = parse_number(str_replace(salary_totals, "$", "")),
         average_salary = parse_number(str_replace(average_salary, "$", "")),
         fte_count = parse_number(fte_count))

# Though the class size data file includes some population percentages, selected
# populations contains more distinct populations

pops <- read_excel("final_project_data/new_data/selectedpopulations.xlsx", skip = 3) %>% 
  clean_names() %>% 
  
  # The data spreadsheet has one row with section names, and one with % vs. # I
  # keep just the numbers, except use high_needs_percent to determine total
  # students, useful for future ability to group by district and still get
  # accurate percents. I need to remove the row of % and #
  
  transmute(school_code = orgcode,
         non_native_english = first_language_not_english,
         ell_students = english_language_learner,
         swd = students_with_disabilities,
         high_needs,
         high_needs_percent = x_4,
         economically_disadvantaged) %>% 
  slice(2:n()) %>% 
  mutate(non_native_english = parse_number(non_native_english),
         ell_students = parse_number(ell_students),
         swd = parse_number(swd),
         high_needs = parse_number(high_needs),
         high_needs_percent = parse_number(high_needs_percent),
         economically_disadvantaged = parse_number(economically_disadvantaged)) %>% 
  
  # All schools have a high needs column for both number and percent. I use
  # these two columns to calculate the number of students in a school
  
  mutate(total_students = high_needs / (high_needs_percent / 100))

# After collecting all the data from various files, I want to join into a single
# data frame. The files either have a district or school code that is consistent
# accross files. Below, see the number of observations in each data set and
# whether it will have school codes or district codes. This information is
# needed to appropriately join the data frames.

# MCAS: 1638 unique schools (school)
# class_size_gender: 1850 (school)
# race_gender_enrollment: 1848 (school)
# grad_rates: 384 (high school)
# spending: 323 (district)
# pops: 1848 (school)
# salaries: 324 (district)

# After reading in all demographic data, I want to join it into one large data
# frame with the mcas data Some data frames are by school and some by district.
# I start with joining all schools and all districts. Initially, I chose to join
# all this data into a single data frame and then partitioned it in the shiny
# app. However, Shiny wasn't able to deal with a data set this big, so after
# joining I partitioned the data by which tab in my app I use it in and saved
# each tab's data into a separate data frame.

school_data <- all_test_data %>% 
  left_join(class_size_gender) %>% 
  left_join(race_gender_enrollment) %>% 
  left_join(grad_rates) %>% 
  left_join(pops) %>% 
  separate(school_code, into = c("district_code", "school_code"), sep = 4)


# The Department of Education website says that the "school code's" first 4
# digits is the district code and the second 4 digits are a school level code. I
# thus get the district code that's actually district representative, even
# though it is stored as 8 digits - the last 4 are all 0s.
district_data <- salaries %>% 
  left_join(spending) %>% 
  mutate(district_code = str_sub(district_code, 1, 4))

all_data <- school_data %>% 
  left_join(district_data)

# For a lot of my plots, I'm intersted in everyone in a given school that meets
# or exceeds expectations. Doing this analysis in my shiny app is slow, so I
# create a new data frame with just this information (ie percents of students
# "passing" - meeting or exceeding expectations - in a given school)

passing_percents <- all_data %>% 
  
  # To calculate this percent, it will be easier if achivement level is in columns
  
  spread(achievement_level, students_at_level) %>% 
  
  # Here, I group to get all tests and all grades at a given school
  
  group_by(school_name, district_code, school_code) %>% 
  summarize(percent_passing = 100 * (sum(exceeds) + sum(met)) / sum(num_students_testing)) %>% 
  
  # I want to join this data with my shapefile data in the shiny app to show
  # this data visually. The shape file has a single school id, so I add that
  # column back in here (ie merge the district and school parts of the back into
  # 8 digit string)
  
  mutate(SCHID = paste0(district_code, school_code)) %>% 
  ungroup() 

passing_percents %>% 
  write_rds(path = "MassEducation/passing_percents.rds")
  
# Some of the demographic data requires reformatting before it can be used as
# desired in the Shiny App. In particular, some rows need to be converted to
# percents, and different race and gender categories are already in percents but
# need to be gathered into a single column. Originally I did this in my shiny
# app, but after finalizing what characteristics I was interested in displaying
# it became clear it made more sense to precreate the needed dataframe, save it,
# and read it into shiny.

dem_data <- all_data %>%
  gather(key = "race",
         value = "race_percent",
         african_american:multi_race_non_hispanic) %>%
  gather(key = "gender", value = "gender_percent", males:females) %>%
  mutate(
    economically_disadvantaged_percent = 100 * economically_disadvantaged / total_students,
    ell_students_percent = 100 * ell_students / total_students,
    swd_percent = 100 * swd / total_students
  ) %>%
  select(
    school_name,
    district_code,
    school_code,
    race,
    race_percent,
    gender,
    gender_percent,
    economically_disadvantaged_percent,
    ell_students_percent,
    swd_percent
  ) %>%
  distinct()

# I use this data alongside testing data, so join the testing data with this
# demographic data before saving into an rds file to use in my shiny app.

full_demographics_data <- passing_percents %>% 
  left_join(dem_data, by = c("school_code", "district_code")) 
full_demographics_data %>% 
  write_rds(path = "MassEducation/full_demographic_data.rds")

# My second to last shiny tab contains information regarding passing percents
# and school level characteristics. I chose to include spending, teacher salary,
# average class size, and grad percent. Other columns from my files were mostly
# redundant, like looking at average salary and also total money spent on
# salaries and the total number of teachers. After selecting just this data, I
# join it with the passing data and save the file to be used in the shiny app.

school_data <- all_data %>% 
  select(school_name, school_code, district_code, in_district_per_pupil_spending, 
         total_per_pupil_spending, grad_percent, avg_class_size, average_salary) %>% 
  distinct()

full_school_char_data <- passing_percents %>% 
  left_join(school_data, by = c("school_code", "district_code")) 

full_school_char_data %>% 
  write_rds(path = "MassEducation/school_data.rds")
