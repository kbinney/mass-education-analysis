library(tidyverse)
library(leaflet)
library(rgdal)
library(readxl)
library(janitor)
library(knitr)
library(fs)

# Downloading data from the massachusetts department of education is a bit
# tricky. To get different grade levels, I ahd to download different files.
# Luckily these files are mostly formatted the same, so I can read them in with
# a map. The column names aren't very informative, but research showed they were
# the different test levels, so I looked up the names for each abbreviation and
# renamed the columns.
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
  # For some reason, map_dfr with .id is giving list index rather than name
  # so we manually convert to path name. We can then parse out the grade from
  # the file name
  mutate(
    file_index = parse_number(file_index),
    source = filenames[file_index],
    grade = str_remove(source, "final_project_data/new_data/mcas-grade-"),
    grade = str_remove(grade, ".xlsx")
  ) %>%
  # Most of the columns were assumed characters, so we switch to parse as ints
  mutate(
    exceeds = parse_number(exceeds),
    met = parse_number(met),
    partially = parse_number(partially),
    not_meeting = parse_number(not_meeting),
    num_students_testing = parse_number(num_students_testing)
  ) %>%
  # We no longer need the index or source
  select(everything(),-file_index,-source)

# High school science and 10th grade mcas data files have a different format, so
# we read in separately before joining
special_filenames <- c("final_project_data/new_data/grade-10-reg.xlsx",
                       "final_project_data/new_data/hs-science.xlsx")  

# These files ave different format partially because the tests use an old
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
    source = filenames[file_index],
    grade = str_remove(source, "final_project_data/new_data/"),
    grade = str_remove(grade, ".xlsx"),
    grade = case_when(grade == "grade-10-reg" ~ "10",
                      TRUE ~ "HS")
  ) %>%
  # We no longer need the index or source
  select(everything(), -file_index, -source)

# We join old and new data into a single data frame. The old and new mcas have
# different notation for test types, so I rename some of the subject variables
all_test_data <- bind_rows(mcas_data, mcas_old_data) %>% 
  mutate(subject = case_when(subject == "ELA" ~ "ENGLISH LANGUAGE ARTS",
                             subject == "MATH" ~ "MATHEMATICS",
                             TRUE ~ subject)) %>% 
  # The data as is has absolute numbers of students at each level. Let's change
  # to do percent of students
  mutate(exceeds_percent = exceeds / num_students_testing,
         met_percent = met / num_students_testing,
         partially_percent = partially / num_students_testing,
         not_meeting_percent = not_meeting / num_students_testing)

# After getting all the data for MCAS by school and grade level, we read in
# other information about schools. Utimately, we will join this into a single
# data frame
class_size_gender <- read_excel("final_project_data/new_data/ClassSizebyGenPopulation.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(number_classes = parse_number(total_number_of_classes),
         avg_class_size = parse_number(average_class_size),
         num_students = parse_number(number_of_students),
         female_percent = parse_number(female_percent),
         ell_percent = parse_number(english_language_learner_percent),
         # The students wit disabilities column has a few strange inputs, which I interpret as NA
         swd_percent = parse_number(students_with_disabilities_percent, na = c("", "NA", "#####")),
         ec_disad_percent = parse_number(economically_disadvantaged_percent)) %>% 
  select(school_name,
         school_code,
         number_classes,
         avg_class_size,
         num_students,
         ell_percent,
         swd_percent,
         ec_disad_percent)
race_gender_enrollment <- read_excel("final_project_data/new_data/enrollmentbyracegender-school.xlsx", skip = 3, trim_ws = TRUE) %>% 
  clean_names() %>% 
  rename(school_name = school, school_code = org_code) %>% 
  # The school code has an extra space to start. We need to remove to later join the data set
  mutate(school_code = str_trim(school_code))
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
  select(school_name,
         school_code,
         cohort_size,
         grad_percent,
         in_school_percent,
         complete_non_grad_percent,
         hs_equiv_percent,
         drop_out_percent,
         excluded_percent)

# Spending is by district rather than school, which needs to be kep in mind when
# joining to school level data. Money columns are stored with $ signs, which
# need to be removed before we can parse teh column as a number. Cleaning names
# here helps some, but many of the columns are not clear, so I rename and remove
# an extra column.
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

# Restraints includes both school and district codes, which will be great for joining all types of data
restraints <- read_excel("final_project_data/new_data/restraints.xlsx", skip = 1) %>% 
  clean_names() %>% 
  rename(students_restrained = number_of_students_restrained,
         total_restraints = total_number_of_restraints,
         total_injuries = total_number_of_injuries) %>% 
  # Many rows include a -, I assumed this meant NA 
  mutate(students_restrained = parse_number(students_restrained, na = c("", "NA", "-")),
         total_restraints = parse_number(total_restraints, na = c("", "NA", "-")),
         total_injuries = parse_number(total_injuries, na = c("", "NA", "-"))) %>% 
  select(everything(), -district_name)
salaries <- read_excel("final_project_data/new_data/TeacherSalaries.xlsx", skip = 1) %>% 
  clean_names() %>% 
  mutate(salary_totals = parse_number(str_replace(salary_totals, "$", "")),
         average_salary = parse_number(str_replace(average_salary, "$", "")),
         fte_count = parse_number(fte_count))
pops <- read_excel("final_project_data/new_data/selectedpopulations.xlsx", skip = 3) %>% 
  clean_names() %>% 
  # The data spreadsheet has one row with section names, and one with % vs. #
  # I keep just the numbers, except use ell_percent to determine total students
  transmute(school_name = school,
         school_code = orgcode,
         non_native_english = first_language_not_english,
         ell_students = english_language_learner,
         swd = students_with_disabilities,
         high_needs,
         high_needs_percent = x_4,
         economically_disadvantaged) %>% 
  # We need to remove the row of % and #
  slice(2:n()) %>% 
  mutate(non_native_english = parse_number(non_native_english),
         ell_students = parse_number(ell_students),
         swd = parse_number(swd),
         high_needs = parse_number(high_needs),
         high_needs_percent = parse_number(high_needs_percent),
         economically_disadvantaged = parse_number(economically_disadvantaged)) %>% 
  # Finally, we used ell_percent and ell_students to determine the total number of studnts
  mutate(total_students = high_needs / (high_needs_percent / 100))

# data frames are different sizes:
# MCAS: 1638 unique schools (school)
# class_size_gender: 1850 (school)
# race_gender_enrollment: 1848 (school)
# grad_rates: 384 (high school)
# spending: 323 (district)
# restraints: 911 (district, school)
# pops: 1848 (school)
# salaries: 324 (district)
## After reading in all demographic data, I want to join it into one large data frame with the mcas data
# Some data frames are by school and some by district. I start with joining all schools and all districts
school_data <- all_test_data %>% 
  left_join(class_size_gender) %>% 
  left_join(race_gender_enrollment) %>% 
  left_join(grad_rates) %>% 
  left_join(restraints) %>% 
  left_join(pops) %>% 
  separate(school_code, into = c("district_code", "school_code"), sep = 4)

district_data <- salaries %>% 
  left_join(spending) %>% 
  mutate(district_code = str_sub(district_code, 1, 4))

# DoE says that the school code's first 4 digits is the district code. Let's see if that works
all_data <- school_data %>% 
  left_join(district_data)

## Save to be used in shiny app
write_rds(all_data, path = "MassEducation/joined_data")