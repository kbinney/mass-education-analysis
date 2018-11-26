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
    "name" = school_name,
    "code" = school_code,
    "subject" = subject,
    "exceeds" = e_number,
    "met" = m_number,
    "partially" = pm_number,
    "not_meeting" = nm_number,
    "num_students" = no_of_students_included,
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
    num_students = parse_number(num_students)
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
    "name" = school_name,
    "code" = school_code,
    "subject" = subject,
    "exceeds" = a_number,
    "met" = p_number,
    "partially" = ni_number,
    "not_meeting" = w_f_number,
    "num_students" = student_included,
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
all_data <- bind_rows(mcas_data, mcas_old_data) %>% 
  mutate(subject = case_when(subject == "ELA" ~ "ENGLISH LANGUAGE ARTS",
                             subject == "MATH" ~ "MATHEMATICS",
                             TRUE ~ subject))

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
race_gender_enrollment <- read_excel("final_project_data/new_data/enrollmentbyracegender-school.xlsx", skip = 3) %>% 
  clean_names() %>% 
  rename(school_name = school, school_code = org_code)
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
         FTE_count_in_district = parse_number(total_in_district_ft_es),
         per_pupil_spending_in_district = parse_number(str_replace(in_district_expenditures_per_pupil, "$", "")),
         total_expenses = parse_number()
restraints <- read_excel("final_project_data/new_data/restraints.xlsx")
salaries <- read_excel("final_project_data/new_data/TeacherSalaries.xlsx")
pops <- read_excel("final_project_data/new_data/selectedpopulations.xls")

