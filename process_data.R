library(tidyverse)
library(leaflet)
library(rgdal)
library(readxl)
library(janitor)
library(knitr)
library(fs)

filenames <- dir_ls("final_project_data/new_data/") %>% 
  str_subset("mcas-grade")

mcas_data <- map_dfr(filenames, read_xlsx, skip = 1, .id = "file_index") %>% 
  clean_names() %>% 
  transmute("name" = school_name, "code" = school_code, "subject" = subject, "exceeds" = e_number,
            "met" = m_number, "partially" = pm_number, "not_meeting" = nm_number,
            "num_students" = no_of_students_included, "file_index" = file_index) %>% 
  # For some reason, map_dfr with .id is giving list index rather than name
  # so we manually convert to path name. We can then parse out the grade from
  # the file name
  mutate(file_index = parse_number(file_index),
         source = filenames[file_index],
         grade = str_remove(source, "final_project_data/new_data/mcas-grade-"),
         grade = str_remove(grade, ".xlsx"),
         grade = parse_number(grade)) %>% 
  # Most of the columns were assumed characters, so we switch to parse as ints
  mutate(exceeds = parse_number(exceeds),
         met = parse_number(met),
         partially = parse_number(partially),
         not_meeting = parse_number(not_meeting),
         num_students = parse_number(num_students)) %>% 
  # We no longer need the index or source
  select(everything(), -file_index, -source)

# High school science and 10th grade mcas data files have a different format, so we read in
# separately before joining
special_filenames <- c("final_project_data/new_data/grade-10-reg.xlsx",
                       "final_project_data/new_data/hs-science.xlsx")  

# These files ave different format partially because the tests use an old scoring system. I 
# convert for joining, and will note this conversion on my app
mcas_old_data <- map_dfr(special_filenames, read_xlsx, skip = 1, .id = "file_index") %>% 
  clean_names() %>% 
  transmute("name" = school_name, "code" = school_code, "subject" = subject, "exceeds" = a_number,
            "met" = p_number, "partially" = ni_number, "not_meeting" = w_f_number,
            "num_students" = student_included, "file_index" = file_index)

