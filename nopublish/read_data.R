library(tidyverse)
# library(ggthemes)
# library(gt)
# library(ggtext)

# Training/Certification Data ----

data_traincert <- read_csv("C:/Users/jnese/Desktop/BRT/GRANT-CORE_II/project/data/human-prosody_study/audio_files_pull/data/project_product_data/raters/data_trainingcertification.csv")

write_csv(data_traincert, here::here("nopublish", "data_trainingcertification.csv"))

# Final Rating Data ----

data_ratings <- read_csv("C:/Users/jnese/Desktop/BRT/GRANT-CORE_II/project/data/human-prosody_study/audio_files_pull/data/project_product_data/data_ratings_final.csv")

write_csv(data_ratings, here::here("nopublish", "data_ratings_final.csv"))


# Rater Database ----

data_raterdatabase <- read_csv("C:/Users/jnese/Desktop/BRT/GRANT-CORE_II/project/data/human-prosody_study/audio_files_pull/data/project_product_data/raters/data_raterdatabase.csv")
 
write_csv(data_raterdatabase, here::here("nopublish", "data_raterdatabase.csv"))

# **NEED TO FINALIZE::::Audio File Directory ----

dir <- read_csv("W:/corestorage/prosody-study_phase1/audiofile_directory_phase1_v4.csv")

write_csv(dir, here::here("nopublish", "audiofile_directory_phase1_v4.csv") )

##easyCBM demographics ------------------------

####-- Year 4 ----

# Function to read and clean year 4 easyCBM data 
demoyr4_fx <- function(f){
  tibble(
    file = filesyr4_cpsd6[1],
    season = str_extract(file, "(?<=2018_)(.*?)(?=.csv)"),
    district = str_extract(file, "(?<=easyCBM_data/)(.*?)(?=_)"),
    data = map(file, read_csv)) %>% 
    select(-file) %>% 
    mutate(data = map(data,
                      ~janitor::clean_names(.) %>%
                        mutate(student_grade = as.numeric(student_grade)) %>% 
                        filter(., student_grade >= 2, student_grade <= 4) %>% 
                        select(last, first, easycbm_id = student_id, student_grade, student_dob, student_gender, student_ethnicity, student_race) %>% 
                        mutate_at(vars(last, first), ~str_to_lower(.)))) %>% 
    unnest(cols = c(data)) %>% 
    distinct(last, first, .keep_all = TRUE) %>% 
    select(-season)
}

# List all easyCBM files to read (fall, winter, spring)
filesyr4_cpsd6 <- list.files("W:/datastore/COREfiles/CORE_Year4/2017-18_easyCBM_data/cpsd6_or", pattern = "Benchmark", full.names = TRUE)
filesyr4_shelton <- list.files("W:/datastore/COREfiles/CORE_Year4/2017-18_easyCBM_data/shelton_wa", pattern = "Benchmark", full.names = TRUE)
filesyr4_slane <- list.files("W:/datastore/COREfiles/CORE_Year4/2017-18_easyCBM_data/slane_or", pattern = "Benchmark", full.names = TRUE)

# Apply function
demoyr4_cpsd6 <- demoyr4_fx(filesyr4_cpsd6)
demoyr4_shelton <- demoyr4_fx(filesyr4_shelton)
demoyr4_slane <- demoyr4_fx(filesyr4_slane)

demosyr4_easycbm <- bind_rows(demoyr4_cpsd6, demoyr4_shelton, demoyr4_slane)

####-- Year 5 ----

# Function to read and clean year 4 easyCBM data 
demoyr5_fx <- function(f){
  tibble(file = f,
         season = str_extract(file, "(?<=2019_)(.*?)(?=.csv)"),
         district = str_extract(file, "(?<=easycbm_1819/)(.*?)(?=_)"),
         data = map(f, read_csv)) %>%
    select(-file) %>% 
    mutate(data = map(data,
                      ~janitor::clean_names(.) %>%
                        mutate(student_grade = as.numeric(student_grade)) %>% 
                        filter(., student_grade >= 2, student_grade <= 4) %>% 
                        select(last, first, easycbm_id = student_id, student_grade, student_dob, student_gender, student_ethnicity, student_race) %>% 
                        mutate_at(vars(last, first), ~str_to_lower(.)))) %>% 
    unnest(cols = c(data)) %>% 
    distinct(last, first, .keep_all = TRUE) %>% 
    select(-season)
}

# List all easyCBM files to read (fall, winter, spring)
filesyr5_slane <- list.files("W:/datastore/COREfiles/CORE_Year5/easycbm_1819/slane_or", full.names = TRUE)
filesyr5_sps <- list.files("W:/datastore/COREfiles/CORE_Year5/easycbm_1819/sps_or", full.names = TRUE, pattern = "copy")
filesyr5_cp <- list.files("W:/datastore/COREfiles/CORE_Year5/easycbm_1819/cpsd6_or", full.names = TRUE)
filesyr5_shelton <- list.files("W:/datastore/COREfiles/CORE_Year5/easycbm_1819/shelton_wa", full.names = TRUE, pattern = "copy")

# Apply function
demoyr5_slane <- demoyr5_fx(filesyr5_slane) %>% 
  mutate(student_ethnicity = as.character(student_ethnicity),
         student_race = as.character(student_race))
demoyr5_sps <- demoyr5_fx(filesyr5_sps)
demoyr5_cp <- demoyr5_fx(filesyr5_cp)
demoyr5_shelton <- demoyr5_fx(filesyr5_shelton)

demosyr5_easycbm <- bind_rows(demoyr5_slane, demoyr5_sps, demoyr5_cp, demoyr5_shelton)

demos_easycbm <- bind_rows(mutate(demosyr4_easycbm, year = 4), mutate(demosyr5_easycbm, year = 5)) %>% 
  mutate(
    student_ethnicity = case_when(
      student_ethnicity == "1" | student_ethnicity == "N" | str_detect(student_ethnicity, "No") ~ "Not Hispanic/Latino",
      student_ethnicity == "4" | student_ethnicity == "Y" | student_ethnicity == "Hispanic" | student_ethnicity == "Hispanic/Latino" ~ "Hispanic/Latino"),
    student_race = case_when(
      student_race == "1" | str_detect(student_race, "Indian") ~ "American Indian/Native Alaskan",
      student_race == "2" | str_detect(student_race, "Asian") ~ "Asian",
      student_race == "2 or more races" | str_detect(student_race, "Two or") | str_detect(student_race, "Multi") ~ "Multi-Racial",
      student_race == "3" | str_detect(student_race, "African") | str_detect(student_race, "Black") ~ "Black/African American",
      student_race == "5" | str_detect(student_race, "White") ~ "White",
      student_race == "7" | str_detect(student_race, "Hawaiian") ~ "Native Hawaiian/Other Pacific Islander",
      TRUE ~ student_race)
)


write_csv(demos_easycbm, here::here("nopublish", "demos_easycbm.csv"))

##




  