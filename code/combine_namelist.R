## COMPILE ALL DATA INTO ONE

library(tidyverse)
library(arsenal)


# Set the directory that raw files are stored in 
fnames <- dir("data", pattern = ".xlsx", full.names = T)


### COLS NEEDED
target_cols <- c(
  "test_date", "dob", "name", "gender", "academy", "age", "class", "height", "weight", "arm_span", "seated_height",
  "x20m_1", "x20m_2", "x30m_1", "x30m_2", "x505_1", "x505_2", "reach_height", "raw_vj_1", "raw_vj_2", "hg_1", "hg_2", "bb_6cm_1",
  "bb_6cm_2", "bb_6cm_3", "bb_4_5cm_1", "bb_4_5cm_2", "bb_4_5cm_3", "bb_3cm_1", "bb_3cm_2", "bb_3cm_3", "ms_1",
  "ms_2", "js_1", "js_2", "wall_toss_1", "wall_toss_2", "left_front_1", "right_front_1", "left_away_1",
  "right_away_1", "left_x_1", "right_x_1", "left_front_2", "right_front_2", "left_away_2", "right_away_2",
  "left_x_2", "right_x_2", "mbt_1",  "mbt_2", "remarks") 
  
### clean columns function (set to same cols for all df)
clean_columns <- function(data){
  missing_cols <- setdiff(target_cols, names(data))
  data[missing_cols] <- NA
  
  new_df <- data %>%
    select(any_of(target_cols))

  return(new_df)
}


### import file, clean name and save into list function
import_files <- function(file){
  
  name <- tools::file_path_sans_ext(basename(file))
  
  df_original <- readxl::read_excel(file) %>% 
    janitor::clean_names()
  
  
  df_cleaned <- clean_columns(df_original)
  
 return(df_cleaned)
  
}

# apply to all files in data directory
df <- lapply(fnames, import_files )

# save as dataframe
df <- as.data.frame(data.table::rbindlist(df))


### DATA CLEANING 

# clean academies
df_cleaned <- df %>% 
  mutate(academy = recode(academy,
                          "SWM" = "Swimming",
                          "FEN" = "Fencing",
                          "NET" = "Netball",
                          "SHT" = "Shooting",
                          "TNF" = "Track & Field",
                          "FTB" = "Football",
                          "BDM" = "Badminton",
                          "BWL" = "Bowling",
                          "TTS"  = "Table Tennis", 
                          "MSA" = "Multi-Sport", 
                          "IND - SLT" = "IP - SILAT",
                          "IND - AQT - WPO" = "IP - WATER POLO",
                          "IND - GYM" = "IP - GYMNASTICS",
                          "IND - TKD" =  "IP - TAEKWONDO",
                          "IND - WSH" = "IP - WUSHU",
                          "IND - FBG" = "IP - FBG",
                          "IND - GOLF" = "IP - GOLF",
                          "IND-AQT" = "IP - AQT",
                          "IND - ROLL" = "IP - ROLLERSPORTS",
                          "IND - SKT" = "IP - SKATING",
                          "IND - SIL"  = "IP - SILAT",
                          "IND-FBG"  =  "IP - FBG",
                          "IND-AQT - WPO" = "IP - WATER POLO",
                          "IND - TKW" = "IP - TAEKWONDO",
                          "IND - WUSH" =  "IP - WUSHU",
                          "SLT" = "IP - SILAT",
                          "IP -GYMNASTICS" = "IP - GYMNASTICS",
                          "WATER POLO"  = "IP - WATER POLO",
                          "CLB" = "IP - SPORT CLIMBING",
                          "GYM" = "IP - GYMNASTICS",
                          "TRACK AND FIELD" = "Track & Field",
                          "Track and Field"  = "Track & Field",  
                          "IP-Aquatics (Diving)" = 'IP - Diving',
                          "IP - FOOTBALL" =  "IP - FBG",
                          "Individual Programme - Silat" = "IP - SILAT"
                          )) %>% 
  mutate(academy = str_to_title(academy)) 


# clean gender
df_cleaned <- df_cleaned %>% 
  mutate(gender = recode(gender,
                         'Male' = 'M',
                         'Female' = 'F'))

# clean name
df_cleaned <- df_cleaned %>% 
  mutate(name = str_to_title(name))  %>%
  mutate(name = case_when(str_starts(name, '/') ~ name %>% # only apply to those with '/'
                            str_remove("^/") %>%  # remove '/'
                            str_replace_all('_',' ') %>%  # replace underscore with space
                            str_to_title(), # title case
                          TRUE ~ name)) %>% 
  mutate(name = name %>% 
           str_remove("\\s*\\(.*\\)") %>% # remove anything in parenthesis
           str_replace(',' , '') %>%  # remove commas
           str_squish() %>% 
           str_remove('@'))

# clean data type
df_cleaned <- df_cleaned %>% 
  mutate(across(height:mbt_2, as.numeric)) %>% 
  mutate(across(c(gender, class, academy), as.factor))

# str(df_cleaned)

# add in data for best tests
df_cleaned <- df_cleaned %>% 
  mutate(leg_length = height - seated_height + 38, #platform length
         x20m = case_when(x20m_1 == 0 | x20m_2 == 0 ~ pmax(x20m_1, x20m_2, na.rm=TRUE), 
                          TRUE ~ pmin(x20m_1, x20m_2, na.rm=TRUE)), 
         x30m = case_when(x30m_1 == 0 | x30m_2 == 0 ~ pmax(x30m_1, x30m_2, na.rm=TRUE), 
                          TRUE ~ pmin(x30m_1, x30m_2, na.rm=TRUE)), 
         x505 = case_when(x505_1 == 0 | x505_2 == 0 ~ pmax(x505_1, x505_2,na.rm=TRUE),
                          TRUE ~ pmin(x505_1, x505_2,na.rm=TRUE)),
         VJ = (pmax(raw_vj_1, raw_vj_2, na.rm=TRUE) - reach_height), 
         total_BB = rowSums(select(., bb_6cm_1:bb_3cm_3)), 
         total_MS = rowSums(across(c(ms_1, ms_2))),
         total_JS = rowSums(across(c(js_1, js_2))),
         wall_toss = pmax(wall_toss_1, wall_toss_2,na.rm=TRUE), 
         hand_grip = pmax(hg_1, hg_2,na.rm=TRUE), 
         y_bal_left = (((pmax(left_front_1, left_front_2,na.rm=TRUE) + pmax(left_away_1, left_away_2,na.rm=TRUE) +
                           pmax(left_x_1, left_x_2,na.rm=TRUE)) / (3*leg_length))) * 100,
         y_bal_right = (((pmax(right_front_1, right_front_1,na.rm=TRUE) + pmax(right_away_1, right_away_2,na.rm=TRUE) +
                            pmax(right_x_1, right_x_2,na.rm=TRUE)) / (3*leg_length))) * 100,
         y_bal_avg = (y_bal_left + y_bal_right) / 2,
         mbt = pmax(mbt_1, mbt_2, na.rm = TRUE)) %>% 
  mutate(VJ = case_when(reach_height > pmax(raw_vj_1, raw_vj_2, na.rm=TRUE) ~ reach_height - pmin(raw_vj_1, raw_vj_2, na.rm=TRUE),
                        TRUE ~ VJ)) # account for error in record taking for 2022 batch

  
# fill in DOB if have data but missing and update chrono age
df_cleaned <- df_cleaned %>% 
  group_by(name) %>% 
  mutate(dob = case_when(is.na(dob) ~ first(dob),
                         TRUE ~ dob)) %>% 
  ungroup() %>% 
  mutate(age = lubridate::time_length(interval(dob, test_date), unit = "years")) 


summary(df_cleaned)
  


### SAVE DF INTO NEW COLLATED DATABASE

writexl::write_xlsx(df_cleaned, 'data/analysis/collated_database.xlsx')



