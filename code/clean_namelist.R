library(tidyverse)

df_2023 <- readxl::read_excel('data/MSE TESTING RESULTS_Masterlist.xlsx', 
                              sheet = 'All') %>% 
  janitor::clean_names() %>% 
  mutate(name = str_to_title(name),
         academy = str_to_title(academy)) %>% 
  mutate(academy = str_replace(academy, regex("\\bip\\b", 
                               ignore_case = TRUE),
                               "IP")) %>% 
  mutate(academy = case_when(academy == 'Track And Field' ~ 'Track & Field',
                             TRUE ~ academy))

df_2024_sec2 <- readxl::read_excel('data/MSE 2024_Testing for Sec 2.xlsx') %>% 
  janitor::clean_names() %>% 
  mutate(academy = case_when(academy == 'Track and Field' ~ 'Track & Field',
                             TRUE ~ academy))

df_2024_sec4 <- readxl::read_excel('data/SSP Sec 4_Testing 2024.xlsx') %>% 
  janitor::clean_names() %>% 
  mutate(gender = case_when(gender== "Male" ~ 'M',
                            gender == 'Female' ~ 'F'))

#names(df_2024_sec4)
#str(df_2024_sec4)

df_2024_sec2 <- df_2024_sec2 %>% 
  mutate(across(height:best_hg, as.numeric)) %>% 
  mutate(across(c(gender, class, academy), as.factor))

df_2024_sec4 <- df_2024_sec4 %>% 
  mutate(across(height:best_wt, as.numeric)) %>% 
  mutate(across(c(gender, class, academy), as.factor))
   
df_2024_names<- bind_rows(df_2024_sec2, df_2024_sec4) %>% 
  select(test_date, name, gender, class, academy)

df_2023_names <- df_2023 %>% 
  select(test_date, name, gender, class, academy)

df_names <- bind_rows(df_2023_names, df_2024_names) %>% 
  arrange(name) %>% 
  mutate(name = name %>% 
           str_remove("\\s*\\(.*\\)") %>% # remove anything in parenthesis
           str_replace(',' , '') %>%  # remove commas
           str_squish())  # remove spaces 

duplicate_names <- df_names %>% 
  select(name) %>% 
  mutate(name_words = str_split(name,  "\\s+"), # return list of each word
         name_words_sorted = map(name_words, ~sort(.x)), # apply sort function
         name = map_chr(name_words_sorted, ~paste(.x, collapse = " "))) %>% # applies paste function while removing spaces
  arrange(name) %>% 
  select(-name_words, -name_words_sorted )

df_names_cleaned <- df_names %>% 
  group_by(name) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  distinct(name, .keep_all =TRUE) %>% 
  select(-class, -test_date) %>% 
  arrange(academy)


writexl::write_xlsx(df_names_cleaned, 'data/namelist.xlsx')



