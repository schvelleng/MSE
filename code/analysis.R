library(tidyverse)

df <- readxl::read_excel('data/analysis/collated_database.xlsx') 
ranking <- readxl::read_excel('data/rankings/2022_P6_Collated_Rankings.xlsx') %>% 
  janitor::clean_names()

# df_namelist <- df %>% 
#   group_by(name) %>% 
#   filter(n() > 1) %>% 
#   ungroup() %>% 
#   distinct(name, .keep_all =TRUE) 
#   arrange(academy)
# 

df_namelist <- df %>% 
  filter(year(test_date) == '2022')

# normalise rankings 
ranking <- ranking %>% 
  group_by(academy, gender) %>% 
  mutate(normalised_rank =  1 - ((relative_ranking - 1) / (n() - 1)))

str(df_namelist)
