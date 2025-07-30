

library(tidyverse)


# handle cases where "tan ah meng" and "ah meng" is the same person

# sort words first
name_map <- df_cleaned %>% 
  mutate(name_words = str_split(name,  "\\s+"), # return list of each word, split on the spaces
         name_words_sorted = map(name_words, ~sort(.x)), # apply sort function
         name_key = map_chr(name_words_sorted, ~paste(.x, collapse = " "))) # applies paste function to the vector of words while removing spaces %>% 


final_df <- name_map %>% 
  group_by(name) %>% 
  filter(n() > 2) %>% 
  ungroup()

