df_food    <- read_csv2("../Data/3 Food/data_analysis_similarity_food.csv") 
df_country <- read_csv2("../Data/1 Countries/data_analysis_similarity_country.csv") 
df_mammal  <- read_csv2("../Data/2 Mammals/data_analysis_similarity_mammals.csv") 


# Load stimulus List

stims_food    <- read_csv2("../Materials/stimlist_foods.csv")
stims_country <- read_csv2("../Materials/stimlist_countries.csv") |> rename(category = continent)
stims_mammal  <- read_csv2("../Materials/stimlist_mammals.csv") 



# Replace German with English names

# Join for ID_stim_1
df_mammal <- df_mammal %>% left_join(stims_mammal %>% select(ID_stim_1 = ID, item_1 = item), by = "ID_stim_1")

# Join for ID_stim_2
df_mammal <- df_mammal %>% left_join(stims_mammal %>% select(ID_stim_2 = ID, item_2 = item), by = "ID_stim_2")

# Replace German names with English names
df_mammal <- df_mammal %>%
  mutate(
    mammal_stim_1 = item_1,
    mammal_stim_2 = item_2
  ) %>%
  select(-item_1, -item_2)


# Join for ID_stim_1
df_country <- df_country %>% left_join(stims_country %>% select(ID_stim_1 = ID, item_1 = item), by = "ID_stim_1")

# Join for ID_stim_2
df_country <- df_country %>% left_join(stims_country %>% select(ID_stim_2 = ID, item_2 = item), by = "ID_stim_2")

# Replace German names with English names
df_country <- df_country %>%
  mutate(
    country_stim_1 = item_1,
    country_stim_2 = item_2
  ) %>%
  select(-item_1, -item_2)



# Join for ID_stim_1
df_food <- df_food %>% left_join(stims_food %>% select(ID_stim_1 = ID, item_1 = item), by = "ID_stim_1")

# Join for ID_stim_2
df_food <- df_food %>% left_join(stims_food %>% select(ID_stim_2 = ID, item_2 = item), by = "ID_stim_2")

# Replace German names with English names
df_food <- df_food %>%
  mutate(
    food_stim_1 = item_1,
    food_stim_2 = item_2
  ) %>%
  select(-item_1, -item_2)



write_csv2(df_mammal, "../Data/2 Mammals/data_analysis_similarity_mammals.csv") 
write_csv2(df_country, "../Data/1 Countries/data_analysis_similarity_country.csv")
write_csv2(df_food, "../Data/3 Food/data_analysis_similarity_food.csv") 
