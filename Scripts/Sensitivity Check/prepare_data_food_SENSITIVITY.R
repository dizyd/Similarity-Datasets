# Load packages                   -----------------------------------------------------------

library(tidyverse)
library(psych)

source("Scripts/plot_settings.R")

# Load Data                       -----------------------------------------------------------

food0 <- read_csv2("Data/3 Food/data_similarity_food.csv")
items <- read_csv2("Materials/stimlist_foods.csv")

# Initial Filtering               ---------------------------------------------------------------

# Filter out all participants who failed one or more attention check (AC) trials
# except IDs: iga5cl2o79d2,51wswuft7tc0 and 4v8iv5pzq6l1 who directly
# message via Prolific and explained finger slip (with exact food item, ~ trial number etc.)

foodf1 <- food0 %>% 
              group_by(ID) %>% 
              mutate(failed_AC = sum(response != 0 & ID_trial == "AC")) %>% 
              ungroup() %>% 
              filter(failed_AC == 0 | ID %in% c("3vdiatzu9or5", "6tq6a50n2pnm",
                                                "1e7qnf74lqgp", "j8v5n7osrlpl"))


# Filter out participants indicated that their data should not be used
foodf2 <- foodf1 %>% filter(quality == "yes")

# Filter out participants indicated that they cheated (used notes etc)
foodf3 <- foodf2 %>% filter(cheated == "no")


foodf5 <- foodf3

# Filter out AC trials
foodf6 <- foodf5 %>% 
              ungroup() %>% 
              filter(ID_trial != "AC")

# Filter out trials where participants pressed 0
foodf7 <- foodf6 %>% 
              ungroup() %>% 
              filter(response != 0)


foodf9 <- foodf7


# N progression
food0$ID  %>% n_distinct() # 757
foodf1$ID %>% n_distinct() # 649
foodf2$ID %>% n_distinct() # 645
foodf3$ID %>% n_distinct() # 643

foodf5 %>% nrow()          # 103523
foodf6 %>% nrow()          # 101594
foodf7 %>% nrow()          # 101515

# Make Tidy DF                    ----------------------------------------------------------

food <- foodf9 %>%
              rename(sim     = response,
                     pair_ID = pair) %>% 
              mutate(ID_stim_1 = as.numeric(gsub("\\D", "", stim_1)),
                     ID_stim_2 = as.numeric(gsub("\\D", "", stim_2)),
                     dist      =  8-sim,
                     norm_dist = (dist-1)/(7-1),
                     norm_sim  = (1-norm_dist),
                     pair      = paste0(ID_stim_1,"_",ID_stim_2)) %>% 
              select(ID,n_trial,pair_ID,pair,ID_stim_1,ID_stim_2,rt_ms,sim,dist,
                     norm_dist,norm_sim,stim_1,stim_2,food_knowledge,age,gender,features)


# Add food items names to the df
food <- left_join(food, items, join_by(ID_stim_1 == ID)) %>% 
              rename(food_stim_1 = item) %>% 
              left_join(., items, join_by(ID_stim_2 == ID)) %>% 
              rename(food_stim_2 = item) %>% 
              relocate(food_stim_1,food_stim_2,.before = rt_ms)



write_csv2(food,file = "Data/Sensitivity Checks/data_analysis_similarity_food_SENSITIVITY.csv")


