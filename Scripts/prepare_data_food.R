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


# Check correlation with average (as in Sanders & Nosofsky, 2020)
check_r <- foodf3 %>%
            select(ID, pair, response,food_knowledge) %>%
            group_by(pair) %>%
            mutate(avg_sim = mean(response,na.rm=TRUE)) %>%
            group_by(ID) %>%
            summarize(r = cor(avg_sim,response))

describe(check_r$r)

ggplot(check_r,aes(x = r)) +
  geom_histogram(bins=15,color="white") +
  theme_nice()


# Filter participants with r < .25
foodf4 <- foodf3 %>% filter(!ID %in% check_r[check_r$r<.25,]$ID)



# Check RT
ggplot(foodf4,aes(x = log(rt_ms))) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits=c(5,12), breaks=c(5:12)) +
  theme_nice()

# Filter participants with 10% trials below 400ms
foodf5 <- foodf4 %>% 
              group_by(ID) %>% 
              mutate(per_low  = mean(rt_ms< 400)) %>% 
              filter(per_low <= .10)

# Filter out AC trials
foodf6 <- foodf5 %>% 
              ungroup() %>% 
              filter(ID_trial != "AC")

# Filter out trials where participants pressed 0
foodf7 <- foodf6 %>% 
              ungroup() %>% 
              filter(response != 0)


# Filter out trials below 400ms
foodf8 <- foodf7 %>% 
              ungroup() %>% 
              filter(rt_ms >= 400)

# Filter out trials above 30000ms
foodf9 <- foodf8 %>% 
              ungroup() %>% 
              filter(rt_ms <= 30000)



# N progression
food0$ID  %>% n_distinct() # 757
foodf1$ID %>% n_distinct() # 649
foodf2$ID %>% n_distinct() # 645
foodf3$ID %>% n_distinct() # 643
foodf4$ID %>% n_distinct() # 608
foodf5$ID %>% n_distinct() # 607
foodf6$ID %>% n_distinct() # 607
foodf5 %>% nrow()          # 97727
foodf6 %>% nrow()          # 95906
foodf7 %>% nrow()          # 95857
foodf8 %>% nrow()          # 95778
foodf9 %>% nrow()          # 95393

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



write_csv2(food,file = "Data/3 Food/data_analysis_similarity_food.csv")


