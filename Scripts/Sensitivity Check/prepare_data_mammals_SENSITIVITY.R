# Load packages                   -----------------------------------------------------------

library(tidyverse)
library(psych)

source("Scripts/plot_settings.R")

# Load Data                       -----------------------------------------------------------

mammals0 <- read_csv2("Data/2 Mammals/data_similarity_mammals.csv")
items    <- read_csv2("Materials/stimlist_mammals.csv")  %>% select(ID,item)


# Initial Filtering               ---------------------------------------------------------------

# Filter out all participants who failed one or more attention check (AC) trials,
# except IDs: 3vdiatzu9or5, 6tq6a50n2pnm, 1e7qnf74lqgp, and j8v5n7osrlpl who directly
# message via Prolific and explained finger slip (with animal, ~ trial number etcs)

mammalsf1 <- mammals0 %>% 
              group_by(ID) %>% 
              mutate(failed_AC = sum(response != 0 & ID_trial == "AC")) %>% 
              ungroup() %>% 
              filter(failed_AC == 0 | ID %in% c("3vdiatzu9or5", "6tq6a50n2pnm",
                                                "1e7qnf74lqgp", "j8v5n7osrlpl"))


# Filter out participants indicated that their data should not be used
mammalsf2 <- mammalsf1 %>% filter(quality == "yes")

# Filter out participants indicated that they cheated (used notes etc)
mammalsf3 <- mammalsf2 %>% filter(cheated == "no")


mammalsf5 <- mammalsf3

# Filter out AC trials
mammalsf6 <- mammalsf5 %>% 
              ungroup() %>% 
              filter(ID_trial != "AC")

# Filter out trials where participants pressed 0
mammalsf7 <- mammalsf6 %>% 
              ungroup() %>% 
              filter(response != 0)

mammalsf9 <- mammalsf7 


# N progression
mammals0$ID  %>% n_distinct() # 734
mammalsf1$ID %>% n_distinct() # 682
mammalsf2$ID %>% n_distinct() # 680
mammalsf3$ID %>% n_distinct() # 679

mammalsf5 %>% nrow()          # 109480
mammalsf6 %>% nrow()          # 107440
mammalsf7 %>% nrow()          # 107312


# Make Tidy DF                    ----------------------------------------------------------


mammals <- mammalsf9 %>%
              rename(sim     = response,
                     pair_ID = pair) %>% 
              mutate(ID_stim_1 = as.numeric(gsub("\\D", "", stim_1)),
                     ID_stim_2 = as.numeric(gsub("\\D", "", stim_2)),
                     dist      =  8-sim,
                     norm_dist = (dist-1)/(7-1),
                     norm_sim  = (1-norm_dist),
                     pair      = paste0(ID_stim_1,"_",ID_stim_2)) %>% 
              select(ID,n_trial,pair_ID,pair,ID_stim_1,ID_stim_2,rt_ms,sim,dist,
                     norm_dist,norm_sim,stim_1,stim_2,mammal_knowledge,age,gender,features)



# Add food items names to the df
mammals <- left_join(mammals, items, join_by(ID_stim_1 == ID)) %>% 
            rename(mammal_stim_1 = item) %>% 
            left_join(., items, join_by(ID_stim_2 == ID)) %>% 
            rename(mammal_stim_2 = item) %>% 
            relocate(mammal_stim_1,mammal_stim_2,.before = rt_ms)


write_csv2(mammals,file = "Data/Sensitivity Checks/data_analysis_similarity_mammals_SENSITIVITY.csv")

