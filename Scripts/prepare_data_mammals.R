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



# Check correlation with average (as in Sanders & Nosofsky, 2020)
check_r <- mammalsf3 %>%
            select(ID, pair, response,mammal_knowledge) %>%
            group_by(pair) %>%
            mutate(avg_sim = mean(response,na.rm=TRUE)) %>%
            group_by(ID) %>%
            summarize(r = cor(avg_sim,response))

describe(check_r$r)

ggplot(check_r,aes(x = r)) +
  geom_histogram(bins=15,color="white") +
  theme_nice()


# Filter participants with r < .25
mammalsf4 <- mammalsf3 %>% filter(!ID %in% check_r[check_r$r<.25,]$ID)



# Check RT
ggplot(mammalsf4,aes(x = log(rt_ms))) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits=c(5,12), breaks=c(5:12)) +
  theme_nice()

# Filter participants with 10% trials below 400ms
mammalsf5 <- mammalsf4 %>% 
              group_by(ID) %>% 
              mutate(per_low  = mean(rt_ms< 400)) %>% 
              filter(per_low <= .10)

# Filter out AC trials
mammalsf6 <- mammalsf5 %>% 
              ungroup() %>% 
              filter(ID_trial != "AC")

# Filter out trials where participants pressed 0
mammalsf7 <- mammalsf6 %>% 
              ungroup() %>% 
              filter(response != 0)

# Filter out trials below 400ms
mammalsf8 <- mammalsf7 %>% 
              ungroup() %>% 
              filter(rt_ms >= 400)

# Filter out trials above 30000ms
mammalsf9 <- mammalsf8 %>% 
              ungroup() %>% 
              filter(rt_ms <= 30000)


# N progression
mammals0$ID  %>% n_distinct() # 734
mammalsf1$ID %>% n_distinct() # 682
mammalsf2$ID %>% n_distinct() # 680
mammalsf3$ID %>% n_distinct() # 679
mammalsf4$ID %>% n_distinct() # 672
mammalsf5$ID %>% n_distinct() # 671
mammalsf6$ID %>% n_distinct() # 671

mammalsf6 %>% nrow()          # 106176
mammalsf7 %>% nrow()          # 106057
mammalsf8 %>% nrow()          # 106010
mammalsf9 %>% nrow()          # 105695


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


write_csv2(mammals,file = "Data/2 Mammals/data_analysis_similarity_mammals.csv")

