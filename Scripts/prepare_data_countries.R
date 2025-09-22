# Load packages                   -----------------------------------------------------------

library(tidyverse)
library(psych)

source("Scripts/plot_settings.R")


# Load Data                       -----------------------------------------------------------

country0 <- read_csv2("Data/1 Countries/data_similarity_countries.csv")
items    <- read_csv2("Materials/stimlist_countries.csv") %>% select(ID,item)

# Initial Filtering               ---------------------------------------------------------------

# Filter out all participants who failed one or more attention check (AC) trials
# except IDs: k8krgrs74kzu, hj1v7f9ur49t, a6ef6170uqx7, 24of1eze6c79, 9fqsc2wpj75f,
#             i3h1lwbfiobx, ztey0vbi21y7, and lk7imglwomrw who directly
#             messaged via Prolific and explained/reported their mistake (with country seen and 
#             ~ trial number etc.)

countryf1 <- country0 %>% 
               group_by(ID) %>% 
               mutate(failed_AC = sum(response != 0 & ID_trial == "AC")) %>% 
               ungroup() %>% 
               filter(failed_AC == 0 | ID %in% c("k8krgrs74kzu","hj1v7f9ur49t","9fqsc2wpj75f",
                                                 "24of1eze6c79","a6ef6170uqx7","i3h1lwbfiobx",
                                                 "ztey0vbi21y7","lk7imglwomrw"))


# Filter out participants indicated that their data should not be used
countryf2 <- countryf1 %>% filter(quality == "yes")

# Filter out participants indicated that they cheated (used notes etc)
countryf3 <- countryf2 %>% filter(cheated == "no")




# Check correlation with average (as in Sanders & Nosofsky, 2020)
check_r <- countryf3 %>%
            select(ID, pair, response,country_knowledge) %>%
            group_by(pair) %>%
            mutate(avg_sim = mean(response,na.rm=TRUE)) %>%
            group_by(ID) %>%
            summarize(r = cor(avg_sim,response))

describe(check_r$r)

ggplot(check_r,aes(x = r)) +
  geom_histogram(bins=15,color="white") +
  theme_nice()


# Filter participants with r < .25
countryf4 <- countryf3 %>% filter(!ID %in% check_r[check_r$r<.25,]$ID)


# Check RT
ggplot(countryf4,aes(x = log(rt_ms))) +
  geom_histogram(bins=100) +
  scale_x_continuous(limits=c(5,12), breaks=c(5:12)) +
  theme_nice()

# Filter participants with 10% trials below 400ms
countryf5 <- countryf4 %>% 
              group_by(ID) %>% 
              mutate(per_low  = mean(rt_ms< 400)) %>% 
              filter(per_low <= .10)


# Filter out AC trials
countryf6 <- countryf5 %>% 
              ungroup() %>% 
              filter(ID_trial != "AC")

# Filter out trials where participants pressed 0
countryf7 <- countryf6 %>% 
              ungroup() %>% 
              filter(response != 0)


# Filter out trials below 400ms
countryf8 <- countryf7 %>% 
              ungroup() %>% 
              filter(rt_ms >= 400)

# Filter out trials above 30000ms
countryf9 <- countryf8 %>% 
              ungroup() %>% 
              filter(rt_ms <= 30000)




# N progression
country0$ID  %>% n_distinct() # 787
countryf1$ID %>% n_distinct() # 652
countryf2$ID %>% n_distinct() # 642
countryf3$ID %>% n_distinct() # 620
countryf4$ID %>% n_distinct() # 522
countryf5$ID %>% n_distinct() # 520

countryf6 %>% nrow()          # 82160
countryf7 %>% nrow()          # 82117
countryf8 %>% nrow()          # 82061
countryf9 %>% nrow()          # 81103


# Make Tidy DF                    ----------------------------------------------------------

country <- countryf9 %>%
              rename(sim     = response,
                     pair_ID = pair) %>% 
              mutate(ID_stim_1 = as.numeric(gsub("\\D", "", stim_1)),
                     ID_stim_2 = as.numeric(gsub("\\D", "", stim_2)),
                     dist      =  8-sim,
                     norm_dist = (dist-1)/(7-1),
                     norm_sim  = (1-norm_dist),
                     pair      = paste0(ID_stim_1,"_",ID_stim_2)) %>% 
              select(ID,n_trial,pair_ID,pair,ID_stim_1,ID_stim_2,rt_ms,sim,dist,
                     norm_dist,norm_sim,stim_1,stim_2,country_knowledge,age,gender,features)


# Add country names to the df

country <- left_join(country, items, join_by(ID_stim_1 == ID)) %>% 
            rename(country_stim_1 = item) %>% 
            left_join(., items, join_by(ID_stim_2 == ID)) %>% 
            rename(country_stim_2 = item) %>% 
            relocate(country_stim_1,country_stim_2,.before = rt_ms)

# Safe DF
write_csv2(country,file = "Data/1 Countries/data_analysis_similarity_country.csv")


