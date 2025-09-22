df_c <- read_csv("../Data/Judgment Data/data_wide_judgment_countries.csv")
df_f <- read_csv("../Data/Judgment Data/data_wide_judgment_food.csv")
df_m <- read_csv("../Data/Judgment Data/data_wide_judgment_mammals.csv")



df_c <- df_c |> 
          pivot_longer(cols=n11xewpgbxj4:vzgw3yufjpr4,names_to = "ID",values_to = "est") |> 
          select(-img)  |> 
          add_column(domain = "Countries")

df_f <- df_f |> 
          pivot_longer(cols=qza2yw7qkddd:j4tufliz4ct6,names_to = "ID",values_to = "est") |> 
          select(-img)  |> 
          add_column(domain = "Food")

df_m <- df_m |> 
          pivot_longer(cols=chp46vl70814:u13d9hi208ge,names_to = "ID",values_to = "est") |> 
          select(-img) |> 
          add_column(domain = "Mammals")



df <- bind_rows(df_c,df_f,df_m) 


write_csv(df,"../Data/estimation_data.csv")


