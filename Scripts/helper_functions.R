
# Function to drop leading zeros
dropLeading0<- function(label){
  str_replace(label,"0(?=.)","")
}


# Function to compute summary statistics
demo_summary <- function(df) {
  
  # Total sample size
  total_sample <- nrow(df)
  
  # Compute mean and sd of age
  mean_age <- mean(df$age, na.rm = TRUE) 
  sd_age   <- sd(df$age, na.rm = TRUE)   
  min_age  <- min(df$age, na.rm = TRUE) 
  max_age  <- max(df$age, na.rm = TRUE) 
  
  
  # Percentage male and female
  percent_male       <- mean(df$gender == "male", na.rm = TRUE) * 100
  percent_female     <- mean(df$gender == "female", na.rm = TRUE) * 100
  percent_non_binary <- mean(df$gender == "non_binary", na.rm = TRUE) * 100
  
  
  # Return a named list with the demographic summary
  ds <- data.frame(
    total_sample_size  = total_sample,
    mean_age           = papaja::printnum(mean_age),
    sd_age             = papaja::printnum(sd_age),
    min_age            = min_age,
    max_age            = max_age,
    percent_male       = papaja::printnum(percent_male, digits = 1),
    percent_female     = papaja::printnum(percent_female, digits = 1),
    percent_non_binary = papaja::printnum(percent_non_binary, digits = 1)
  )
  
  
  return(ds)
}


# Print demographics

