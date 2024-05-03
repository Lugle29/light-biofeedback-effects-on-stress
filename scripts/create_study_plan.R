## ---------------------------
##
## Script name: create_study_plan.R
##
## Purpose of script: setting up an excel file for study implementation
##
## Author: Luis Glenzer
##
## Date Created: 2023-12-04
##
## ---------------------------

# Setup
library(openxlsx)

# Create a vector with both versions of Stdy part II
ver_calib <- c("4","6","8","10")
test_ver <- c('Ver_1','Ver_2')

# Shuffle the sequence
shuffle_version <- function(vec) {
  paste(sample(vec), collapse = '|')
}

# Set seed for reproducibility
set.seed(4938)

# Get calbiration sequences
shuffled_calib <- replicate(80, shuffle_version(ver_calib), simplify = FALSE)
unlist(shuffled_calib)

# Get version indication
test_ver <- rep(test_ver, 40)

# Shuffle ID-String
generate_random_string <- function(){
  letters1 <- sample(LETTERS, 2, replace = TRUE)
  letters2 <- sample(LETTERS, 2, replace = TRUE)
  numbers <- sample(0:9, 2, replace = TRUE)
  
  # Combine to String
  random_string <- paste0(letters1, numbers, letters2)
  random_string <- paste0(random_string[1], random_string[2])
  
  return(random_string)
}

# Set seed for reproducibility
set.seed(1357)

# Get ID column
ID <- replicate(80, generate_random_string())

# Add to dataframe
study_plan <- data.frame(No. = c(1:80),
                         ID = unlist(ID),
                         Mail = NA,
                         testing_date = NA,
                         testing_version = test_ver,
                         version_calib = unlist(shuffled_calib),
                         rmssd_4 = NA,
                         rmssd_6 = NA,
                         rmssd_8 = NA,
                         rmssd_10 = NA,
                         calib_frequency = NA,
                         calib_color = NA,
                         #version_stp_1 = unlist(shuffled_stp1),
                         #version_stp_2 = unlist(shuffled_stp2),
                         CPT_endurance = NA,
                         CPT_coping = NA)

# Check for multiple cases
nrow(unique(study_plan['ID'])) == 80

# Write as Excel file
write.xlsx(study_plan, 'study_plan.xlsx', rowNames = FALSE)


