# Setup
library(openxlsx)

# Create a vector with both versions of Stdy part II

ver_calib <- c("4","6","8","10")
test_ver <- c('Ver_1','Ver_2')
# ver_stp1 <- c('feedback', 'sitting_break')
# ver_stp2 <- c('feedback', 'roomlight')
# 
# Shuffle the sequence
shuffle_version <- function(vec) {
  paste(sample(vec), collapse = '|')
}
set.seed(4938)
shuffled_calib <- replicate(80, shuffle_version(ver_calib), simplify = FALSE)
unlist(shuffled_calib)
test_ver <- rep(test_ver, 40)
# set.seed(1234)
# shuffled_stp1 <- replicate(80, shuffle_version(ver_stp1), simplify = FALSE)
# set.seed(5678)
# shuffled_stp2 <- replicate(80, shuffle_version(ver_stp2), simplify = FALSE)

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

set.seed(1357)
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


