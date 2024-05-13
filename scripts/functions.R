## ---------------------------
##
## Script name: functions.R
##
## Purpose of script: storing functions for heartrate data management
##
## Author: Luis Glenzer
##
## Date Created: 2024-03-30
##
## ---------------------------

# Setup
# Load Libraries ---------------------------------------------------------------
library(hms)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)

# Function to create list HRV vectors for each ID
list_hrv <- function(data, id_data, score = 'HRV') {
  # Prepare --------------------------------------------------------------------
  # Set Up List
  hrv_list <- list()
  
  if (score == 'HRV') {
    col <- 'RMSSD_corr'
  } else if (scre == 'HR') {
    col <- 'heartrate'
  } else {
    warning('The chosen score is not available! Use HRV or HR')
  }
  
  # Assign Values --------------------------------------------------------------
  for (id in id_data$ID) {
    
    # Subset Dataframe for each participant
    data_sub <- data[data$ID == id,]
    
    # Get HRV Vectors for light condition
    ## Subset Data
    data_l   <- data_sub[data_sub$Licht == 'Licht' & data_sub$Training == 'TrainingOn',]
    
    ## Get time vector
    hrv_list[[id]][['light']][['default']] <- data_l$Log_Sek
    
    ## Get default Vector
    hrv_list[[id]][['light']][['default']] <- data_l[,col]
    
    ## Get Vector with Cut out Values
    ### Set cut out values
    cut_beg  <- 60 # How many seconds should be cut from the beginning
    data_lt  <- data_l[data_l$Tra_Sek > cut_beg,]
    
    ### Get manipulated Vector
    hrv_list[[id]][['light']][['manip']]   <- data_lt[,col]
    
    # Get HRV Vectors for non-light condition
    ## Subset Data
    data_nl   <- data_sub[data_sub$Licht == 'ohneLicht',]
    
    ## Get time vector
    hrv_list[[id]][['light']][['time']] <- data_nl$Log_Sek
    
    ## Get default vector
    hrv_list[[id]][['nlight']][['default']] <- data_nl[,col]
    
    ## Get Vector with Cut out Values
    ### Subset Data
    data_nlt  <- data_nl[data_nl$Tra_Sek > cut_beg,]
    
    ### Get manipulated Vector
    hrv_list[[id]][['nlight']][['manip']] <- data_nlt[,col]
  }
  # Return ---------------------------------------------------------------------
  return(hrv_list)
}

# Function to create list with time, HR and HRV vectors for each ID
list_hr_time <- function(data) {
  # Prepare --------------------------------------------------------------------
  # Set Up List
  hr_list <- list()
  
  # Define column to extract
  col <- c('heartrate', 'RMSSD_corr')
  
  # Get Vectors for each Participant -------------------------------------------
  for (id in unique(data$ID)) {
    # Subset Data
    data_sub <- data[data$ID == id,]
    
    # Get Time Vector
    hr_list[[id]][['time']] <- as.numeric(data_sub$time - data_sub$time[1]) # Get second indicator
    
    # Get heartrate Vector
    hr_list[[id]][['hr']] <- data_sub[,col[1]]
    
    # Get hrv Vector
    hr_list[[id]][['hrv']] <- data_sub[,col[2]]
  }
  # Return ---------------------------------------------------------------------
  return(hr_list)
}

# Function to turn hr_list into dataframe with specified score
hr_bytime <- function(data, score) {
  # Prepare --------------------------------------------------------------------
  # Set Up Dataframe to store results
  df <- data.frame(time = 0:1200)
  
  # Assign Values --------------------------------------------------------------
  for (id in names(data)) {
    # Dataframe for actual participant
    id_df <- as.data.frame(data[[id]][c('time',score)])
    
    # Calculate average for each second
    id_df <- id_df %>%
      group_by(time) %>%
      summarize(!!id := mean(!!sym(score))) %>%
      slice(1:(n() - 10))
    
    # Merge to group dataframe
    df <- merge(df, id_df, on = 'time', all.x = TRUE)
  }
  
  # Calculate group average
  df[score] <- rowMeans(df[,2:ncol(df)], na.rm = TRUE)
  
  # Return ---------------------------------------------------------------------
  return(df)
}

# Function to average manipulated vectors
avg_vec <- function(vector_list) {
  # Prepare --------------------------------------------------------------------
  # Set up list to store results
  return_list = list()
  
  # Set up light and non-light vectors
  avgs_l <- c()
  avgs_nl <- c()
  
  # Compute Average
  for (id in names(vector_list)) {
    vec_l <- vector_list[[id]][['light']][['manip']]
    avgs_l <- c(avgs_l, mean(vec_l, na.rm = TRUE))
    
    vec_nl <- vector_list[[id]][['nlight']][['manip']]
    avgs_nl <- c(avgs_nl, mean(vec_nl,na.rm = TRUE))
  }
  
  # Aggregate ------------------------------------------------------------------
  return_list[['ID']]     <- names(vector_list)
  return_list[['light']]  <- avgs_l
  return_list[['nlight']] <- avgs_nl
  
  # Return ---------------------------------------------------------------------
  return(return_list)
}

# Function to calculate Euclidean distance
euclidean_distance <- function(rel_dev, other) {
  sqrt((rel_dev - other)^2)
}
