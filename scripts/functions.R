# Setup
library(hms)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)

# Function to create list with IDs as index. Within are the vectors representing
# the HRV.
list_hrv <- function(data, id_data) {
  ## Prepare List
  hrv_list <- list()
  
  ## Fill List
  for (id in id_data$ID) {
    
    # Subset Dataframe
    data_sub <- data[data$ID == id,]
    
    # Get HRV Vectors for light break
    ## Subset Data
    data_l   <- data_sub[data_sub$Licht == 'Licht' & data_sub$Training == 'TrainingOn',]
    
    ## Get Vector
    hrv_list[[id]][['light']][['default']] <- data_l$RMSSD_corr
    
    ## Get Vector with Cut out Values
    ### Subset Data
    cut_beg  <- 60 # How many seconds should be cut from the beginning
    data_lt  <- data_l[data_l$Tra_Sek > cut_beg,]
    
    ### Get Vector
    hrv_list[[id]][['light']][['manip']]   <- data_lt$RMSSD_corr
    
    # Get HRV Vectors for non-light break
    ## Subset Data
    data_nl   <- data_sub[data_sub$Licht == 'ohneLicht',]
    
    ## Get Vector
    hrv_list[[id]][['nlight']][['default']] <- data_sub$RMSSD_corr[data_sub$Licht == 'ohneLicht']
    
    ## Get Vector with Cut out Values
    ### Subset Data
    data_nlt  <- data_nl[data_nl$Tra_Sek > cut_beg,]
    
    ### Get Vector
    hrv_list[[id]][['nlight']][['manip']]   <- data_nlt$RMSSD_corr
  }
  return(hrv_list)
}

# Function to get the manipulated averaged vectors
avg_vec <- function(vector_list) {
  # Prepare Objects
  ## List
  return_list = list()
  
  ## Prepare Light and Non-Light Vectors
  avgs_l <- c()
  avgs_nl <- c()
  
  # Compute Average
  for (id in names(vector_list)) {
    vec_l <- vector_list[[id]][['light']][['manip']]
    avgs_l <- c(avgs_l, mean(vec_l, na.rm = TRUE))
    
    vec_nl <- vector_list[[id]][['nlight']][['manip']]
    avgs_nl <- c(avgs_nl, mean(vec_nl,na.rm = TRUE))
  }
  
  # Aggregate in List to Return
  return_list[['ID']]     <- names(vector_list)
  return_list[['light']]  <- avgs_l
  return_list[['nlight']] <- avgs_nl
  
  return(return_list)
}

# Function to calculate Euclidean distance
euclidean_distance <- function(rel_dev, other) {
  sqrt((rel_dev - other)^2)
}
