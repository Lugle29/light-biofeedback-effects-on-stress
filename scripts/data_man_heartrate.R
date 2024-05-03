## ---------------------------
##
## Script name: data_man_heartrate.R
##
## Purpose of script: preprocessing heartrate data, collected by POLAR device.
##
## Author: Luis Glenzer
##
## Date Created: 2024-03-25
##
## ---------------------------

# Setup
# Load Libraries---------------------------------------------------------------
library(hms)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)

# Source Function Script
source('functions.R')

# Get Data---------------------------------------------------------------------

# change working directory
setwd(paste0(dirname(getwd()),'/data'))
# load data
quest <- read.csv("questionnaire/final_transformed.csv")

# check if cleaned datafile exists
if (file.exists("heartrate/hrv_final.csv")) {
  # if so read it
  data <- read.csv("heartrate/hrv_final.csv")
  
  data$Training[data$ID == 'T3KK5X' & 
                  data$TLPLOGEvent == 'Pause1-Start' &
                  data$Log_Sek > 9 &
                  data$Log_Sek < 262] <- 'TrainingOn'
  # transform time variable
  data$time <- as_hms(data$time)
} else {
  # if not, read raw data
  data <- read_delim("heartrate/__Gesamt_short.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE,
                     locale = locale(decimal_mark = ','))
  
  data <- read.csv("heartrate/__Gesamt_short.csv")
  
  # Clean Dataset
  # Remove Columns
  cols_to_remove <- c('Filename', 'date')
  data <- data[, !(names(data) %in% cols_to_remove)]
  
  # Rename Columns
  #data <- data %>%
    #rename(Log_Sek = TLPLogEvent_SekCount,
           #Inq_Sek = InqEvent_SekCount,
           #Tra_Sek = Training_SekCount)
  
  # transform time variable
  data$time <- as_hms(data$time)
  
  write_csv(data, 'heartrate/hrv_final.csv')
  write_xlsx(data, 'heartrate/hrv_final.xlsx')
}

################################### Calibration ################################
# Get Indexes for Calibration Sessions------------------------------------------

# Get the unique IDs from the dataframe
unique_ids <- unique(data$ID)

# Get the indexes where type is CalibrationStart or 
# CalibrationStop and Training is 'CalibrationOn'
start_indexes <- which(data$type == 'CalibrationStart')
stop_indexes <- which(data$type != 'RRIntervall')

# Prepare an empty list to store indexes for each session
cal_ind <- vector("list", length(unique_ids))
names(cal_ind) <- as.character(unique_ids)

# Loop through each pair of start and stop indexes
for (i in seq_along(start_indexes)) {
  start <- start_indexes[i]
  stop <- stop_indexes[stop_indexes > start][1] - 1
  
  # Filter rows where Training is 'CalibrationOn'
  indexes <- start:stop
  indexes <- indexes[data$Training[indexes] == 'CalibrationOn']
  
  id <- data$ID[start]  # Get the ID corresponding to the start index
  
  # Check if the length of indexes is at least 120
  if (length(indexes) >= 100) {
    # Append the indexes to the list with ID as the indicator
    if (is.null(cal_ind[[as.character(id)]])) {
      cal_ind[[as.character(id)]] <- list(indexes)
    } else {
      cal_ind[[as.character(id)]] <- c(cal_ind[[as.character(id)]], list(indexes))
    }
  }
}

# Sanity Check------------------------------------------------------------------

# Get IDs with too many or too little vectors
over  <- names(which(sapply(cal_ind, length) > 4))
under <- names(which(sapply(cal_ind, length) < 4))

cat('There are',length(over),'cases with too much calibration sessions and',
    length(under),'cases with too little calibration sessions')

# Get RMSSD-Values--------------------------------------------------------------
# Compute Average RMSSD for each Calibration Session 
cal_avg <- data.frame(ID         = character(),
                      new_cal_4  = numeric(),
                      new_cal_6  = numeric(),
                      new_cal_8  = numeric(),
                      new_cal_10 = numeric())

for (id in names(cal_ind)) {
  # Set up Vectors
  hrv     <- c(id)
  new_cal <- c('ID')
  
  # Iterate over each Session
  for (vec in cal_ind[[id]]) {
    # Subset Dataframe
    data_sub  <- data[vec,]
    
    # Get Relevant Times - Remove the first 60 seconds
    rmssd   <- data_sub$RMSSD_corr[data_sub$Log_Sek > 60]
    
    # Compute Average and Add to Vector
    avg_hrv <- mean(rmssd, na.rm = TRUE)
    hrv     <- c(hrv, avg_hrv)
    
    # Get Calibration Session and Add to Vector
    cal_ses <- as.character(unique(data$BreathingRate[vec[60:length(vec)]]))
    new_cal <- c(new_cal, paste0('new_cal_',cal_ses))
  }
  
  # Store in Dataframe
  df <- setNames(data.frame(matrix(ncol = length(new_cal), nrow = 1)), new_cal)
  df[1,] <- hrv
  
  # Add each iteration to existing Dataframe
  cal_avg <- rbind(cal_avg, df)
}

# Change datatypes to numeric
for (name in names(cal_avg)[-1]) {
  cal_avg[,name] <- as.numeric(cal_avg[,name])
}

# Relocate Columns
cal_avg <- cal_avg %>% 
  relocate('new_cal_10', .after = 'new_cal_8')

# Compare RMSSD Values----------------------------------------------------------

# Prepare Data
noted <- read_xlsx('study_plan_filled.xlsx')

cols_to_keep <- c('ID','rmssd_4','rmssd_6','rmssd_8','rmssd_10')
cal_avg_com  <- merge(cal_avg, noted[cols_to_keep])

# Rename Columns
cal_avg_com <- cal_avg_com %>%
  rename(old_cal_4 = rmssd_4,
         old_cal_6 = rmssd_6,
         old_cal_8 = rmssd_8,
         old_cal_10 = rmssd_10)

# Create Deviation Columns
for (i in c('4','6','8','10')) {
  df <- cal_avg_com[str_detect(names(cal_avg_com),i)]
  cal_avg_com[paste0('comp_',i)] <- df[1] - df[2]
}


# Find Outliers-------------------------------------------------------------------

# Subset the data to get the IDs where at least one value exceeds 10 in any of the four columns
ids_exceeding_15 <- subset(cal_avg_com, comp_4 > 15 |
                             comp_6 > 15 |
                             comp_8 > 15 |
                             comp_10 > 15)$ID

print(ids_exceeding_15)

# Get Info about wrong Breathing Rate-------------------------------------------

# Deviations in Frequency Choice
cal_avg_com <- merge(cal_avg_com, quest[c('ID','frequency','version')], by = 'ID')

# Rename Column
cal_avg_com <- cal_avg_com %>%
  rename(chosen_rate = frequency)

# Relocate Columns
cal_avg_com <- cal_avg_com %>% 
  relocate('version', .after = 'ID')

# Find the Columns with the highest RMSSD
cols_new_cal <- c('new_cal_4', 'new_cal_6', 'new_cal_8', 'new_cal_10')

rmssd_matrix <- as.matrix(cal_avg_com[cols_new_cal])
max_column_index <- max.col(rmssd_matrix, "first")
max_column_name <- names(cal_avg_com[cols_new_cal])[max_column_index]
max_column_name <- as.numeric(str_extract(max_column_name, '\\d+'))

# Add this to Dataframe
cal_avg_com$best_rate <- max_column_name

# Calculate Difference between Chosen and Ideal
chosen_cols <- paste0('old_cal_',cal_avg_com$chosen_rate)
ideal_cols  <- paste0('new_cal_',cal_avg_com$best_rate)

# Store deviation number
dev <- c()
for (i in 1:nrow(cal_avg_com)) {
  diff <- (cal_avg_com[i,ideal_cols[i]]- cal_avg_com[i,chosen_cols[i]])
  dev  <- c(dev, diff)
}

# Save deviation
cal_avg_com$deviation <- dev

# Distance metrics for calibration deviation------------------------------------
# Mean Euclidean Distance
dev_cols <- grep('^comp', names(cal_avg_com), value = TRUE)
dev_df   <- cal_avg_com[dev_cols]

old_cal <- grep('*old*', names(cal_avg_com), value = TRUE)

# Set Up Vector
avg_euc <- c()

# Loop through each Participant
for (row in 1:nrow(cal_avg_com)) {
  
  # Get the deviation from the relevant frequency
  rel_dev <- cal_avg_com$deviation[row]
  
  # Get the deviation from the other frequencies
  others  <- dev_df[row,]
  others  <- others[others != rel_dev]
  
  # Set up euclidean distance vector
  euc     <- c()
  
  # Compute Euclidean Distance for each pair of relevant and other
  for (i in 1:length(others)) {
    euc   <- c(euclidean_distance(rel_dev,others[i]))
  }
  
  # Get mean Euclidean Distance
  avg_euc <- c(avg_euc,mean(euc))
}

# Store Vector
cal_avg_com$euc_dist <- avg_euc

# Normalize Euclidean Distance
mean_cal_hrv <- rowMeans(cal_avg_com[old_cal])
cal_avg_com$euc_norm <- (cal_avg_com$euc_dist/mean_cal_hrv)

# Save
write.csv(cal_avg_com, 'heartrate/calibration.csv', row.names = FALSE)
# Clean Environment
rm(list = ls()[!ls() %in% c('data', 'quest','cal_avg_com', 'avg_vec', 'list_hrv')])

################################### Study Part I ###############################
### CPT ###
# Get Data----------------------------------------------------------------------
cpt      <- data[data$TLPLOGEvent == 'CPT-Start',]
cpt_list <- list()

# Get Vectors for each Participant----------------------------------------------
for (id in unique(data$ID)) {
  # Subset Data
  cpt_sub <- cpt[cpt$ID == id,]
  
  # Get Default Vector
  cpt_list[[id]][['default']] <- cpt$RMSSD_corr[cpt$ID == id]
  
  # Get Vector with Cut out Values
  ## Subset Data
  cut_beg <- 70                        # How many seconds should be cut from the beginning
  cut_end <- max(cpt_sub$Log_Sek) - 10 # How many seconds should be cut from the end
  cpt_rel <- cpt_sub[cpt_sub$Log_Sek > cut_beg & cpt_sub$Log_Sek < cut_end,]
  
  ## Get Vector
  cpt_list[[id]][['manip']]   <- cpt_rel$RMSSD_corr
}

# Average the manipulated Vectors-----------------------------------------------
## Cut 80 seconds from start (20 sec before CPT onset and 60 sec for real RMSSD) 
## and cut 10 seconds from end (clicking end button after offset)

avgs <- c()
for (id in names(cpt_list)) {
  avgs <- c(avgs, mean(cpt_list[[id]][['manip']], na.rm = TRUE))
}

# Store HRV-average-------------------------------------------------------------

# Set Up Dataframe to store all HRV Vectors
avg_hrv <- data.frame(ID = names(cpt_list),
                      cpt = avgs)
                      
### PASAT ### 
# Preparation ------------------------------------------------------------------
# Get Data
pasat_times <- read.csv('inquisit/PASAT.csv')
pasat       <- data[data$TLPLOGEvent == 'PASAT-Start',]

# Transform Timecolumn 
# Manipulate for further usage
pasat_times$lvl3_starttime <- as_hms(pasat_times$lvl3_starttime)
pasat_times$lvl3_starttime <- as_hms(round(pasat_times$lvl3_starttime))

# Get HRV for PASAT ------------------------------------------------------------
# Get HRV vectors for PASAT LVL 3
pasat_list <- list()
for (id in quest$ID) {

  if (id %in% unique(pasat$ID)) {
    
    # Subset Dataframe
    pasat_sub      <- pasat[pasat$ID == id,]
    lvl3_starttime <- pasat_times$lvl3_starttime[pasat_times$ID == id]
    
    # Get Start end End Times of 3rd PASAT Level
    start_ind <- which.min(abs(pasat_sub$time - lvl3_starttime)) # When does the third level start?
    starttime <- pasat_sub$time[start_ind]
    end_ind   <- which.min(abs(pasat_sub$time - as_hms(starttime + 600)))
    endtime   <- pasat_sub$time[end_ind]
    
    # Subset for 3rd level
    pasat_lvl <- pasat_sub[pasat_sub$time >= starttime & pasat_sub$time <= endtime,]
    
    # Get Default Vector
    pasat_list[[id]][['default']] <- pasat_lvl$RMSSD_corr
    
    # Get Vector with Cut out Values
    ## Subset Data
    cut_beg   <- min(pasat_lvl$Inq_Sek) + 60 # How many seconds should be cut from the beginning
    pasat_rel <- pasat_lvl[pasat_lvl$Inq_Sek > cut_beg,]
    
    ### Rename Columns
    pasat_list[[id]][['manip']]   <- pasat_rel$RMSSD_corr
    
  } else {
    pasat_list[[id]][['default']] <- NA
    pasat_list[[id]][['manip']]   <- NA
  }
}

# Average the manipulated Vectors 
## Cut 60 sec from start for real RMSSD
avgs <- c()
for (id in names(pasat_list)) {
  avgs <- c(avgs, mean(pasat_list[[id]][['manip']],na.rm = TRUE))
}

# Add to Dataframe -------------------------------------------------------------
avg_hrv$pasat <- avgs

# Clean Environment
rm(list = ls()[!ls() %in% c('data', 'quest', 'avg_hrv', 'avg_vec', 'list_hrv')])

### Breaks ###
# Prepare ----------------------------------------------------------------------
# Subset for Break Data
break_hrv <- data[data$TLPLOGEvent == 'Pause1-Start' |
                    data$TLPLOGEvent == 'Pause2-Start',]

# Get HRV for breaks -----------------------------------------------------------
# Get HRV Vectors
break_list <- list_hrv(break_hrv,quest)

# Extract Averaged Manipulated Vectors
avgs <- avg_vec(break_list)

# Add to Dataframe -------------------------------------------------------------
avg_hrv$break_l  <- avgs[['light']]
avg_hrv$break_nl <- avgs[['nlight']]

# Clean Environment
rm(list = ls()[!ls() %in% c('data', 'quest', 'avg_hrv', 'avg_vec', 'list_hrv',
                            'plot_path')])
################################### Study Part II ##############################
### PVT ###
# Prepare ----------------------------------------------------------------------
# Subset for PVT Data
pvt_hrv <- data[data$TLPLOGEvent == 'PVT1-Start' |
                  data$TLPLOGEvent == 'PVT2-Start',]

# Prepare List to stor results in
pvt_list <- list_hrv(pvt_hrv, quest)

# Get HRV for PVT --------------------------------------------------------------
# Extract Averaged Manipulated Vectors
avgs <- avg_vec(pvt_list)

# Add to Dataframe -------------------------------------------------------------
avg_hrv$pvt_l  <- avgs[['light']]
avg_hrv$pvt_nl <- avgs[['nlight']]

# Clean Environment
rm(list = ls()[!ls() %in% c('data', 'quest', 'avg_hrv', 'avg_vec', 'list_hrv', 
                            'plot_path')])

### BART ###
# Prepare ----------------------------------------------------------------------
# Subset for BART Data
bart_hrv <- data[data$TLPLOGEvent == 'BART1-Start' |
                  data$TLPLOGEvent == 'BART2-Start',]

# Get HRV for BART -------------------------------------------------------------
# Get HRV Vectors
bart_list <- list_hrv(bart_hrv, quest)

# Extract Averaged Manipulated Vectors
avgs <- avg_vec(bart_list)

# Add to Dataframe -------------------------------------------------------------
avg_hrv$bart_l  <- avgs[['light']]
avg_hrv$bart_nl <- avgs[['nlight']]

# Clean Environment
rm(list = ls()[!ls() %in% c('data', 'quest', 'avg_hrv', 'avg_vec', 'list_hrv')])

# Save Files -------------------------------------------------------------------
# as csv
write.csv(avg_hrv, 'heartrate/average_hrv.csv', row.names = FALSE)
# as xlsx
write_xlsx(avg_hrv, 'heartrate/average_hrv.xlsx')

# Find Missing Values
hrv <- avg_hrv
na_indices <- which(apply(hrv, 1, function(row) any(is.na(row))))
hrv$ID[na_indices]
