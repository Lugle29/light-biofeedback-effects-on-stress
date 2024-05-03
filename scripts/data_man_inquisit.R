## ---------------------------
##
## Script name: data_man_inquisit.R
##
## Purpose of script: preprocessing performance test data
##
## Author: Luis Glenzer
##
## Date Created: 2024-03-25
##
## ---------------------------

# Setup
# Load Libraries ---------------------------------------------------------------
library(stringr)
library(dplyr)
library(readr)
library(hms)

# Set Datapaths ----------------------------------------------------------------
setwd(paste0(getwd(),'/data'))
quest <- read.csv("questionnaire/final_transformed.csv")

####################################### PASAT ##################################

# Prepare ----------------------------------------------------------------------
# Get Data
pasat_sum <- read_delim("inquisit/#_Pasat_Summary_Gesamt.csv",
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Extract relevant Columns
rel_cols <- c('subjectid', 'starttime', 'Version', 'elapsedtime',
              'totalcorrect', 'level1Duration_actual', 'level2Duration_actual',
              'level3Duration_actual')

pasat_sum <- pasat_sum[rel_cols]

# Transformation ---------------------------------------------------------------
# Rename Columns
colnames(pasat_sum)[1] <- 'ID'
colnames(pasat_sum)[3] <- 'version'

colnames(pasat_sum)[which(names(pasat_sum) == 'level1Duration_actual')] <- 'lvl1_duration'
colnames(pasat_sum)[which(names(pasat_sum) == 'level2Duration_actual')] <- 'lvl2_duration'
colnames(pasat_sum)[which(names(pasat_sum) == 'level3Duration_actual')] <- 'lvl3_duration'

# Manipulate Variables
## Version
pasat_sum$version <- str_extract(pasat_sum$version, '[1-9]')
pasat_sum$version <- as.factor(pasat_sum$version)

## Change Duration into seconds
for (col in c('elapsedtime', 'lvl1_duration','lvl2_duration','lvl3_duration')){
  pasat_sum[col] <- (pasat_sum[col]/1000)
}

### Get the sum of all lvl durations to see how much time was spent in instructions
pasat_sum$lvl_duration <- (pasat_sum$lvl1_duration + 
                             pasat_sum$lvl2_duration + 
                             pasat_sum$lvl3_duration)

pasat_sum$non_lvl_duration <- (pasat_sum$elapsedtime - 
                                 pasat_sum$lvl_duration)

### Get Lvl3 Starttime by computing the time when the test ended and substracting 
### the lvl3 duration from it
pasat_sum$lvl3_starttime <- as_hms((pasat_sum$starttime + 
                                      pasat_sum$elapsedtime - 
                                      pasat_sum$lvl3_duration))

### Relocate Column
pasat_sum <- pasat_sum %>% 
  relocate('totalcorrect', .after = 'version') %>%
  relocate('lvl_duration', .after = 'elapsedtime') %>%
  relocate('non_lvl_duration', .after = 'lvl_duration')

# Save Dataframe ---------------------------------------------------------------
write.csv(pasat_sum, paste0(getwd(),'/inquisit/PASAT.csv'), row.names = FALSE)

####################################### PVT ####################################

# Prepare ----------------------------------------------------------------------
# Get Summary Data
pvt_sum <- read_delim("inquisit/#_PVT_Summary_Gesamt.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE,
                      locale = locale(decimal_mark = ','))

# Get Raw Data
pvt_raw <- read_delim("inquisit/#_PVT_Raw_Gesamt.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE,
                      locale = locale(decimal_mark = ','))

# Transform --------------------------------------------------------------------
# Change Column Names
colnames(pvt_sum)[which(names(pvt_sum) == 'subjectid')] <- 'ID'
colnames(pvt_raw)[which(names(pvt_raw) == 'subject')] <- 'ID'

# Correct data for instances with technical issues (O6UA2U; P8SJ1E)
for (id in c('O6UA2U', 'P8SJ1E')){
  # Get Information where to assign new values
  index <- which(pvt_raw$rt[pvt_raw$ID == id] == 30000)
  n_issue <- length(index)
  durchgang <- unique(pvt_raw$Durchgang[index])
  
  # Compute new values
  rt <- pvt_raw$rt[pvt_raw$ID == id & pvt_raw$Durchgang == durchgang]
  rt[rt == 30000] <- NA
  
  ## All RT
  new_max         <- max(rt, na.rm = TRUE)
  new_mean        <- mean(rt, na.rm = TRUE)
  new_median      <- median(rt, na.rm = TRUE)
  number_lapses   <- pvt_sum$numberOfLapses[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]
  new_lapses      <- (number_lapses - n_issue)
  
  ### Assign new values
  pvt_sum$meanRT[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]         <- new_mean
  pvt_sum$maxRT[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]          <- new_max
  pvt_sum$medianRT[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]       <- new_median
  pvt_sum$numberOfLapses[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang] <- new_lapses
  
  ## RT500
  new_max         <- max(rt[rt <= 500], na.rm = TRUE)
  new_mean        <- mean(rt[rt <= 500], na.rm = TRUE)
  new_median      <- median(rt[rt <= 500], na.rm = TRUE)
  
  ### Assign new values
  pvt_sum$meanRT500[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]   <- new_mean
  pvt_sum$maxRT500[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang]    <- new_max
  pvt_sum$medianRT500[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang] <- new_median
  
  ## Different important values
  ### Stimulustime as values due to technical issues will be removed, such that 
  ### their stimulustime has to be removed from elapsedtime too
  sum_stimtime <- sum(pvt_raw$clock.target.stimulusonset[pvt_raw$ID == id][index])
  pvt_sum$elapsedtime[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang] <- 
    (pvt_sum$elapsedtime[pvt_sum$ID == id & pvt_sum$Durchgang == durchgang] - sum_stimtime)
}

# Extract relevant Columns
rel_cols <- c('ID', 'Version', 'Durchgang', 'starttime',  'elapsedtime', 
              'meanRT', 'reciprocalMeanRT', 'meanRT500', 'reciprocalMeanRT500',
              'numberOfLapses', 'countFalseStarts')

pvt_sum <- pvt_sum[rel_cols]

# Transform Reaction Time (Reciprocal Mean) by multiplying with 1000 so that it
# will be displayed in milliseconds
pvt_sum$reciprocalMeanRT    <- (pvt_sum$reciprocalMeanRT*1000)
pvt_sum$reciprocalMeanRT500 <- (pvt_sum$reciprocalMeanRT500*1000)

# Add Endtime
## Transform elapsedtime from milliseconds to seconds
pvt_sum$elapsedtime <- (pvt_sum$elapsedtime/1000)
pvt_sum$endtime     <- as_hms(round(pvt_sum$starttime + pvt_sum$elapsedtime))

## Relocate Column
pvt_sum <- pvt_sum %>% 
  relocate('endtime', .after = 'starttime')

## Rename Column
colnames(pvt_sum)[which(names(pvt_sum) == 'Version')] <- 'version'

## Manipulate Version
pvt_sum$version <- str_extract(pvt_sum$version, '[1-9]')

# Create Light Indication
pvt_sum$light <- ifelse((pvt_sum$version == 1 & pvt_sum$Durchgang == 1) |
                           (pvt_sum$version == 2 & pvt_sum$Durchgang == 2),
                         'light', 'nlight')
pvt_sum$version <- as.factor(pvt_sum$version)

# Transpose From Long to Wide Format
## Extract Light Columns and Rename them
pvt_light <- pvt_sum[pvt_sum$light == 'light',
                       !(names(pvt_sum) %in% c('Durchgang', 'light'))]

for (i in 3:length(colnames(pvt_light))){
  colnames(pvt_light)[i] <- paste0(colnames(pvt_light)[i],'_l')
}

## Extract Non-Light Columns and Rename them
pvt_nlight <- pvt_sum[pvt_sum$light == 'nlight',
                        !(names(pvt_sum) %in% c('Durchgang', 'light', 'version'))]

for (i in 2:length(colnames(pvt_nlight))){
  colnames(pvt_nlight)[i] <- paste0(colnames(pvt_nlight)[i],'_nl')
}

## Merge Dataframes
pvt_sum_wide <- merge(pvt_light, pvt_nlight, by = 'ID')

# Save -------------------------------------------------------------------------
# Wide Format
write.csv(pvt_sum_wide, paste0(getwd(),'/inquisit/PVT.csv'), row.names = FALSE)
# Long Format
write.csv(pvt_sum, paste0(getwd(),'/inquisit/PVT_long.csv'), row.names = FALSE)

# Clean Environment
rm(list = ls()[!ls() %in% "quest"])

####################################### BART ###################################
# Prepare ----------------------------------------------------------------------
# Import Data
bart_sum <- read_delim("inquisit/#_Bart_Summary_Gesamt.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE,
                       locale = locale(decimal_mark = ','))

# Extract relevant Columns
rel_cols <- c('subjectid', 'Version','starttime', 'Durchgang', 'elapsedtime',
              'totalearnings', 'total_explosions', 'totalpumpcount')
bart_sum <- bart_sum[rel_cols]

# Transform --------------------------------------------------------------------
# Rename Column
colnames(bart_sum)[1] <- 'ID'
colnames(bart_sum)[2] <- 'version'

# Manipulate Version
bart_sum$version <- str_extract(bart_sum$version, '[1-9]')

# Add Endtime
## Transform elapsedtime from milliseconds to seconds
bart_sum$elapsedtime <- (bart_sum$elapsedtime/1000)
bart_sum$endtime <- as_hms(round(bart_sum$starttime + bart_sum$elapsedtime))

## Relocate Column
bart_sum <- bart_sum %>% 
  relocate('endtime', .after = 'starttime')

# Create Light Indication
bart_sum$light <- ifelse((bart_sum$version == 1 & bart_sum$Durchgang == 1) |
                               (bart_sum$version == 2 & bart_sum$Durchgang == 2),
                             'light', 'nlight')
bart_sum$version <- as.factor(bart_sum$version)

# Transpose From Long to Wide Format
## Extract Light Columns and Rename them
bart_light <- bart_sum[bart_sum$light == 'light',
                       !(names(bart_sum) %in% c('Durchgang', 'light'))]

for (i in 3:length(colnames(bart_light))){
  colnames(bart_light)[i] <- paste0(colnames(bart_light)[i],'_l')
}

# Extract Non-Light Columns and Rename them
bart_nlight <- bart_sum[bart_sum$light == 'nlight',
                        !(names(bart_sum) %in% c('Durchgang', 'light', 'version'))]

for (i in 2:length(colnames(bart_nlight))){
  colnames(bart_nlight)[i] <- paste0(colnames(bart_nlight)[i],'_nl')
}

# Merge Dataframes
## Merge
bart_sum_wide <- merge(bart_light, bart_nlight, by = 'ID')

## Relocate Columns
bart_sum_wide <- bart_sum_wide %>% 
  relocate('starttime_nl', .after = 'starttime_l') %>%
  relocate('endtime_nl', .after = 'endtime_l') %>%
  relocate('elapsedtime_nl', .after = 'elapsedtime_l')

# Save -------------------------------------------------------------------------
# Wide Format
write.csv(bart_sum_wide, paste0(getwd(),'/inquisit/BART.csv'), row.names = FALSE)
# Long Format
write.csv(bart_sum, paste0(getwd(),'/inquisit/BART_long.csv'), row.names = FALSE)

# Clean Environment
rm(list = ls()[!ls() %in% "quest"])
