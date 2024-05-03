library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(tidyverse)
library(rstatix)
library(xml2)
library(stringr)
library(writexl)

setwd(paste0(getwd(),'/data'))

# Get Data
data <- read.csv2(paste0(getwd(),'/questionnaire/','questionnaire.csv'))
study_plan <- read_excel('study_plan_filled.xlsx')

## Remove empty columns
data <- data[,-c(1:4)]
columns_to_remove <- c(grep("Studienleitung", names(data)),
                       grep("Pause", names(data)),
                       grep("Ende", names(data)))
data <- data[, -columns_to_remove]

## Clean faulty Column Values
### UEQ
data$UEQS1.SQ001. <- as.integer(gsub("^A", "", data$UEQS1.SQ001.))
data$UEQS1.SQ002. <- as.integer(gsub("^A", "", data$UEQS1.SQ002.))
data$UEQS2.SQ001. <- as.integer(gsub("^A", "", data$UEQS1.SQ003.))
data$UEQS2.SQ001. <- as.integer(gsub("^A", "", data$UEQS2.SQ001.))
data$UEQS2.SQ002. <- as.integer(gsub("^A", "", data$UEQS2.SQ002.))
data$UEQS2.SQ003. <- as.integer(gsub("^A", "", data$UEQS2.SQ003.))

### Sehunterstützung
data$Sehunterstuetzung[data$Sehunterstuetzung == "N"] <- 0
data$Sehunterstuetzung[data$Sehunterstuetzung == "Y"] <- 1
data$Sehunterstuetzung <- as.integer(data$Sehunterstuetzung)

data$Sehunterstuetzung2[data$Sehunterstuetzung2 == ""] <- NA
data$Sehunterstuetzung2[data$Sehunterstuetzung2 == "A1"] <- 'Brille'
data$Sehunterstuetzung2[data$Sehunterstuetzung2 == "A2"] <- 'Linse'

### Studium
data$Semester <- as.integer(str_extract(data$Semester,'[1-9]'))
data$Semester <- ifelse(data$Schulabschluss >= 6 & data$Semester >= 8,
                        data$Semester - 6, data$Semester)

data$Stud_Circle <- ifelse(data$Schulabschluss == 6, 
                           'Master', 'Bachelor')

data <- data %>% 
  relocate('Stud_Circle', .after = 'Schulabschluss')

### Eisbaden
data$Eisbaden[data$Eisbaden == "N"] <- 0
data$Eisbaden[data$Eisbaden == "Y"] <- 1
data$Eisbaden <- as.integer(data$Eisbaden)

### Ishihara
data$Ishihara <- as.double(data$Ishihara)
data$Ishihara[data$Ishihara < 1] <- data$Ishihara[data$Ishihara < 1] * 100

# Merge Data
data <- merge(study_plan[c('ID','testing_version','calib_frequency','calib_color','sex_numerical')], 
              data, by = 'ID')
data$testing_version <- ifelse(data$testing_version == 'Ver_1',1,2)

# Rename Columns
colnames(data)[colnames(data) %in% c('testing_version',
                                     'calib_frequency',
                                     'calib_color',
                                     'sex_numerical')] <- c('version', 'frequency', 'color', 'sex')

# Add config data
filenames <- list.files(paste0(getwd(),'/config'))

config <- function(df = data,
                   files = filenames){
  for (file in files){
    
    # Get ID
    id <- str_extract(file, '^[0-9A-Z]{6}')
    
    # Get Config Info
    xml_data <- read_xml(paste0(getwd(),'/config/',file))
    xml_info <- xml_text(xml_find_all(xml_data, ".//value"))
    
    # Extract Relevant Info
    df$frequency[df$ID == id] <- xml_info[3]
    df$color[df$ID == id]  <- xml_info[5]
    df$brightness[df$ID == id] <- as.numeric(xml_info[length(xml_info)])
  }
  return(df)
}
data <- config()

# Reorder Columns
data <- data %>% 
  relocate('brightness', .after = 'color')

# Change Column names
## Replace . by _
i = 1
for (name in colnames(data)){
  if (str_detect(name, "\\.")){
    name <- str_replace(name,'\\.','\\_')
    name <- str_remove(name, '\\.')
  }
  if (str_detect(name, "\\SQ")){
    name <- str_replace(name,'\\SQ00','')
  }
  colnames(data)[i] <- name
  i = i + 1
}

## Rescale VAS, such that 'no stress' equals 0
col_to_rescale <- c('VAS1_1', 'VAS2_1', 'VAS3_1',
                    'VAS4_1', 'VAS5_1', 'VAS6_1')
for (col in col_to_rescale){
  data[col] <- (data[col] - 1)
}

write.csv(data, paste0(getwd(),'/questionnaire/final.csv'), row.names = FALSE)

# Compute Health Scores after https://heartbeat-med.com/de/resources/whoqol-bref/
data$phy_health <- 4*((6 - data$QOLErl_3) + (6 - data$QOLErl_4) + 
                        data$QOLMoeg_10 + data$QOLFort_15 + data$QOLZuf_16 + 
                        data$QOLZuf_17 + data$QOLZuf_18)/7
data$psy_health <- 4*(data$QOLErl_5 + data$QOLErl_6 + data$QOLErl_7 + 
                        data$QOLMoeg_11 + data$QOLZuf_19 +
                        (6 - data$QOLAng_26))/6
data$soc_health <- 4*(data$QOLZuf_20 + data$QOLZuf_21 + data$QOLZuf_22)/3
data$env_health <- 4*(data$QOLErl_8 + data$QOLErl_9 + data$QOLMoeg_12 + 
                        data$QOLMoeg_13 + data$QOLMoeg_14 + data$QOLZuf_23 + 
                        data$QOLZuf_24 + data$QOLZuf_25)/8

## Scale to 0-100
data$phy_health <- (data$phy_health - 4)*(100/16)
data$psy_health <- (data$psy_health - 4)*(100/16)
data$soc_health <- (data$soc_health - 4)*(100/16)
data$env_health <- (data$env_health - 4)*(100/16)

## Drop Unused Columns
remove_cols = vector()
for (name in colnames(data)){
  if (str_detect(name, "\\QOL")){
    remove_cols <- c(remove_cols, name)
  }
}

data = subset(data, select = !(colnames(data) %in% remove_cols))

# Reorder Columns
data <- data %>% 
  relocate(c('phy_health',
             'psy_health',
             'soc_health',
             'env_health'), .after = 'Ishihara')


# Change Columns based on version such that columns can be renamed after light/no_light ####
## Study Part I
### Rescale KAB Colums
#### Get Relevant Column Names

kab_columns      <- grep('^KAB', names(data), value = TRUE)
items_to_rescale <- c(2,4,6)

#### Rescale
for (col in kab_columns) {
  # Get Item Number
  ## It is relevant to seperate KAB Numbers as in questionnaire construction
  ## there was a mistake such that KAB1-2 ranges from 0-5 while the rest ranges
  ## from 1-6
  kab_no   <- str_extract(col, "[-+]?\\d*\\.?\\d+")
  kab_no   <- as.numeric(kab_no)
  
  kab_item <- str_extract_all(col, "\\d(?!.*\\d)")[[1]]
  kab_item <- as.numeric(kab_item)
  
  # Rescale if the item number matches the one to transform
  if (kab_item %in% items_to_rescale) {
    if (kab_no <= 2) {
      max_value <- 5
      data[col] <- (max_value - data[col])
    } else {
      max_value <- 6
      data[col] <- (max_value - (data[col] - 1))
    }
  }
}

### VAS, UEQ_S, KAB light
#### Create Column Vectors
col_ver1_l <- c('ID', 'version', 'VAS3_1', 'UEQS1_1', 'UEQS1_2', 'UEQS1_3',
                'KAB1_01','KAB1_02','KAB1_03','KAB1_04','KAB1_05','KAB1_06')
col_to_repl_l <- c('VAS3_l', 'UEQS11_l', 'UEQS12_l', 'UEQS13_l', 'KAB11_l',
                 'KAB12_l','KAB13_l','KAB14_l','KAB15_l','KAB16_l')

ver1_l <- data[data$version == 1, col_ver1_l]
colnames(ver1_l)[3:length(ver1_l)] <- col_to_repl_l
  
col_ver2_l <- c('ID', 'version', 'VAS6_1', 'UEQS2_1', 'UEQS2_2', 'UEQS2_3',
                'KAB2_01','KAB2_02','KAB2_03','KAB2_04','KAB2_05','KAB2_06')
ver2_l <- data[data$version == 2, col_ver2_l]
colnames(ver2_l)[3:length(ver2_l)] <- col_to_repl_l 

col_l <- rbind(ver1_l,ver2_l)

### VAS, UEQ_S, KAB non-light
col_ver1_nl <- col_ver2_l
col_to_repl_nl <- c('VAS3_nl', 'UEQS11_nl', 'UEQS12_nl', 'UEQS13_nl', 'KAB11_nl',
                 'KAB12_nl','KAB13_nl','KAB14_nl','KAB15_nl','KAB16_nl')

ver1_nl <- data[data$version == 1, col_ver1_nl]
colnames(ver1_nl)[3:length(ver1_nl)] <- col_to_repl_nl

col_ver2_nl <- col_ver1_l

ver2_nl <- data[data$version == 2, col_ver2_nl]
colnames(ver2_nl)[3:length(ver2_nl)] <- col_to_repl_nl 

col_nl <- rbind(ver1_nl,ver2_nl)

### Implement VAS, UEQ_S, KAB_(non)light column in data
data <- merge(data, col_l[,c('ID',col_to_repl_l)], by = "ID", all.x = TRUE)
data <- merge(data, col_nl[,c('ID',col_to_repl_nl)], by = "ID", all.x = TRUE)

### Rename Columns
data <- data %>%
  rename(VAS_pre_CPT     = VAS1_1,
         VAS_pos_CPT     = VAS2_1,
         VAS_pre_PASAT   = VAS4_1,
         VAS_pos_PASAT   = VAS5_1,
         VAS_bre_CPT     = VAS3_1,
         UEQS_cpt_1      = UEQS1_1,
         UEQS_cpt_2      = UEQS1_2,
         UEQS_cpt_3      = UEQS1_3,
         KAB_cpt_1       = KAB1_01,
         KAB_cpt_2       = KAB1_02,
         KAB_cpt_3       = KAB1_03,
         KAB_cpt_4       = KAB1_04,
         KAB_cpt_5       = KAB1_05,
         KAB_cpt_6       = KAB1_06,
         VAS_bre_PASAT   = VAS6_1,
         UEQS_PASAT_1    = UEQS2_1,
         UEQS_PASAT_2    = UEQS2_2,
         UEQS_PASAT_3    = UEQS2_3,
         KAB_PASAT_1     = KAB2_01,
         KAB_PASAT_2     = KAB2_02,
         KAB_PASAT_3     = KAB2_03,
         KAB_PASAT_4     = KAB2_04,
         KAB_PASAT_5     = KAB2_05,
         KAB_PASAT_6     = KAB2_06)

### Clean the environment
rm(list = ls()[!ls() %in% "data"])

## Study Part II
### PVT
#### Light
##### Version 1
col_v1_l <- c('ID', 'version')
for (name in colnames(data)){
  if (str_detect(name, "KAB3") |
      str_detect(name, "KAB4") |
      str_detect(name, "UEQ1") |
      str_detect(name, "StoerBS1")){
    col_v1_l <- c(col_v1_l, name)
  }
}

col_repl_l <- c(paste0(rep('KAB_pre_',6),rep('PVT_l',6),rep(1:6)),
                paste0(rep('KAB_pos_',6),rep('PVT_l',6),rep(1:6)),
                paste0(rep('UEQ_',8),rep('PVT_l',8),rep(1:8)),
                paste0(rep('SBS_',3),rep('PVT_l',3),rep(1:3)))

ver1_l <- data[data$version == 1, col_v1_l]
colnames(ver1_l)[3:length(ver1_l)] <- col_repl_l

##### Version 2
col_v2_l <- c('ID', 'version')
for (name in colnames(data)){
  if (str_detect(name, "KAB7") |
      str_detect(name, "KAB8") |
      str_detect(name, "UEQ3") |
      str_detect(name, "StoerBS3")){
    col_v2_l <- c(col_v2_l, name)
  }
}

ver2_l <- data[data$version == 2, col_v2_l]
colnames(ver2_l)[3:length(ver2_l)] <- col_repl_l

col_l <- rbind(ver1_l,ver2_l)

#### Non-Light
##### Version 1
col_v1_nl <- col_v2_l

col_repl_nl <- c(paste0(rep('KAB_pre_',6),rep('PVT_nl',6),rep(1:6)),
                 paste0(rep('KAB_pos_',6),rep('PVT_nl',6),rep(1:6)),
                 paste0(rep('UEQ_',8),rep('PVT_nl',8),rep(1:8)),
                 paste0(rep('SBS_',3),rep('PVT_nl',3),rep(1:3)))

ver1_nl <- data[data$version == 1, col_v1_nl]
colnames(ver1_nl)[3:length(ver1_nl)] <- col_repl_nl

##### Version 2
col_v2_nl <- col_v1_l

ver2_nl <- data[data$version == 2, col_v2_nl]
colnames(ver2_nl)[3:length(ver2_nl)] <- col_repl_nl

col_nl <- rbind(ver1_nl,ver2_nl)

### Implement VAS_(non)light column in data
data <- merge(data, col_l[,c('ID',col_repl_l)], by = "ID", all.x = TRUE)
data <- merge(data, col_nl[,c('ID',col_repl_nl)], by = "ID", all.x = TRUE)

### Drop Unused Columns
remove_cols <- c(col_v1_l[3:length(col_v1_l)], col_v1_nl[3:length(col_v1_nl)])
data = subset(data, select = !(colnames(data) %in% remove_cols))

# Clean the environment
rm(list = ls()[!ls() %in% "data"])

### BART
#### Light
##### Version 1
col_v1_l <- c('ID', 'version')
for (name in colnames(data)){
  if (str_detect(name, "KAB5") |
      str_detect(name, "KAB6") |
      str_detect(name, "UEQ2") |
      str_detect(name, "StoerBS2")){
    col_v1_l <- c(col_v1_l, name)
  }
}

col_repl_l <- c(paste0(rep('KAB_pre_',6),rep('BART_l_',6),rep(1:6)),
                paste0(rep('KAB_pos_',6),rep('BART_l_',6),rep(1:6)),
                paste0(rep('UEQ_',8),rep('BART_l_',8),rep(1:8)),
                paste0(rep('SBS_',3),rep('BART_l_',3),rep(1:3)))

ver1_l <- data[data$version == 1, col_v1_l]
colnames(ver1_l)[3:length(ver1_l)] <- col_repl_l

##### Version 2
col_v2_l <- c('ID', 'version')
for (name in colnames(data)){
  if (str_detect(name, "KAB9") |
      str_detect(name, "KAB10") |
      str_detect(name, "UEQ4") |
      str_detect(name, "StoerBS4")){
    col_v2_l <- c(col_v2_l, name)
  }
}

ver2_l <- data[data$version == 2, col_v2_l]
colnames(ver2_l)[3:length(ver2_l)] <- col_repl_l

col_l <- rbind(ver1_l,ver2_l)

#### Non-Light
##### Version 1
col_v1_nl <- col_v2_l

col_repl_nl <- c(paste0(rep('KAB_pre_',6),rep('BART_nl_',6),rep(1:6)),
                 paste0(rep('KAB_pos_',6),rep('BART_nl_',6),rep(1:6)),
                 paste0(rep('UEQ_',8),rep('BART_nl_',8),rep(1:8)),
                 paste0(rep('SBS_',3),rep('BART_nl_',3),rep(1:3)))

ver1_nl <- data[data$version == 1, col_v1_nl]
colnames(ver1_nl)[3:length(ver1_nl)] <- col_repl_nl

##### Version 2
col_v2_nl <- col_v1_l

ver2_nl <- data[data$version == 2, col_v2_nl]
colnames(ver2_nl)[3:length(ver2_nl)] <- col_repl_nl

col_nl <- rbind(ver1_nl,ver2_nl)

### Implement VAS_(non)light column in data
data <- merge(data, col_l[,c('ID',col_repl_l)], by = "ID", all.x = TRUE)
data <- merge(data, col_nl[,c('ID',col_repl_nl)], by = "ID", all.x = TRUE)

### Drop Unused Columns
remove_cols <- c(col_v1_l[3:length(col_v1_l)], col_v1_nl[3:length(col_v1_nl)])
data = subset(data, select = !(colnames(data) %in% remove_cols))

# Clean the environment
rm(list = ls()[!ls() %in% "data"])

# Save Final File ####
write.csv(data, paste0(getwd(),'/questionnaire/final_transformed.csv'), row.names = FALSE)
write_xlsx(data, paste0(getwd(),'/questionnaire/final_transformed.xlsx'))
