#### Intera procedura: Index ####
# 1) Importazione .DAT e LOG; pulizia e join dei due dataframe, modifica in metri 
# 2) Pulizia spikes da colonna laser_altitude_m con creazione colonna laser_altitude_m_cleaned
# 3) Correggo tilt con 0 a 5° da tilt_deg a tilt_corrected
# 4) Applicazione correzione tilt_corrected a laser_altitude_m_cleaned in una nuova colonna laser_altitude_m_corrected
# 5) Calcola Gap e stats
# 6) Normalizza la differenza punto per punto, volo per volo
# 7) Plotta e fai regressione lineare

#### 1) Importazione .DAT e LOG; pulizia e join dei due dataframe, modifica in metri ####

## Packages 
library(pastecs)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hms)
library(stringr)
library(tidyverse)
library(tidyr)

## functions
spike_cleaning <- function(data, column_name, new_column_name, lower_limit, upper_limit, omit_first_n = 0, omit_last_n = 0) {
  result <- data
  result[[new_column_name]] <- NA  # Crea la nuova colonna inizialmente con NA
  
  for (i in 1:length(data[[column_name]])) {
    if (i > omit_first_n && i <= (length(data[[column_name]]) - omit_last_n)) {
      value <- data[[column_name]][i]
      
      # Se il valore è un outlier, sostituisci con NA, altrimenti assegna il valore originale
      if (value >= lower_limit && value <= upper_limit) {
        result[[new_column_name]][i] <- value
      }
    }
  }
  
  return(result)
}
#FLY304_REC_MOT_LD_1.2 <- spike_cleaning(FLY304_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 2, upper_limit = 60, omit_first_n = 5, omit_last_n = 5)

correct_altitude <- function(dataset) {
  if (!("laser_altitude_m_cleaned" %in% colnames(dataset)) ||
      !("tilt_corrected" %in% colnames(dataset))) {
    stop("Le colonne richieste non sono presenti nel dataset.")
  }
  
  dataset$laser_altitude_m_corrected <- ifelse(
    is.na(dataset$laser_altitude_m_cleaned) | is.na(dataset$tilt_corrected),
    NA,  # Imposta il valore su NA se uno dei valori è NA
    round(dataset$laser_altitude_m * cos(dataset$tilt_corrected * pi / 180), 2)
  )
  return(dataset)
}
#FLY304_REC_MOT_LD_1.2 <- correct_altitude(FLY304_REC_MOT_LD_1.2)

GAP3 <- function(data, col1, col2, index_ranges) {
  # Verifica se le colonne col1 e col2 sono presenti nei dati
  if (!(col1 %in% names(data)) || !(col2 %in% names(data))) {
    stop("Le colonne specificate non sono presenti nei dati.")
  }
  
  # Inizializza una colonna per le differenze
  data$diff_col <- NA
  
  # Calcola le differenze per ciascun intervallo specificato
  for (range in index_ranges) {
    start_index <- range[1]
    end_index <- range[2]
    
    # Verifica se gli indici sono validi e se ci sono dati nel range specificato
    if (!is.na(start_index) && !is.na(end_index) &&
        start_index <= end_index &&
        start_index >= 1 && end_index <= nrow(data) &&
        !all(is.na(data[[col1]][start_index:end_index])) &&
        !all(is.na(data[[col2]][start_index:end_index]))) {
      
      # Calcola le differenze nel range specificato
      differences <- data[[col1]][start_index:end_index] - data[[col2]][start_index:end_index]
      
      # Assegna le differenze alla colonna diff_col
      data$diff_col[start_index:end_index] <- differences
    } else {
      warning("Intervallo non valido: ", paste("start_index =", start_index, ", end_index =", end_index))
    }
  }
  
  # Restituisci il dataset modificato
  return(data)
}
#FLY304_REC_MOT_LD_1.2 <- GAP3(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(265, 451), c(481, 541), c(571, 591), c(611, 671), c(691, 771),c(861, 911)))

calculate_smoothed_diff <- function(vector) {
  result <- rep(NA, length(vector))
  
  for (i in 3:(length(vector) - 2)) {
    if (!is.na(vector[i])) {
      if (is.na(vector[i - 1]) && is.na(vector[i - 2]) && is.na(vector[i - 3])) {
        result[i] <- vector[i]
      } else if (!is.na(vector[i - 1]) && is.na(vector[i - 2]) && is.na(vector[i - 3])) {
        result[i] <- (vector[i - 1] + vector[i]) / 2
      } else if (is.na(vector[i - 1]) && is.na(vector[i - 2]) && !is.na(vector[i + 1]) && !is.na(vector[i + 2])) {
        result[i] <- (vector[i + 1] + vector[i + 2]) / 2
      } else if (!is.na(vector[i - 1]) && !is.na(vector[i - 2]) && is.na(vector[i + 1]) && is.na(vector[i + 2])) {
        result[i] <- (vector[i - 1] + vector[i - 2]) / 2
      } else {
        result[i] <- (vector[i - 1] + vector[i] + vector[i + 1]) / 3
      }
    }
  }
  
  return(result)
}
#FLY304_REC_MOT_LD_1.2$diff_smooth <- calculate_smoothed_diff(FLY304_REC_MOT_LD_1.2$diff_col)

## Importing

# Import CSV 
FLY304_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY304_REC_MOT.csv")
# Import LOG 
LOG_0030 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0030.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY304_REC_MOT_clean <- distinct(FLY304_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY304_REC_MOT_ready <- FLY304_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0030_R <- transform(LOG_0030, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0030_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0030_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY304_REC_MOT_LD <- left_join(FLY304_REC_MOT_ready, LOG_0030_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY304_REC_MOT_LD_1 <- FLY304_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

#### 2) Pulizia spikes da colonna laser_altitude_m con creazione colonna laser_altitude_m_cleaned ####

spike_cleaning <- function(data, column_name, new_column_name, lower_limit, upper_limit, omit_first_n = 0, omit_last_n = 0) {
  result <- data
  result[[new_column_name]] <- NA  # Create the new column initially with NA
  
  for (i in 1:length(data[[column_name]])) {
    if (i > omit_first_n && i <= (length(data[[column_name]]) - omit_last_n)) {
      value <- data[[column_name]][i]
      
      # Check for missing value and if the value is an outlier, replace with NA, otherwise assign the original value
      if (!is.na(value) && value >= lower_limit && value <= upper_limit) {
        result[[new_column_name]][i] <- value
      }
    }
  }
  
  return(result)
}

FLY304_REC_MOT_LD_1.2 <- spike_cleaning(FLY304_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 2, upper_limit = 60, omit_first_n = 5, omit_last_n = 5)

# check results by plotting
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

#### 3) Correggo tilt con 0 a 5° da tilt_deg a tilt_corrected ####

# Aggiungi una colonna per la correzione del tilt_deg >> tilt_corrected
FLY304_REC_MOT_LD_1.2$tilt_corrected <- FLY304_REC_MOT_LD_1.2$tilt_deg - 5
FLY304_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY304_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY304_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY304_REC_MOT_LD_1.2$tilt_corrected, FLY304_REC_MOT_LD_1.2$tilt_corrected)
)

#### 4) Applicazione correzione tilt_corrected a laser_altitude_m_cleaned in una nuova colonna laser_altitude_m_corrected ####

correct_altitude <- function(dataset) {
  if (!("laser_altitude_m_cleaned" %in% colnames(dataset)) ||
      !("tilt_corrected" %in% colnames(dataset))) {
    stop("Le colonne richieste non sono presenti nel dataset.")
  }
  
  dataset$laser_altitude_m_corrected <- ifelse(
    is.na(dataset$laser_altitude_m_cleaned) | is.na(dataset$tilt_corrected),
    NA,  # Imposta il valore su NA se uno dei valori è NA
    round(dataset$laser_altitude_m * cos(dataset$tilt_corrected * pi / 180), 2)
  )
  return(dataset)
}

# apply function
FLY304_REC_MOT_LD_1.2 <- correct_altitude(FLY304_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY304_REC_MOT_LD_1.2), by = 20))


#### 5) Calcola Gap 
GAP3 <- function(data, col1, col2, index_ranges) {
  # Verifica se le colonne col1 e col2 sono presenti nei dati
  if (!(col1 %in% names(data)) || !(col2 %in% names(data))) {
    stop("Le colonne specificate non sono presenti nei dati.")
  }
  
  # Inizializza una colonna per le differenze
  data$diff_col <- NA
  
  # Calcola le differenze per ciascun intervallo specificato
  for (range in index_ranges) {
    start_index <- range[1]
    end_index <- range[2]
    
    # Verifica se gli indici sono validi e se ci sono dati nel range specificato
    if (!is.na(start_index) && !is.na(end_index) &&
        start_index <= end_index &&
        start_index >= 1 && end_index <= nrow(data) &&
        !all(is.na(data[[col1]][start_index:end_index])) &&
        !all(is.na(data[[col2]][start_index:end_index]))) {
      
      # Calcola le differenze nel range specificato
      differences <- data[[col1]][start_index:end_index] - data[[col2]][start_index:end_index]
      
      # Assegna le differenze alla colonna diff_col
      data$diff_col[start_index:end_index] <- differences
    } else {
      warning("Intervallo non valido: ", paste("start_index =", start_index, ", end_index =", end_index))
    }
  }
  
  # Restituisci il dataset modificato
  return(data)
}

# Esempio di utilizzo della funzione con una lista di intervalli
FLY304_REC_MOT_LD_1.2 <- GAP3(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(265, 451), c(481, 541), c(571, 591), c(611, 671), c(691, 771),c(861, 911)))
FLY304_REC_MOT_LD_1.2$diff_col
mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

#### 6) Normalizza la differenza punto per punto, volo per volo ####

# Plot the differences
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

# differenza relativa
FLY304_REC_MOT_LD_1.2$relative_diff <- FLY304_REC_MOT_LD_1.2$diff_col / mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

#### 7) Plot cloud
df_FLY304 <- FLY304_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY304 <- df_FLY304 %>% rename(seconds_FLY304 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY304 <- df_FLY304 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY304, aes(x = seconds_FLY304)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()


# ------------------------------ all flights --------------------- #

#### FLY_298 + LOG0028 ####

# Import csv
FLY298_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY298_REC_MOT.csv")
# Import log
LOG_0028 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0028.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY298_REC_MOT_clean <- distinct(FLY298_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY298_REC_MOT_ready <- FLY298_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0028_R <- transform(LOG_0028, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0028_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0028_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY298_REC_MOT_LD <- left_join(FLY298_REC_MOT_ready, LOG_0028_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY298_REC_MOT_LD_1 <- FLY298_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY298_REC_MOT_LD_1.2 <- FLY298_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY298_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0, head(FLY298_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -2))

# spike cleaning
FLY298_REC_MOT_LD_1.2 <- spike_cleaning(FLY298_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 77, omit_last_n = 10)

# check results by plotting
ggplot(FLY298_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-16 06:19:01 (FLY_304 LOG_0028)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# tilt correction 
FLY298_REC_MOT_LD_1.2$tilt_corrected <- FLY298_REC_MOT_LD_1.2$tilt_deg - 5
FLY298_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY298_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY298_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY298_REC_MOT_LD_1.2$tilt_corrected, FLY298_REC_MOT_LD_1.2$tilt_corrected)
)

# create lider_altitude_m_corrected
FLY298_REC_MOT_LD_1.2 <- correct_altitude(FLY298_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY298_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-16 06:19:01 (FLY_298 LOG_0028)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY298_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY298_REC_MOT_LD_1.2 <- GAP3(FLY298_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(111, 191), c(211, 561)))
FLY298_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto
ggplot(FLY298_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_298 <- sd(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_298 <- sd(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY298_REC_MOT_LD_1.2$relative_diff <- FLY298_REC_MOT_LD_1.2$diff_col / mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY298_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY298_REC_MOT_LD_1.2 <- FLY298_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud
df_FLY298 <- FLY298_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY298 <- df_FLY298 %>% rename(seconds_FLY298 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY298 <- df_FLY298 %>% rename_all(~sub("relative_diff_", "", .))

ggplot(df_FLY298, aes(x = seconds_FLY298)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal() 

#### FLY_299 + LOG0029 ####

# Import csv
FLY299_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY299_REC_MOT.csv")
# Import log
LOG_0029 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0029.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY299_REC_MOT_clean <- distinct(FLY299_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY299_REC_MOT_ready <- FLY299_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0029_R <- transform(LOG_0029, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0029_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0029_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY299_REC_MOT_LD <- left_join(FLY299_REC_MOT_ready, LOG_0029_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY299_REC_MOT_LD_1 <- FLY299_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY299_REC_MOT_LD_1.2 <- FLY299_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# spike cleaning
FLY299_REC_MOT_LD_1.2 <- spike_cleaning(FLY299_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 44, omit_last_n = 10)

# check results by plotting
ggplot(FLY299_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-16 07:20:01 (FLY_299 LOG0029)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# tilt correction 
FLY299_REC_MOT_LD_1.2$tilt_corrected <- FLY299_REC_MOT_LD_1.2$tilt_deg - 5
FLY299_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY299_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY299_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY299_REC_MOT_LD_1.2$tilt_corrected, FLY299_REC_MOT_LD_1.2$tilt_corrected)
)

# create lider_altitude_m_corrected
FLY299_REC_MOT_LD_1.2 <- correct_altitude(FLY299_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY299_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-16 07:20:01 (FLY_299 LOG_0029)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY299_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY299_REC_MOT_LD_1.2 <- GAP3(FLY299_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(81, 651), c(671, 681)))
FLY299_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto
ggplot(FLY299_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_299 <- sd(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_299 <- sd(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY299_REC_MOT_LD_1.2$relative_diff <- FLY299_REC_MOT_LD_1.2$diff_col / mean(FLY299_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY299_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY299_REC_MOT_LD_1.2 <- FLY299_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud
df_FLY299 <- FLY299_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY299 <- df_FLY299 %>% rename(seconds_FLY299 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY299 <- df_FLY299 %>% rename_all(~sub("relative_diff_", "", .))

ggplot(df_FLY299, aes(x = seconds_FLY299)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal() 

#### FLY_304 + LOG_0030 ####

# Import CSV 
FLY304_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY304_REC_MOT.csv")
# Import LOG 
LOG_0030 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0030.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY304_REC_MOT_clean <- distinct(FLY304_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY304_REC_MOT_ready <- FLY304_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0030_R <- transform(LOG_0030, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0030_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0030_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY304_REC_MOT_LD <- left_join(FLY304_REC_MOT_ready, LOG_0030_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY304_REC_MOT_LD_1 <- FLY304_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY304_REC_MOT_LD_1.2 <- spike_cleaning(FLY304_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 2, upper_limit = 60, omit_first_n = 5, omit_last_n = 5)

# check results by plotting
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY304_REC_MOT_LD_1.2$tilt_corrected <- FLY304_REC_MOT_LD_1.2$tilt_deg - 5
FLY304_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY304_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY304_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY304_REC_MOT_LD_1.2$tilt_corrected, FLY304_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY304_REC_MOT_LD_1.2 <- correct_altitude(FLY304_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY304_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY304_REC_MOT_LD_1.2 <- GAP3(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(265, 451), c(481, 541), c(571, 591), c(611, 671), c(691, 771),c(861, 911)))
FLY304_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_304 <- sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_304 <- sd(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)


# differenza relativa
FLY304_REC_MOT_LD_1.2$relative_diff <- FLY304_REC_MOT_LD_1.2$diff_col / mean(FLY304_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY304 <- FLY304_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY304 <- df_FLY304 %>% rename(seconds_FLY304 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY304 <- df_FLY304 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY304, aes(x = seconds_FLY304)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_306 + LOG_0031 ####

# Import CSV 
FLY306_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY306_REC_MOT.csv")
# Import LOG 
LOG_0031 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0031.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY306_REC_MOT_clean <- distinct(FLY306_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY306_REC_MOT_ready <- FLY306_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0031_R <- transform(LOG_0031, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0031_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0031_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY306_REC_MOT_LD <- left_join(FLY306_REC_MOT_ready, LOG_0031_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY306_REC_MOT_LD_1 <- FLY306_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY306_REC_MOT_LD_1.2 <- FLY306_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY306_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY306_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY306_REC_MOT_LD_1.2 <- spike_cleaning(FLY306_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 0, upper_limit = 60, omit_first_n = 50, omit_last_n = 10)

# check results by plotting
ggplot(FLY306_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 14:05:05 (FLY_306 LOG_0031)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY306_REC_MOT_LD_1.2$tilt_corrected <- FLY306_REC_MOT_LD_1.2$tilt_deg - 5
FLY306_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY306_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY306_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY306_REC_MOT_LD_1.2$tilt_corrected, FLY306_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY306_REC_MOT_LD_1.2 <- correct_altitude(FLY306_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY306_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 14:05:051 (FLY_306 LOG_0031)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY306_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY306_REC_MOT_LD_1.2 <- GAP3(FLY306_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(71, 141), c(151, 161), c(201, 291)))
FLY306_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY306_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_306 <- sd(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_306 <- sd(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)


# differenza relativa
FLY306_REC_MOT_LD_1.2$relative_diff <- FLY306_REC_MOT_LD_1.2$diff_col / mean(FLY306_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY306_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY306_REC_MOT_LD_1.2 <- FLY306_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY306 <- FLY306_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY306 <- df_FLY306 %>% rename(seconds_FLY306 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY306 <- df_FLY306 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY306, aes(x = seconds_FLY306)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_307 + LOG_0032 ####

FLY307_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY307_REC_MOT.csv")
# Import LOG 
LOG_0032 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0032.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY307_REC_MOT_clean <- distinct(FLY307_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY307_REC_MOT_ready <- FLY307_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0032_R <- transform(LOG_0032, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0032_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0032_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY307_REC_MOT_LD <- left_join(FLY307_REC_MOT_ready, LOG_0032_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY307_REC_MOT_LD_1 <- FLY307_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY307_REC_MOT_LD_1.2 <- FLY307_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY307_REC_MOT_LD_1.2 <- spike_cleaning(FLY307_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 20, upper_limit = 60, omit_first_n = 5, omit_last_n = 10)

# check results by plotting
ggplot(FLY307_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 15:14:31 (FLY_307 LOG_0032)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY307_REC_MOT_LD_1.2$tilt_corrected <- FLY307_REC_MOT_LD_1.2$tilt_deg - 5
FLY307_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY307_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY307_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY307_REC_MOT_LD_1.2$tilt_corrected, FLY307_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY307_REC_MOT_LD_1.2 <- correct_altitude(FLY307_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY307_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 15:14:31 (FLY_307 LOG_0032)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY307_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY307_REC_MOT_LD_1.2 <- GAP3(FLY307_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(61, 101), c(151, 231), c(251, 291), c(311, 331), c(401, 431), c(471, 501) ))
FLY307_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY307_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_307 <- sd(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_307 <- sd(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)



# differenza relativa
FLY307_REC_MOT_LD_1.2$relative_diff <- FLY307_REC_MOT_LD_1.2$diff_col / mean(FLY307_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY307_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY307_REC_MOT_LD_1.2 <- FLY307_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY307 <- FLY307_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY307 <- df_FLY307 %>% rename(seconds_FLY307 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY307 <- df_FLY307 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY307, aes(x = seconds_FLY307)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_310 + LOG_0034 ####

# Import csv
FLY310_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY310_REC_MOT.csv")
# Import LOG 
LOG_0034 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0034.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY310_REC_MOT_clean <- distinct(FLY310_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY310_REC_MOT_ready <- FLY310_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0034_R <- transform(LOG_0034, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0034_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0034_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY310_REC_MOT_LD <- left_join(FLY310_REC_MOT_ready, LOG_0034_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY310_REC_MOT_LD_1 <- FLY310_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY310_REC_MOT_LD_1.2 <- FLY310_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY310_REC_MOT_LD_1.2 <- spike_cleaning(FLY310_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 22, omit_last_n = 30)

# check results by plotting
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 11:17:31 (FLY_310 LOG_0034)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY310_REC_MOT_LD_1.2$tilt_corrected <- FLY310_REC_MOT_LD_1.2$tilt_deg - 5
FLY310_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY310_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY310_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY310_REC_MOT_LD_1.2$tilt_corrected, FLY310_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY310_REC_MOT_LD_1.2 <- correct_altitude(FLY310_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 11:17:31 (FLY_310 LOG_0034)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY310_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY310_REC_MOT_LD_1.2 <- GAP3(FLY310_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(71, 191), c(211, 381), c(491, 561), c(571, 611)))
FLY310_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_310 <-sd(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_310 <- sd(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY310_REC_MOT_LD_1.2$relative_diff <- FLY310_REC_MOT_LD_1.2$diff_col / mean(FLY310_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY310_REC_MOT_LD_1.2 <- FLY310_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY310 <- FLY310_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY310 <- df_FLY310 %>% rename(seconds_FLY310 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY310 <- df_FLY310 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY310, aes(x = seconds_FLY310)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_311 + LOG_0035 ####

# Import csv
FLY311_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY311_REC_MOT.csv")
# Import LOG 
LOG_0035 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0035.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY311_REC_MOT_clean <- distinct(FLY311_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY311_REC_MOT_ready <- FLY311_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0035_R <- transform(LOG_0035, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0035_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0035_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY311_REC_MOT_LD <- left_join(FLY311_REC_MOT_ready, LOG_0035_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY311_REC_MOT_LD_1 <- FLY311_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY311_REC_MOT_LD_1.2 <- FLY311_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY311_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY311_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY311_REC_MOT_LD_1.2 <- spike_cleaning(FLY311_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 4, upper_limit = 60, omit_first_n = 10, omit_last_n = 10)

# check results by plotting
ggplot(FLY311_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 12:34:10 (FLY_311)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY311_REC_MOT_LD_1.2$tilt_corrected <- FLY311_REC_MOT_LD_1.2$tilt_deg - 5
FLY311_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY311_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY311_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY311_REC_MOT_LD_1.2$tilt_corrected, FLY311_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY311_REC_MOT_LD_1.2 <- correct_altitude(FLY311_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY311_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 12:34:10 (FLY_311)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY311_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY311_REC_MOT_LD_1.2 <- GAP3(FLY311_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 41), c(61, 111), c(141, 161), c(181, 361), c(381, 471)))
FLY311_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY311_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_311 <- sd(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_311 <- sd(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY311_REC_MOT_LD_1.2$relative_diff <- FLY311_REC_MOT_LD_1.2$diff_col / mean(FLY311_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY311_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY311_REC_MOT_LD_1.2 <- FLY311_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY311 <- FLY311_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY311 <- df_FLY311 %>% rename(seconds_FLY311 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY311 <- df_FLY311 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY311, aes(x = seconds_FLY311)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_312 + LOG_0036 #### 

# Import csv
FLY312_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY312_REC_MOT.csv")
# Import LOG 
LOG_0036 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0036.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY312_REC_MOT_clean <- distinct(FLY312_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY312_REC_MOT_ready <- FLY312_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0036_R <- transform(LOG_0036, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0036_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0036_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY312_REC_MOT_LD <- left_join(FLY312_REC_MOT_ready, LOG_0036_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY312_REC_MOT_LD_1 <- FLY312_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY312_REC_MOT_LD_1.2 <- FLY312_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY312_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY312_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY312_REC_MOT_LD_1.2 <- spike_cleaning(FLY312_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 15, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY312_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-12 08:54:10 (FLY_312)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY312_REC_MOT_LD_1.2$tilt_corrected <- FLY312_REC_MOT_LD_1.2$tilt_deg - 5
FLY312_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY312_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY312_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY312_REC_MOT_LD_1.2$tilt_corrected, FLY312_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY312_REC_MOT_LD_1.2 <- correct_altitude(FLY312_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY312_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-12 08:54:10 (FLY_312)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY312_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY312_REC_MOT_LD_1.2 <- GAP3(FLY312_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(61, 71), c(91, 221)))
FLY312_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY312_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_312 <- sd(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_312 <- sd(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)


# differenza relativa
FLY312_REC_MOT_LD_1.2$relative_diff <- FLY312_REC_MOT_LD_1.2$diff_col / mean(FLY312_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY312_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY312_REC_MOT_LD_1.2 <- FLY312_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY312 <- FLY312_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY312 <- df_FLY312 %>% rename(seconds_FLY312 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY312 <- df_FLY312 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY312, aes(x = seconds_FLY312)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_313 + LOG_0037 ####

# Import csv
FLY313_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY313_REC_MOT.csv")
# Import LOG 
LOG_0037 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0037.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY313_REC_MOT_clean <- distinct(FLY313_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY313_REC_MOT_ready <- FLY313_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0037_R <- transform(LOG_0037, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0037_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0037_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY313_REC_MOT_LD <- left_join(FLY313_REC_MOT_ready, LOG_0037_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY313_REC_MOT_LD_1 <- FLY313_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY313_REC_MOT_LD_1.2 <- FLY313_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY313_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY313_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY313_REC_MOT_LD_1.2 <- spike_cleaning(FLY313_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 55, omit_last_n = 5)

# check results by plotting
ggplot(FLY313_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 09:05:38 (FLY_313 LOG_0037)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY313_REC_MOT_LD_1.2$tilt_corrected <- FLY313_REC_MOT_LD_1.2$tilt_deg - 5
FLY313_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY313_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY313_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY313_REC_MOT_LD_1.2$tilt_corrected, FLY313_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY313_REC_MOT_LD_1.2 <- correct_altitude(FLY313_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY313_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-10 09:05:38 (FLY_313 LOG_0037)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY313_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY313_REC_MOT_LD_1.2 <- GAP3(FLY313_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(101, 181), c(195, 201)))
FLY313_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY313_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_313 <- sd(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_313 <- sd(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY313_REC_MOT_LD_1.2$relative_diff <- FLY313_REC_MOT_LD_1.2$diff_col / mean(FLY313_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY313_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY313_REC_MOT_LD_1.2 <- FLY313_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY313 <- FLY313_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY313 <- df_FLY313 %>% rename(seconds_FLY313 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY313 <- df_FLY313 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY313, aes(x = seconds_FLY313)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_315 + LOG_0038 ####

# Import csv
FLY315_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY315_REC_MOT.csv")
# Import LOG 
LOG_0038 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0038.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY315_REC_MOT_clean <- distinct(FLY315_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY315_REC_MOT_ready <- FLY315_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0038_R <- transform(LOG_0038, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0038_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0038_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY315_REC_MOT_LD <- left_join(FLY315_REC_MOT_ready, LOG_0038_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY315_REC_MOT_LD_1 <- FLY315_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY315_REC_MOT_LD_1.2 <- FLY315_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY315_REC_MOT_LD_1.2 <- spike_cleaning(FLY315_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 38, omit_last_n = 5)

# check results by plotting
ggplot(FLY315_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 16:38:25 (FLY_315 LOG_0038)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY315_REC_MOT_LD_1.2$tilt_corrected <- FLY315_REC_MOT_LD_1.2$tilt_deg - 5
FLY315_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY315_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY315_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY315_REC_MOT_LD_1.2$tilt_corrected, FLY315_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY315_REC_MOT_LD_1.2 <- correct_altitude(FLY315_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY315_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 16:38:25 (FLY_315 LOG_0038)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY315_REC_MOT_LD_1.2), by = 20))


# Calcola Gap 
FLY315_REC_MOT_LD_1.2 <- GAP3(FLY315_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(71, 121), c(171, 318), c(331, 391)))
FLY315_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY315_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_315 <- sd(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_315 <- sd(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY315_REC_MOT_LD_1.2$relative_diff <- FLY315_REC_MOT_LD_1.2$diff_col / mean(FLY315_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY315_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY315_REC_MOT_LD_1.2 <- FLY315_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY315 <- FLY315_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY315 <- df_FLY315 %>% rename(seconds_FLY315 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY315 <- df_FLY315 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY315, aes(x = seconds_FLY315)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

####FLY_316 + LOG_0039 ####

# Import csv
FLY316_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY316_REC_MOT.csv")
# Import LOG 
LOG_0039 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0039.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY316_REC_MOT_clean <- distinct(FLY316_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY316_REC_MOT_ready <- FLY316_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0039_R <- transform(LOG_0039, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0039_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0039_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY316_REC_MOT_LD <- left_join(FLY316_REC_MOT_ready, LOG_0039_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY316_REC_MOT_LD_1 <- FLY316_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY316_REC_MOT_LD_1.2 <- FLY316_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY316_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY316_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY316_REC_MOT_LD_1.2 <- spike_cleaning(FLY316_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 20, omit_last_n = 5)

# check results by plotting
ggplot(FLY316_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 17:23:33 (FLY_316 LOG_0039)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY316_REC_MOT_LD_1.2$tilt_corrected <- FLY316_REC_MOT_LD_1.2$tilt_deg - 5
FLY316_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY316_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY316_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY316_REC_MOT_LD_1.2$tilt_corrected, FLY316_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY316_REC_MOT_LD_1.2 <- correct_altitude(FLY316_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY316_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 17:23:33 (FLY_316 LOG_0039)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY316_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY316_REC_MOT_LD_1.2 <- GAP3(FLY316_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(101, 181)))
FLY316_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY316_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_316 <- sd(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_316 <- sd(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY316_REC_MOT_LD_1.2$relative_diff <- FLY316_REC_MOT_LD_1.2$diff_col / mean(FLY316_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY316_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY316_REC_MOT_LD_1.2 <- FLY316_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY316 <- FLY316_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY316 <- df_FLY316 %>% rename(seconds_FLY316 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY316 <- df_FLY316 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY316, aes(x = seconds_FLY316)) +
  geom_point(aes(y = relative_diff), color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_317 + LOG_0040 ####

# Import csv
FLY317_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY317_REC_MOT.csv")
# Import LOG 
LOG_0040 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0040.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY317_REC_MOT_clean <- distinct(FLY317_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY317_REC_MOT_ready <- FLY317_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0040_R <- transform(LOG_0040, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0040_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0040_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY317_REC_MOT_LD <- left_join(FLY317_REC_MOT_ready, LOG_0040_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY317_REC_MOT_LD_1 <- FLY317_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY317_REC_MOT_LD_1.2 <- FLY317_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY317_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0, head(FLY317_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -2))

# Pulizia spikes 
FLY317_REC_MOT_LD_1.2 <- spike_cleaning(FLY317_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 5)

# check results by plotting
ggplot(FLY317_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 18:04:20 (FLY_317 LOG_0040)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY317_REC_MOT_LD_1.2$tilt_corrected <- FLY317_REC_MOT_LD_1.2$tilt_deg - 5
FLY317_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY317_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY317_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY317_REC_MOT_LD_1.2$tilt_corrected, FLY317_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY317_REC_MOT_LD_1.2 <- correct_altitude(FLY317_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY317_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-13 18:04:20 (FLY_317 LOG_0040)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY317_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY317_REC_MOT_LD_1.2 <- GAP3(FLY317_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(81, 271), c(311, 431)))
FLY317_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY317_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_317<- sd(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_317<- sd(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY317_REC_MOT_LD_1.2$relative_diff <- FLY317_REC_MOT_LD_1.2$diff_col / mean(FLY317_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY317_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY317_REC_MOT_LD_1.2 <- FLY317_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY317 <- FLY317_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY317 <- df_FLY317 %>% rename(seconds_FLY317 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY317 <- df_FLY317 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY317, aes(x = seconds_FLY317, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_318 + LOG_0041 ####

# Import csv
FLY318_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY318_REC_MOT.csv")
# Import LOG 
LOG_0041 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0041.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY318_REC_MOT_clean <- distinct(FLY318_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY318_REC_MOT_ready <- FLY318_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0041_R <- transform(LOG_0041, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0041_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0041_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY318_REC_MOT_LD <- left_join(FLY318_REC_MOT_ready, LOG_0041_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY318_REC_MOT_LD_1 <- FLY318_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY318_REC_MOT_LD_1.2 <- FLY318_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY318_REC_MOT_LD_1.2 <- spike_cleaning(FLY318_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 10, omit_last_n = 5)

# check results by plotting
ggplot(FLY318_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-14 13:13:38 (FLY_318 LOG_0041)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY318_REC_MOT_LD_1.2$tilt_corrected <- FLY318_REC_MOT_LD_1.2$tilt_deg - 5
FLY318_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY318_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY318_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY318_REC_MOT_LD_1.2$tilt_corrected, FLY318_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY318_REC_MOT_LD_1.2 <- correct_altitude(FLY318_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY318_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-14 13:13:38 (FLY_318 LOG_0041)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY318_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY318_REC_MOT_LD_1.2 <- GAP3(FLY318_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(41, 195)))
FLY318_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY318_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_318<- sd(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_318<- sd(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY318_REC_MOT_LD_1.2$relative_diff <- FLY318_REC_MOT_LD_1.2$diff_col / mean(FLY318_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY318_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY318_REC_MOT_LD_1.2 <- FLY318_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY318 <- FLY318_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY318 <- df_FLY318 %>% rename(seconds_FLY318 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY318 <- df_FLY318 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY318, aes(x = seconds_FLY318, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_319 + LOG_0042 ####

# Import csv
FLY319_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY319_REC_MOT.csv")
# Import LOG 
LOG_0042 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0042.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY319_REC_MOT_clean <- distinct(FLY319_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY319_REC_MOT_ready <- FLY319_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0042_R <- transform(LOG_0042, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0042_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0042_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY319_REC_MOT_LD <- left_join(FLY319_REC_MOT_ready, LOG_0042_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY319_REC_MOT_LD_1 <- FLY319_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY319_REC_MOT_LD_1.2 <- FLY319_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY319_REC_MOT_LD_1.2 <- spike_cleaning(FLY319_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 1, upper_limit = 60, omit_first_n = 124, omit_last_n = 0)

# check results by plotting
ggplot(FLY319_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-14 13:29:54 (FLY_319 LOG_0042)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY319_REC_MOT_LD_1.2$tilt_corrected <- FLY319_REC_MOT_LD_1.2$tilt_deg - 5
FLY319_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY319_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY319_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY319_REC_MOT_LD_1.2$tilt_corrected, FLY319_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY319_REC_MOT_LD_1.2 <- correct_altitude(FLY319_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY319_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-14 13:29:54 (FLY_319 LOG_0042)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY319_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY319_REC_MOT_LD_1.2 <- GAP3(FLY319_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(161, 191), c(201,211), c(261,311)))
FLY319_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY319_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_319<- sd(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_319<- sd(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY319_REC_MOT_LD_1.2$relative_diff <- FLY319_REC_MOT_LD_1.2$diff_col / mean(FLY319_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY319_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY319_REC_MOT_LD_1.2 <- FLY319_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY319 <- FLY319_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY319 <- df_FLY319 %>% rename(seconds_FLY319 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY319 <- df_FLY319 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY319, aes(x = seconds_FLY319, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_323 + LOG_0045 ####
# Import csv
FLY323_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY323_REC_MOT.csv")
# Import LOG 
LOG_0045 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0045.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY323_REC_MOT_clean <- distinct(FLY323_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY323_REC_MOT_ready <- FLY323_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0045_R <- transform(LOG_0045, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0045_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0045_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY323_REC_MOT_LD <- left_join(FLY323_REC_MOT_ready, LOG_0045_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY323_REC_MOT_LD_1 <- FLY323_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY323_REC_MOT_LD_1.2 <- FLY323_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY323_REC_MOT_LD_1.2 <- spike_cleaning(FLY323_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 280, omit_last_n = 5)

# check results by plotting
ggplot(FLY323_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-20 16:00:52 (FLY_323 LOG_0045)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY323_REC_MOT_LD_1.2$tilt_corrected <- FLY323_REC_MOT_LD_1.2$tilt_deg - 5
FLY323_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY323_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY323_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY323_REC_MOT_LD_1.2$tilt_corrected, FLY323_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY323_REC_MOT_LD_1.2 <- correct_altitude(FLY323_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY323_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-20 16:00:52 (FLY_323 LOG_0045)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY323_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY323_REC_MOT_LD_1.2 <- GAP3(FLY323_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(321, 431), c(441,451)))
FLY323_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY323_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_323<- sd(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_323<- sd(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY323_REC_MOT_LD_1.2$relative_diff <- FLY323_REC_MOT_LD_1.2$diff_col / mean(FLY323_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY323_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY323_REC_MOT_LD_1.2 <- FLY323_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY323 <- FLY323_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY323 <- df_FLY323 %>% rename(seconds_FLY323 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY323 <- df_FLY323 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY323, aes(x = seconds_FLY323, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_327 + LOG_0048 ####

# Import csv
FLY327_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY327_REC_MOT.csv")
# Import LOG 
LOG_0048 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0048.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY327_REC_MOT_clean <- distinct(FLY327_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY327_REC_MOT_ready <- FLY327_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0048_R <- transform(LOG_0048, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0048_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0048_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY327_REC_MOT_LD <- left_join(FLY327_REC_MOT_ready, LOG_0048_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY327_REC_MOT_LD_1 <- FLY327_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY327_REC_MOT_LD_1.2 <- FLY327_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY327_REC_MOT_LD_1.2 <- spike_cleaning(FLY327_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 30, upper_limit = 60, omit_first_n = 35, omit_last_n = 5)

# check results by plotting
ggplot(FLY327_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-21 12:32:46 (FLY_327 LOG_0048)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY327_REC_MOT_LD_1.2$tilt_corrected <- FLY327_REC_MOT_LD_1.2$tilt_deg - 5
FLY327_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY327_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY327_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY327_REC_MOT_LD_1.2$tilt_corrected, FLY327_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY327_REC_MOT_LD_1.2 <- correct_altitude(FLY327_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY327_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-21 12:32:46 (FLY_327 LOG_0048)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY327_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY327_REC_MOT_LD_1.2 <- GAP3(FLY327_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(60, 11), c(121,141)))
FLY327_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY327_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_327<- sd(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_327<- sd(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY327_REC_MOT_LD_1.2$relative_diff <- FLY327_REC_MOT_LD_1.2$diff_col / mean(FLY327_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY327_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY327_REC_MOT_LD_1.2 <- FLY327_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY327 <- FLY327_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY327 <- df_FLY327 %>% rename(seconds_FLY327 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY327 <- df_FLY327 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY327, aes(x = seconds_FLY327, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()


#### FLY_330 + LOG_0050 ####
# Import csv
FLY330_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY330_REC_MOT.csv")
# Import LOG 
LOG_0050 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0050.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY330_REC_MOT_clean <- distinct(FLY330_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY330_REC_MOT_ready <- FLY330_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0050_R <- transform(LOG_0050, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0050_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0050_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY330_REC_MOT_LD <- left_join(FLY330_REC_MOT_ready, LOG_0050_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY330_REC_MOT_LD_1 <- FLY330_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY330_REC_MOT_LD_1.2 <- FLY330_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY330_REC_MOT_LD_1.2 <- spike_cleaning(FLY330_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 30, upper_limit = 60, omit_first_n = 25, omit_last_n = 0)

# check results by plotting
ggplot(FLY330_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-21 17:18:48 (FLY_330 LOG_0050)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY330_REC_MOT_LD_1.2$tilt_corrected <- FLY330_REC_MOT_LD_1.2$tilt_deg - 5
FLY330_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY330_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY330_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY330_REC_MOT_LD_1.2$tilt_corrected, FLY330_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY330_REC_MOT_LD_1.2 <- correct_altitude(FLY330_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY330_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-21 17:18:48 (FLY_330 LOG_0050)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY330_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY330_REC_MOT_LD_1.2 <- GAP3(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(51, 211), c(231,331), c(331, 481), c(511,591), c(621, 661), c(681, 721), c(731, 761)))
FLY330_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY330_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_330<- sd(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_330<- sd(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY330_REC_MOT_LD_1.2$relative_diff <- FLY330_REC_MOT_LD_1.2$diff_col / mean(FLY330_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY330_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY330_REC_MOT_LD_1.2 <- FLY330_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY330 <- FLY330_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY330 <- df_FLY330 %>% rename(seconds_FLY330 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY330 <- df_FLY330 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY330, aes(x = seconds_FLY330, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_332 + LOG_0052 ####
# Import csv
FLY332_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY332_REC_MOT.csv")
# Import LOG 
LOG_0052 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0052.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY332_REC_MOT_clean <- distinct(FLY332_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY332_REC_MOT_ready <- FLY332_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0052_R <- transform(LOG_0052, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0052_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0052_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY332_REC_MOT_LD <- left_join(FLY332_REC_MOT_ready, LOG_0052_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY332_REC_MOT_LD_1 <- FLY332_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY332_REC_MOT_LD_1.2 <- FLY332_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY332_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY332_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY332_REC_MOT_LD_1.2 <- spike_cleaning(FLY332_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 130, omit_last_n = 0)

# check results by plotting
ggplot(FLY332_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-22 10:24:31 (FLY_332 LOG_0052)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY332_REC_MOT_LD_1.2$tilt_corrected <- FLY332_REC_MOT_LD_1.2$tilt_deg - 5
FLY332_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY332_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY332_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY332_REC_MOT_LD_1.2$tilt_corrected, FLY332_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY332_REC_MOT_LD_1.2 <- correct_altitude(FLY332_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY332_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-22 10:24:31 (FLY_332 LOG_0052)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY332_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY332_REC_MOT_LD_1.2 <- GAP3(FLY332_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(195, 231), c(291,311)))
FLY332_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY332_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_332<- sd(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_332<- sd(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY332_REC_MOT_LD_1.2$relative_diff <- FLY332_REC_MOT_LD_1.2$diff_col / mean(FLY332_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY332_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY332_REC_MOT_LD_1.2 <- FLY332_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY332 <- FLY332_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY332 <- df_FLY332 %>% rename(seconds_FLY332 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY332 <- df_FLY332 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY332, aes(x = seconds_FLY332, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_352 + LOG_0068 #### 

# Import csv
FLY352_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY352_REC_MOT.csv")
# Import LOG 
LOG_0068 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0068.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY352_REC_MOT_clean <- distinct(FLY352_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY352_REC_MOT_ready <- FLY352_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0068_R <- transform(LOG_0068, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0068_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0068_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY352_REC_MOT_LD <- left_join(FLY352_REC_MOT_ready, LOG_0068_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY352_REC_MOT_LD_1 <- FLY352_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY352_REC_MOT_LD_1.2 <- FLY352_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY352_REC_MOT_LD_1.2 <- spike_cleaning(FLY352_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 298, omit_last_n = 0)

# check results by plotting
ggplot(FLY352_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 06:04:10 (FLY_352 LOG_0068)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY352_REC_MOT_LD_1.2$tilt_corrected <- FLY352_REC_MOT_LD_1.2$tilt_deg - 5
FLY352_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY352_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY352_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY352_REC_MOT_LD_1.2$tilt_corrected, FLY352_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY352_REC_MOT_LD_1.2 <- correct_altitude(FLY352_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY352_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 06:04:10 (FLY_352 LOG_0068)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY352_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY352_REC_MOT_LD_1.2 <- GAP3(FLY352_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(331, 491), c(511,581)))
FLY352_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY352_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_352<- sd(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_352<- sd(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY352_REC_MOT_LD_1.2$relative_diff <- FLY352_REC_MOT_LD_1.2$diff_col / mean(FLY352_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY352_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY352_REC_MOT_LD_1.2 <- FLY352_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY352 <- FLY352_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY352 <- df_FLY352 %>% rename(seconds_FLY352 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY352 <- df_FLY352 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY352, aes(x = seconds_FLY352, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_353 + LOG_0069 ####
# Import csv
FLY353_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY353_REC_MOT.csv")
# Import LOG 
LOG_0069 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0069.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY353_REC_MOT_clean <- distinct(FLY353_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY353_REC_MOT_ready <- FLY353_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0069_R <- transform(LOG_0069, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0069_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0069_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY353_REC_MOT_LD <- left_join(FLY353_REC_MOT_ready, LOG_0069_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY353_REC_MOT_LD_1 <- FLY353_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY353_REC_MOT_LD_1.2 <- FLY353_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY353_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY353_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY353_REC_MOT_LD_1.2 <- spike_cleaning(FLY353_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 50, omit_last_n = 0)

# check results by plotting
ggplot(FLY353_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 09:10:48 (FLY_353 LOG_0069)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY353_REC_MOT_LD_1.2$tilt_corrected <- FLY353_REC_MOT_LD_1.2$tilt_deg - 5
FLY353_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY353_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY353_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY353_REC_MOT_LD_1.2$tilt_corrected, FLY353_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY353_REC_MOT_LD_1.2 <- correct_altitude(FLY353_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY353_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 09:10:48 (FLY_353 LOG_0069)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY353_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY353_REC_MOT_LD_1.2 <- GAP3(FLY353_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(81, 101), c(111,361), c(371, 401)))
FLY353_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY353_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_353<- sd(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_353<- sd(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY353_REC_MOT_LD_1.2$relative_diff <- FLY353_REC_MOT_LD_1.2$diff_col / mean(FLY353_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY353_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY353_REC_MOT_LD_1.2 <- FLY353_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY353 <- FLY353_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY353 <- df_FLY353 %>% rename(seconds_FLY353 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY332 <- df_FLY332 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY353, aes(x = seconds_FLY353, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_354 + LOG_0070 ####
# Import csv
FLY354_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY354_REC_MOT.csv")
# Import LOG 
LOG_0070 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0070.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY354_REC_MOT_clean <- distinct(FLY354_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY354_REC_MOT_ready <- FLY354_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0070_R <- transform(LOG_0070, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0070_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0070_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY354_REC_MOT_LD <- left_join(FLY354_REC_MOT_ready, LOG_0070_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY354_REC_MOT_LD_1 <- FLY354_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY354_REC_MOT_LD_1.2 <- FLY354_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY354_REC_MOT_LD_1.2 <- spike_cleaning(FLY354_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 30, omit_last_n = 0)

# check results by plotting
ggplot(FLY354_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 10:34:59 (FLY_354 LOG_0070)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY354_REC_MOT_LD_1.2$tilt_corrected <- FLY354_REC_MOT_LD_1.2$tilt_deg - 5
FLY354_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY354_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY354_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY354_REC_MOT_LD_1.2$tilt_corrected, FLY354_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY354_REC_MOT_LD_1.2 <- correct_altitude(FLY354_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY354_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-04 10:34:59 (FLY_354 LOG_0070)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY354_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY354_REC_MOT_LD_1.2 <- GAP3(FLY354_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(71, 101), c(121,401), c(421,471)))
FLY354_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY354_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_354<- sd(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_354<- sd(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY354_REC_MOT_LD_1.2$relative_diff <- FLY354_REC_MOT_LD_1.2$diff_col / mean(FLY354_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY354_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY354_REC_MOT_LD_1.2 <- FLY354_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY354 <- FLY354_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY354 <- df_FLY354 %>% rename(seconds_FLY354 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY354 <- df_FLY354 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY354, aes(x = seconds_FLY354, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_365 + LOG_0071 ####

# Import csv
FLY365_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY365_REC_MOT.csv")
# Import LOG 
LOG_0071 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0071.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY365_REC_MOT_clean <- distinct(FLY365_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY365_REC_MOT_ready <- FLY365_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0071_R <- transform(LOG_0071, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0071_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0071_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY365_REC_MOT_LD <- left_join(FLY365_REC_MOT_ready, LOG_0071_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY365_REC_MOT_LD_1 <- FLY365_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0, head(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -2))

# Pulizia spikes 
FLY365_REC_MOT_LD_1.2 <- spike_cleaning(FLY365_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 50, omit_last_n = 0)

# check results by plotting
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY365_REC_MOT_LD_1.2$tilt_corrected <- FLY365_REC_MOT_LD_1.2$tilt_deg - 5
FLY365_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY365_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY365_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY365_REC_MOT_LD_1.2$tilt_corrected, FLY365_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY365_REC_MOT_LD_1.2 <- correct_altitude(FLY365_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY365_REC_MOT_LD_1.2 <- GAP3(FLY365_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(61, 91), c(111,401), c(542,641), c(661,701)))
FLY365_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_365<- sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_365<- sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY365_REC_MOT_LD_1.2$relative_diff <- FLY365_REC_MOT_LD_1.2$diff_col / mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY365 <- FLY365_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY365 <- df_FLY365 %>% rename(seconds_FLY365 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY365 <- df_FLY365 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY365, aes(x = seconds_FLY365, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_368 + LOG_0074 ####

# Import csv
FLY368_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY368_REC_MOT.csv")
# Import LOG 
LOG_0074 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0074.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY368_REC_MOT_clean1 <- distinct(FLY368_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY368_REC_MOT_ready <- FLY368_REC_MOT_clean1 %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0074_R <- transform(LOG_0074, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0074_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0074_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY368_REC_MOT_LD <- left_join(FLY368_REC_MOT_ready, LOG_0074_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY368_REC_MOT_LD_1 <- FLY368_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY368_REC_MOT_LD_1.2 <- FLY368_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY368_REC_MOT_LD_1.2_clean <- subset(FLY368_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# offset BAR to match with Lidar, max 3 rows meaning 
FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY368_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY368_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 60, omit_last_n = 0)

# check results by plotting
ggplot(FLY368_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-12 15:11:23 (FLY_368 LOG_0074)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY368_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY368_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY368_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY368_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY368_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY368_REC_MOT_LD_1.2_clean$tilt_corrected, FLY368_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY368_REC_MOT_LD_1.2_clean <- correct_altitude(FLY368_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY368_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-12 15:11:23 (FLY_368 LOG_0074)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY368_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY368_REC_MOT_LD_1.2_clean <- GAP3(FLY368_REC_MOT_LD_1.2_clean, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(71, 291), c(381, 581)))
FLY368_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY368_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_368<- sd(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_368<- sd(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY368_REC_MOT_LD_1.2_clean$relative_diff <- FLY368_REC_MOT_LD_1.2_clean$diff_col / mean(FLY368_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY368_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY368_REC_MOT_LD_1.2_clean <- FLY368_REC_MOT_LD_1.2_clean %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY368 <- FLY368_REC_MOT_LD_1.2_clean %>% select(seconds, relative_diff)
df_FLY368 <- df_FLY368 %>% rename(seconds_FLY368 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY368 <- df_FLY368 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY368, aes(x = seconds_FLY368, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_371 + LOG_0075 #####

#Import csv
FLY371_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY371_REC_MOT.csv")
# Import LOG 
LOG_0075 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0075.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY371_REC_MOT_clean <- distinct(FLY371_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY371_REC_MOT_ready <- FLY371_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0075_R <- transform(LOG_0075, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0075_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0075_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY371_REC_MOT_LD <- left_join(FLY371_REC_MOT_ready, LOG_0075_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY371_REC_MOT_LD_1 <- FLY371_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY371_REC_MOT_LD_1.2 <- FLY371_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY371_REC_MOT_LD_1.2 <- spike_cleaning(FLY371_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 130, omit_last_n = 0)

# check results by plotting
ggplot(FLY371_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 09:16:17 (FLY_371 LOG_0075)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY371_REC_MOT_LD_1.2$tilt_corrected <- FLY371_REC_MOT_LD_1.2$tilt_deg - 5
FLY371_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY371_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY371_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY371_REC_MOT_LD_1.2$tilt_corrected, FLY371_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY371_REC_MOT_LD_1.2 <- correct_altitude(FLY371_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY371_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 09:16:17 (FLY_371 LOG_0075)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY371_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY371_REC_MOT_LD_1.2 <- GAP3(FLY371_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(191, 281), c(291,311)))
FLY371_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY371_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_371<- sd(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_371<- sd(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY371_REC_MOT_LD_1.2$relative_diff <- FLY371_REC_MOT_LD_1.2$diff_col / mean(FLY371_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY371_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY371_REC_MOT_LD_1.2 <- FLY371_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY371 <- FLY371_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY371 <- df_FLY371 %>% rename(seconds_FLY371 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY371 <- df_FLY371 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY371, aes(x = seconds_FLY371, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_372 + LOG_0076 ####

# Import csv
FLY372_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY372_REC_MOT.csv")
# Import LOG 
LOG_0076 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0076.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY372_REC_MOT_clean <- distinct(FLY372_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY372_REC_MOT_ready <- FLY372_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0076_R <- transform(LOG_0076, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0076_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0076_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY372_REC_MOT_LD <- left_join(FLY372_REC_MOT_ready, LOG_0076_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY372_REC_MOT_LD_1 <- FLY372_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY372_REC_MOT_LD_1.2 <- FLY372_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY372_REC_MOT_LD_1.2 <- spike_cleaning(FLY372_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 10, omit_last_n = 0)

# check results by plotting
ggplot(FLY372_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 09:42:01 (FLY_372 LOG_0076)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY372_REC_MOT_LD_1.2$tilt_corrected <- FLY372_REC_MOT_LD_1.2$tilt_deg - 5
FLY372_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY372_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY372_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY372_REC_MOT_LD_1.2$tilt_corrected, FLY372_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY372_REC_MOT_LD_1.2 <- correct_altitude(FLY372_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY372_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 09:42:01 (FLY_372 LOG_0076)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY372_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY372_REC_MOT_LD_1.2 <- GAP3(FLY372_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(51, 121)))
FLY372_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY372_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_372<- sd(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_372<- sd(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY372_REC_MOT_LD_1.2$relative_diff <- FLY372_REC_MOT_LD_1.2$diff_col / mean(FLY372_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY372_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY372_REC_MOT_LD_1.2 <- FLY372_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY372 <- FLY372_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY372 <- df_FLY372 %>% rename(seconds_FLY372 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY372 <- df_FLY372 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY372, aes(x = seconds_FLY372, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_374 + LOG_0078 ####

# Import csv
FLY374_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY374_REC_MOT.csv")
# Import LOG 
LOG_0078 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0078.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY374_REC_MOT_clean <- distinct(FLY374_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY374_REC_MOT_ready <- FLY374_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0078_R <- transform(LOG_0078, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0078_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0078_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY374_REC_MOT_LD <- left_join(FLY374_REC_MOT_ready, LOG_0078_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY374_REC_MOT_LD_1 <- FLY374_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY374_REC_MOT_LD_1.2 <- FLY374_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY374_REC_MOT_LD_1.2 <- spike_cleaning(FLY374_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 874, omit_last_n = 0)

# check results by plotting
ggplot(FLY374_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 10:16:31 (FLY_374 LOG_0078)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY374_REC_MOT_LD_1.2$tilt_corrected <- FLY374_REC_MOT_LD_1.2$tilt_deg - 5
FLY374_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY374_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY374_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY374_REC_MOT_LD_1.2$tilt_corrected, FLY374_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY374_REC_MOT_LD_1.2 <- correct_altitude(FLY374_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY374_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-14 10:16:31 (FLY_374 LOG_0078)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY374_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY374_REC_MOT_LD_1.2 <- GAP3(FLY374_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(921, 991)))
FLY374_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY374_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_374<- sd(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_374<- sd(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY374_REC_MOT_LD_1.2$relative_diff <- FLY374_REC_MOT_LD_1.2$diff_col / mean(FLY374_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY374_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY374_REC_MOT_LD_1.2 <- FLY374_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY374 <- FLY374_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY374 <- df_FLY374 %>% rename(seconds_FLY374 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY374 <- df_FLY374 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY374, aes(x = seconds_FLY374, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_382 + LOG_0079 ####

# Import csv
FLY382_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY382_REC_MOT.csv")
# Import LOG 
LOG_0079 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0079.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY382_REC_MOT_clean <- distinct(FLY382_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY382_REC_MOT_ready <- FLY382_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0079_R <- transform(LOG_0079, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0079_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0079_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY382_REC_MOT_LD <- left_join(FLY382_REC_MOT_ready, LOG_0079_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY382_REC_MOT_LD_1 <- FLY382_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY382_REC_MOT_LD_1.2 <- FLY382_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY382_REC_MOT_LD_1.2 <- spike_cleaning(FLY382_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 256, omit_last_n = 0)

# check results by plotting
ggplot(FLY382_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-18 09:41:40 (FLY_382 LOG_0079)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY382_REC_MOT_LD_1.2$tilt_corrected <- FLY382_REC_MOT_LD_1.2$tilt_deg - 5
FLY382_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY382_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY382_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY382_REC_MOT_LD_1.2$tilt_corrected, FLY382_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY382_REC_MOT_LD_1.2 <- correct_altitude(FLY382_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY382_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-18 09:41:40 (FLY_382 LOG_0079)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY382_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY382_REC_MOT_LD_1.2 <- GAP3(FLY382_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(301, 391)))
FLY382_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY382_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_382<- sd(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_382<- sd(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY382_REC_MOT_LD_1.2$relative_diff <- FLY382_REC_MOT_LD_1.2$diff_col / mean(FLY382_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY382_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY382_REC_MOT_LD_1.2 <- FLY382_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY382 <- FLY382_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY382 <- df_FLY382 %>% rename(seconds_FLY382 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY382 <- df_FLY382 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY382, aes(x = seconds_FLY382, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_383 + LOG_0080 ####
# Import csv
FLY383_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY383_REC_MOT.csv")
# Import LOG 
LOG_0080 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0080.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY383_REC_MOT_clean <- distinct(FLY383_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY383_REC_MOT_ready <- FLY383_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0080_R <- transform(LOG_0080, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0080_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0080_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY383_REC_MOT_LD <- left_join(FLY383_REC_MOT_ready, LOG_0080_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY383_REC_MOT_LD_1 <- FLY383_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY383_REC_MOT_LD_1.2 <- FLY383_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY383_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY383_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

# Pulizia spikes 
FLY383_REC_MOT_LD_1.2 <- spike_cleaning(FLY383_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 70, omit_last_n = 0)

# check results by plotting
ggplot(FLY383_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-18 10:04:40 (FLY_383)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY383_REC_MOT_LD_1.2$tilt_corrected <- FLY383_REC_MOT_LD_1.2$tilt_deg - 5
FLY383_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY383_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY383_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY383_REC_MOT_LD_1.2$tilt_corrected, FLY383_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY383_REC_MOT_LD_1.2 <- correct_altitude(FLY383_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY383_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-18 10:04:40 (FLY_383)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY383_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY383_REC_MOT_LD_1.2 <- GAP3(FLY383_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(101, 241), c(261,361), c(421, 470, 481,501), c(511,531)))
FLY383_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY383_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_383<- sd(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_383<- sd(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY383_REC_MOT_LD_1.2$relative_diff <- FLY383_REC_MOT_LD_1.2$diff_col / mean(FLY383_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY383_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY383_REC_MOT_LD_1.2 <- FLY383_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY383 <- FLY383_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY383 <- df_FLY383 %>% rename(seconds_FLY383 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY383 <- df_FLY383 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY383, aes(x = seconds_FLY383, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_384 + LOG_0081 ####

# Import csv
FLY384_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY384_REC_MOT.csv")
# Import LOG 
LOG_0081 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0081.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY384_REC_MOT_clean <- distinct(FLY384_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY384_REC_MOT_ready <- FLY384_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0081_R <- transform(LOG_0081, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0081_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0081_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY384_REC_MOT_LD <- left_join(FLY384_REC_MOT_ready, LOG_0081_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY384_REC_MOT_LD_1 <- FLY384_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY384_REC_MOT_LD_1.2 <- FLY384_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY384_REC_MOT_LD_1.2 <- spike_cleaning(FLY384_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 160, omit_last_n = 0)

# check results by plotting
ggplot(FLY384_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 10:07:31 (FLY_384 LOG_0081)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY384_REC_MOT_LD_1.2$tilt_corrected <- FLY384_REC_MOT_LD_1.2$tilt_deg - 5
FLY384_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY384_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY384_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY384_REC_MOT_LD_1.2$tilt_corrected, FLY384_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY384_REC_MOT_LD_1.2 <- correct_altitude(FLY384_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY384_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 10:07:31 (FLY_384 LOG_0081)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY384_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY384_REC_MOT_LD_1.2 <- GAP3(FLY384_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(195, 391), c(441,481), c(491,531, c(551,591), c(605, 691))))
FLY384_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY384_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_384<- sd(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_384<- sd(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY384_REC_MOT_LD_1.2$relative_diff <- FLY384_REC_MOT_LD_1.2$diff_col / mean(FLY384_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY384_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY384_REC_MOT_LD_1.2 <- FLY384_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY384 <- FLY384_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY384 <- df_FLY384 %>% rename(seconds_FLY384 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY384 <- df_FLY384 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY384, aes(x = seconds_FLY384, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_387 + LOG_0084 ####
# Import csv
FLY387_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY387_REC_MOT.csv")
# Import LOG 
LOG_0084 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0084.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY387_REC_MOT_clean <- distinct(FLY387_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY387_REC_MOT_ready <- FLY387_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0084_R <- transform(LOG_0084, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0084_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0084_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY387_REC_MOT_LD <- left_join(FLY387_REC_MOT_ready, LOG_0084_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY387_REC_MOT_LD_1 <- FLY387_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY387_REC_MOT_LD_1.2 <- FLY387_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY387_REC_MOT_LD_1.2 <- spike_cleaning(FLY387_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 150, omit_last_n = 0)

# check results by plotting
ggplot(FLY387_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 11:10:10 (FLY_387 LOG_0084)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY387_REC_MOT_LD_1.2$tilt_corrected <- FLY387_REC_MOT_LD_1.2$tilt_deg - 5
FLY387_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY387_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY387_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY387_REC_MOT_LD_1.2$tilt_corrected, FLY387_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY387_REC_MOT_LD_1.2 <- correct_altitude(FLY387_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY387_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-07-22 10:24:31 (FLY_332 LOG_0052)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY387_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY387_REC_MOT_LD_1.2 <- GAP3(FLY387_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(181, 241)))
FLY387_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY387_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_387<- sd(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_387<- sd(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY387_REC_MOT_LD_1.2$relative_diff <- FLY387_REC_MOT_LD_1.2$diff_col / mean(FLY387_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY387_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY387_REC_MOT_LD_1.2 <- FLY387_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY387 <- FLY387_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY387 <- df_FLY387 %>% rename(seconds_FLY387 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY387 <- df_FLY387 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY387, aes(x = seconds_FLY376, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_388 + LOG_0085 ####
# Import csv
FLY388_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY388_REC_MOT.csv")
# Import LOG 
LOG_0085 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0085.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY388_REC_MOT_clean <- distinct(FLY388_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY388_REC_MOT_ready <- FLY388_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0085_R <- transform(LOG_0085, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0085_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0085_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY388_REC_MOT_LD <- left_join(FLY388_REC_MOT_ready, LOG_0085_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY388_REC_MOT_LD_1 <- FLY388_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY388_REC_MOT_LD_1.2 <- FLY388_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Pulizia spikes 
FLY388_REC_MOT_LD_1.2 <- spike_cleaning(FLY388_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY388_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 11:25:31 (FLY_388 LOG_0085)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY388_REC_MOT_LD_1.2$tilt_corrected <- FLY388_REC_MOT_LD_1.2$tilt_deg - 5
FLY388_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY388_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY388_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY388_REC_MOT_LD_1.2$tilt_corrected, FLY388_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY388_REC_MOT_LD_1.2 <- correct_altitude(FLY388_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY388_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 11:25:31 (FLY_388 LOG_0085)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY388_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY388_REC_MOT_LD_1.2 <- GAP3(FLY388_REC_MOT_LD_1.2, "laser_altitude_m_corrected", "osd_data:relativeHeight[meters]", list(c(21, 61)))
FLY388_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY388_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_388<- sd(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_388<- sd(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY388_REC_MOT_LD_1.2$relative_diff <- FLY388_REC_MOT_LD_1.2$diff_col / mean(FLY388_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY388_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY388_REC_MOT_LD_1.2 <- FLY388_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY388 <- FLY388_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY388 <- df_FLY388 %>% rename(seconds_FLY388 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY388 <- df_FLY388 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY388, aes(x = seconds_FLY388, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### 

#### nuvola ####

library(dplyr)
library(tidyr)
library(ggplot2)

list_of_data_frames <- list(df_FLY298,df_FLY299,df_FLY304,df_FLY306,df_FLY307,df_FLY310,df_FLY311,df_FLY312,
                            df_FLY313,df_FLY315,df_FLY316,df_FLY317,df_FLY318,df_FLY319,df_FLY323,df_FLY327,
                            df_FLY330,df_FLY332,df_FLY352,df_FLY353,df_FLY354,df_FLY365,df_FLY368,df_FLY371,
                            df_FLY372,df_FLY374,df_FLY382,df_FLY383,df_FLY384)

# Rinomina le colonne "seconds" nei data frame
list_of_data_frames <- lapply(list_of_data_frames, function(df) {
  colnames(df)[grep("^seconds", colnames(df))] <- "seconds"
  df
})

# Unisci i data frame in modo iterativo
result <- list_of_data_frames %>%
  reduce(function(x, y) full_join(x, y, by = "seconds")) %>%
  rename_all(~sub("relative_diff_", "", .))

# Trasforma il risultato in formato lungo
result_long <- result %>%
  gather(key = "flight", value = "relative_diff", -seconds) %>%
  mutate(flight = gsub("FLY", "", flight))

# Grafico a dispersione con ggplot2
ggplot(result_long, aes(x = seconds, y = relative_diff)) +
  geom_point(size = 0.1, color = "black") +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()


ggplot(result_long, aes(x = seconds, y = relative_diff)) +
  geom_point(size = 0.1, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

