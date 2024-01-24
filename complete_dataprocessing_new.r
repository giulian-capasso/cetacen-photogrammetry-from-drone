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

options(scipen = 999) 

## functions
azzera_dataset <- function(dataset) {
  # Trova l'indice del primo valore maggiore di 2 metri
  indice_inizio <- which(dataset$`osd_data:relativeHeight[meters]` > 2)[1]
  
  # Calcola il nuovo indice di partenza
  nuovo_indice_inizio <- indice_inizio - 3
  
  # Elimina tutti i valori precedenti al nuovo indice
  dataset <- dataset[-c(1:nuovo_indice_inizio), , drop = FALSE]
  
  return(dataset)
}
#FLY297_MOT_MOT <- azzera_dataset(FLY297_REC_MOT_LD_1.2)

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
#### FLY_297 + LOG0027 ####
# Import csv
FLY297_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY297_REC_MOT.csv")
# Import log
LOG_0027 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0027.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY297_REC_MOT_clean <- distinct(FLY297_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY297_REC_MOT_ready <- FLY297_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0027_R <- transform(LOG_0027, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0027_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0027_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY297_REC_MOT_LD <- left_join(FLY297_REC_MOT_ready, LOG_0027_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY297_REC_MOT_LD_1 <- FLY297_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY297_REC_MOT_LD_1.2 <- FLY297_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY297_REC_MOT_LD_1.2 <- azzera_dataset(FLY297_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY297_REC_MOT_LD_1.2 <- FLY297_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY297")
# Add seconds column
FLY297_REC_MOT_LD_1.2 <- FLY297_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY297_REC_MOT_LD_1.2 <- spike_cleaning(FLY297_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY297_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-12 14:49:31 (FLY_297 LOG0027)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY297_REC_MOT_LD_1.2$tilt_corrected <- FLY297_REC_MOT_LD_1.2$tilt_deg - 5
FLY297_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY297_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY297_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY297_REC_MOT_LD_1.2$tilt_corrected, FLY297_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY297_REC_MOT_LD_1.2 <- correct_altitude(FLY297_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY297_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-12 14:49:31 (FLY_297 LOG0027)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY297_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY297_REC_MOT_LD_1.2 <- GAP3(FLY297_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 161),c(191,301),c(311,332)))
FLY297_REC_MOT_LD_1.2$diff_col

sd(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned, na.rm = TRUE)/mean(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned, na.rm = TRUE)
sd(FLY297_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, na.rm = TRUE)/mean(FLY297_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, na.rm = TRUE)

mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm=TRUE)
sd(FLY297_REC_MOT_LD_1.2$diff_col, na.rm=TRUE)

stat.desc(FLY297_REC_MOT_LD_1.2$diff_col)


# statistiche descrittive alla fine

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY297_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_297<- sd(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_297<- sd(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

sd(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned[31:161], na.rm = TRUE)
mean(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned[31:161], na.rm = TRUE)
sd(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned[31:161], na.rm = TRUE)/mean(FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned[31:161], na.rm = TRUE)

# differenza relativa
FLY297_REC_MOT_LD_1.2$relative_diff <- FLY297_REC_MOT_LD_1.2$diff_col / mean(FLY297_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY297_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


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
# Azzera accensione motori
FLY298_REC_MOT_LD_1.2 <- azzera_dataset(FLY298_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY298_REC_MOT_LD_1.2 <- FLY298_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY298")
# Add seconds column
FLY298_REC_MOT_LD_1.2 <- FLY298_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# spike cleaning
FLY298_REC_MOT_LD_1.2 <- spike_cleaning(FLY298_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 00, upper_limit = 60, omit_first_n = 0, omit_last_n = 10)

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
FLY298_REC_MOT_LD_1.2 <- GAP3(FLY298_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 121), c(141, 481)))
FLY298_REC_MOT_LD_1.2$diff_col
mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm=TRUE)
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
# Azzera accensione motori
FLY299_REC_MOT_LD_1.2 <- azzera_dataset(FLY299_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY299_REC_MOT_LD_1.2 <- FLY299_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY299")
# Add seconds column
FLY299_REC_MOT_LD_1.2 <- FLY299_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# spike cleaning
FLY299_REC_MOT_LD_1.2 <- spike_cleaning(FLY299_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 10)

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
FLY299_REC_MOT_LD_1.2 <- GAP3(FLY299_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 601), c(631, 651)))
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
# Azzera accensione motori
FLY304_REC_MOT_LD_1.2 <- azzera_dataset(FLY304_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY304")
# Add seconds column
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY304_REC_MOT_LD_1.2 <- spike_cleaning(FLY304_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 2, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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
FLY304_REC_MOT_LD_1.2 <- GAP3(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(71, 141), c(221, 261), c(281, 341), c(361, 401), c(411, 481),c(491, 571), c(671,711)))
FLY304_REC_MOT_LD_1.2$diff_col
FLY304_REC_MOT_LD_1.2$diff_col[221:231]
FLY304_REC_MOT_LD_1.2$diff_col[681:691]

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

# differenza relativa su altezza
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = `osd_data:relativeHeight[meters]`, y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Laser altitude", y = "Differenza (m)") +
  ylim(0, 2) +
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
# Azzera accensione motori
FLY306_REC_MOT_LD_1.2 <- azzera_dataset(FLY306_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY306_REC_MOT_LD_1.2 <- FLY306_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY306")
# Add seconds column
FLY306_REC_MOT_LD_1.2 <- FLY306_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY306_REC_MOT_LD_1.2 <- spike_cleaning(FLY306_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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
FLY306_REC_MOT_LD_1.2 <- GAP3(FLY306_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 99), c(111, 121), c(131, 151), c(161,241)))
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

ggplot(FLY306_REC_MOT_LD_1.2, aes(x = `osd_data:relativeHeight[meters]`, y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
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
# Azzera accensione motori
FLY307_REC_MOT_LD_1.2 <- azzera_dataset(FLY307_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY307_REC_MOT_LD_1.2 <- FLY307_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY307")
# Add seconds column
FLY307_REC_MOT_LD_1.2 <- FLY307_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY307_REC_MOT_LD_1.2 <- spike_cleaning(FLY307_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 2, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY307_REC_MOT_LD_1.2 <- GAP3(FLY307_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 101), c(141, 151), c(161, 231), c(241, 291), c(311, 331), c(401, 431),c(481,491),c(561,571) ))
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
# Azzera accensione motori
FLY310_REC_MOT_LD_1.2 <- azzera_dataset(FLY310_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY310_REC_MOT_LD_1.2 <- FLY310_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY310")
# Add seconds column
FLY310_REC_MOT_LD_1.2 <- FLY310_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY310_REC_MOT_LD_1.2 <- spike_cleaning(FLY310_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY310_REC_MOT_LD_1.2 <- GAP3(FLY310_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 181), c(201, 371), c(481, 551)))
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

# differenza relativa su altezza
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = `osd_data:relativeHeight[meters]`, y = laser_altitude_m_cleaned, )) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Laser altitude", y = "Differenza (m)") +
  ylim(0, 2) +
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
# Azzera accensione motori
FLY311_REC_MOT_LD_1.2 <- azzera_dataset(FLY311_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY311_REC_MOT_LD_1.2 <- FLY311_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY311")
# Add seconds column
FLY311_REC_MOT_LD_1.2 <- FLY311_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY311_REC_MOT_LD_1.2 <- spike_cleaning(FLY311_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 4, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY311_REC_MOT_LD_1.2 <- GAP3(FLY311_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(11, 21), c(41, 81), c(121, 141), c(161, 351), c(361, 401)))
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
# Azzera accensione motori
FLY312_REC_MOT_LD_1.2 <- azzera_dataset(FLY312_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY312_REC_MOT_LD_1.2 <- FLY312_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY312")
# Add seconds column
FLY312_REC_MOT_LD_1.2 <- FLY312_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY312_REC_MOT_LD_1.2 <- spike_cleaning(FLY312_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 1, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY312_REC_MOT_LD_1.2 <- GAP3(FLY312_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 51), c(61, 251)))
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
# Azzera accensione motori
FLY313_REC_MOT_LD_1.2 <- azzera_dataset(FLY313_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY313_REC_MOT_LD_1.2 <- FLY313_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY313")
# Add seconds column
FLY313_REC_MOT_LD_1.2 <- FLY313_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY313_REC_MOT_LD_1.2 <- spike_cleaning(FLY313_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY313_REC_MOT_LD_1.2 <- GAP3(FLY313_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 131), c(145, 161)))
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
# Azzera accensione motori
FLY315_REC_MOT_LD_1.2 <- azzera_dataset(FLY315_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY315_REC_MOT_LD_1.2 <- FLY315_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY315")
# Add seconds column
FLY315_REC_MOT_LD_1.2 <- FLY315_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY315_REC_MOT_LD_1.2 <- spike_cleaning(FLY315_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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
FLY315_REC_MOT_LD_1.2 <- GAP3(FLY315_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(71, 81), c(115, 281), c(301, 341)))
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
# Azzera accensione motori
FLY316_REC_MOT_LD_1.2 <- azzera_dataset(FLY316_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY316_REC_MOT_LD_1.2 <- FLY316_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY316")
# Add seconds column
FLY316_REC_MOT_LD_1.2 <- FLY316_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY316_REC_MOT_LD_1.2 <- spike_cleaning(FLY316_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY316_REC_MOT_LD_1.2 <- GAP3(FLY316_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(101, 175)))
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
# Azzera accensione motori
FLY317_REC_MOT_LD_1.2 <- azzera_dataset(FLY317_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY317_REC_MOT_LD_1.2 <- FLY317_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY317")
# Add seconds column
FLY317_REC_MOT_LD_1.2 <- FLY317_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY317_REC_MOT_LD_1.2 <- spike_cleaning(FLY317_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 30, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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
FLY317_REC_MOT_LD_1.2 <- GAP3(FLY317_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(71, 261), c(301, 420)))
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
# Azzera accensione motori
#FLY318_REC_MOT_LD_1.2 <- azzera_dataset(FLY318_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY318_REC_MOT_LD_1.2 <- FLY318_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY318")
# Add seconds column
FLY318_REC_MOT_LD_1.2 <- FLY318_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

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
FLY318_REC_MOT_LD_1.2 <- GAP3(FLY318_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 195)))
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
# Azzera accensione motori
FLY319_REC_MOT_LD_1.2 <- azzera_dataset(FLY319_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY319_REC_MOT_LD_1.2 <- FLY319_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY319")
# Add seconds column
FLY319_REC_MOT_LD_1.2 <- FLY319_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY319_REC_MOT_LD_1.2 <- spike_cleaning(FLY319_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 1, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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
FLY319_REC_MOT_LD_1.2 <- GAP3(FLY319_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 81), c(85,101), c(151,171),c(19,196)))
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
# Azzera accensione motori
FLY323_REC_MOT_LD_1.2 <- azzera_dataset(FLY323_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY323_REC_MOT_LD_1.2 <- FLY323_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY323")
# Add seconds column
FLY323_REC_MOT_LD_1.2 <- FLY323_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY323_REC_MOT_LD_1.2 <- spike_cleaning(FLY323_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY323_REC_MOT_LD_1.2 <- GAP3(FLY323_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 151), c(165,181)))
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
# Azzera accensione motori
FLY327_REC_MOT_LD_1.2 <- azzera_dataset(FLY327_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY327_REC_MOT_LD_1.2 <- FLY327_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY327")
# Add seconds column
FLY327_REC_MOT_LD_1.2 <- FLY327_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY327_REC_MOT_LD_1.2 <- spike_cleaning(FLY327_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY327_REC_MOT_LD_1.2 <- GAP3(FLY327_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(25, 75), c(85,101)))
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
# Azzera accensione motori
FLY330_REC_MOT_LD_1.2 <- azzera_dataset(FLY330_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY330_REC_MOT_LD_1.2 <- FLY330_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY330")
# Add seconds column
FLY330_REC_MOT_LD_1.2 <- FLY330_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY330_REC_MOT_LD_1.2 <- spike_cleaning(FLY330_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY330_REC_MOT_LD_1.2 <- GAP3(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 191), c(201,301), c(331, 461), c(501,561), c(661, 691), c(705, 715)))
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
# Azzera accensione motori
FLY332_REC_MOT_LD_1.2 <- azzera_dataset(FLY332_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY332_REC_MOT_LD_1.2 <- FLY332_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY332")
# Add seconds column
FLY332_REC_MOT_LD_1.2 <- FLY332_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY332_REC_MOT_LD_1.2 <- spike_cleaning(FLY332_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY332_REC_MOT_LD_1.2 <- GAP3(FLY332_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 101), c(151,181)))
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
# Azzera accensione motori
FLY352_REC_MOT_LD_1.2 <- azzera_dataset(FLY352_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY352_REC_MOT_LD_1.2 <- FLY352_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY352")
# Add seconds column
FLY352_REC_MOT_LD_1.2 <- FLY352_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY352_REC_MOT_LD_1.2 <- spike_cleaning(FLY352_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY352_REC_MOT_LD_1.2 <- GAP3(FLY352_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 201), c(221,241)))
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
# Azzera accensione motori
FLY353_REC_MOT_LD_1.2 <- azzera_dataset(FLY353_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY353_REC_MOT_LD_1.2 <- FLY353_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY353")
# Add seconds column
FLY353_REC_MOT_LD_1.2 <- FLY353_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY353_REC_MOT_LD_1.2 <- spike_cleaning(FLY353_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_vline(xintercept = c(34, 298), linetype = "dashed", color = "red") +  # start stop
  labs(title = "2022-08-04 09:10:48 (FLY_353 LOG_0069)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY353_REC_MOT_LD_1.2), by = 5))

# Calcola Gap 
FLY353_REC_MOT_LD_1.2 <- GAP3(FLY353_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21,41),c(75, 291), c(311,335)))
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
# Azzera accensione motori
FLY354_REC_MOT_LD_1.2 <- azzera_dataset(FLY354_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY354_REC_MOT_LD_1.2 <- FLY354_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY354")
# Add seconds column
FLY354_REC_MOT_LD_1.2 <- FLY354_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY354_REC_MOT_LD_1.2 <- spike_cleaning(FLY354_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY354_REC_MOT_LD_1.2 <- GAP3(FLY354_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 71), c(81,371), c(431,441)))
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
# Azzera accensione motori
FLY365_REC_MOT_LD_1.2 <- azzera_dataset(FLY365_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY365")
# Add seconds column
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY365_REC_MOT_LD_1.2 <- spike_cleaning(FLY365_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
  geom_vline(xintercept = c(34,361, 518,604), linetype = "dashed", color = "black") +
  geom_vline(xintercept = c(79), linetype = "solid", color = "orange") +
  geom_vline(xintercept = c(558), linetype = "solid", color = "purple") +
  
    labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))

# heignt frame whale
mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[77:81])
sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[77:81])
mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[77:81])
sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[77:81])

#height frame pole
mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[556:560])
sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[556:560])
mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[556:560])
sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[556:560])


# Calcola Gap 
FLY365_REC_MOT_LD_1.2 <- GAP3(FLY365_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 361), c(531,581), c(611,631)))
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
# Azzera accensione motori
FLY368_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY368_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY368_REC_MOT_LD_1.2_clean <- FLY368_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY368")
# Add seconds column
FLY368_REC_MOT_LD_1.2_clean <- FLY368_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY368_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY368_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY368_REC_MOT_LD_1.2_clean <- GAP3(FLY368_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 261), c(321, 339), c(351,541)))
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
# offset BAR to match with Lidar, max 3 rows meaning 
FLY371_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY371_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY371_REC_MOT_LD_1.2 <- azzera_dataset(FLY371_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY371_REC_MOT_LD_1.2 <- FLY371_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY371")
# Add seconds column
FLY371_REC_MOT_LD_1.2 <- FLY371_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY371_REC_MOT_LD_1.2 <- spike_cleaning(FLY371_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY371_REC_MOT_LD_1.2 <- GAP3(FLY371_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 151), c(161,175)))
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
# Azzera accensione motori
FLY372_REC_MOT_LD_1.2 <- azzera_dataset(FLY372_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY372_REC_MOT_LD_1.2 <- FLY372_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY372")
# Add seconds column
FLY372_REC_MOT_LD_1.2 <- FLY372_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY372_REC_MOT_LD_1.2 <- spike_cleaning(FLY372_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY372_REC_MOT_LD_1.2 <- GAP3(FLY372_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 121)))
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
# Azzera accensione motori
FLY374_REC_MOT_LD_1.2 <- azzera_dataset(FLY374_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY374_REC_MOT_LD_1.2 <- FLY374_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY374")
# Add seconds column
FLY374_REC_MOT_LD_1.2 <- FLY374_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY374_REC_MOT_LD_1.2 <- spike_cleaning(FLY374_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY374_REC_MOT_LD_1.2 <- GAP3(FLY374_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 121)))
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
# Azzera accensione motori
FLY382_REC_MOT_LD_1.2 <- azzera_dataset(FLY382_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY382_REC_MOT_LD_1.2 <- FLY382_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY382")
# Add seconds column
FLY382_REC_MOT_LD_1.2 <- FLY382_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY382_REC_MOT_LD_1.2 <- spike_cleaning(FLY382_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY382_REC_MOT_LD_1.2 <- GAP3(FLY382_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 101),c(125,151)))
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
# Azzera accensione motori
FLY383_REC_MOT_LD_1.2 <- azzera_dataset(FLY383_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY383_REC_MOT_LD_1.2 <- FLY383_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY383")
# Add seconds column
FLY383_REC_MOT_LD_1.2 <- FLY383_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY383_REC_MOT_LD_1.2 <- spike_cleaning(FLY383_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY383_REC_MOT_LD_1.2 <- GAP3(FLY383_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 161), c(201,281), c(361, 391), c(421,422),c(441,458)))
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
# Azzera accensione motori
FLY384_REC_MOT_LD_1.2 <- azzera_dataset(FLY384_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY384_REC_MOT_LD_1.2 <- FLY384_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY384")
# Add seconds column
FLY384_REC_MOT_LD_1.2 <- FLY384_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY384_REC_MOT_LD_1.2 <- spike_cleaning(FLY384_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY384_REC_MOT_LD_1.2 <- GAP3(FLY384_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 81), c(91,111), c(141,231), c(281,291), c(301, 311),c(350,365),c(411,431),c(471,481)))
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
# Azzera accensione motori
FLY387_REC_MOT_LD_1.2 <- azzera_dataset(FLY387_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY387_REC_MOT_LD_1.2 <- FLY387_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY387")
# Add seconds column
FLY387_REC_MOT_LD_1.2 <- FLY387_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY387_REC_MOT_LD_1.2 <- spike_cleaning(FLY387_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

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
FLY387_REC_MOT_LD_1.2 <- GAP3(FLY387_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 91)))
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
# Azzera accensione motori
FLY388_REC_MOT_LD_1.2 <- azzera_dataset(FLY388_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY388_REC_MOT_LD_1.2 <- FLY388_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY388")
# Add seconds column
FLY388_REC_MOT_LD_1.2 <- FLY388_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

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
FLY388_REC_MOT_LD_1.2 <- GAP3(FLY388_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 61)))
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

#### FLY_389 + LOG_0086 ####
# Import csv
FLY389_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY389_REC_MOT.csv")
# Import LOG 
LOG_0086 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0086.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY389_REC_MOT_clean1 <- distinct(FLY389_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY389_REC_MOT_ready <- FLY389_REC_MOT_clean1 %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0086_R <- transform(LOG_0086, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0086_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0086_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY389_REC_MOT_LD <- left_join(FLY389_REC_MOT_ready, LOG_0086_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY389_REC_MOT_LD_1 <- FLY389_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY389_REC_MOT_LD_1.2 <- FLY389_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY389_REC_MOT_LD_1.2_clean <- subset(FLY389_REC_MOT_LD_1.2, !grepl("2015-10-21", GPS.dateTimeStamp))
# Azzera accensione motori
FLY389_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY389_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY389_REC_MOT_LD_1.2_clean <- FLY389_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY389")
# Add seconds column
FLY389_REC_MOT_LD_1.2_clean <- FLY389_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY389_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY389_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY389_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 11:36:11 (FLY_389 LOG_0086)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY389_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY389_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY389_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY389_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY389_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY389_REC_MOT_LD_1.2_clean$tilt_corrected, FLY389_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY389_REC_MOT_LD_1.2_clean <- correct_altitude(FLY389_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY389_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-19 11:36:11 (FLY_389 LOG_0086)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY389_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY389_REC_MOT_LD_1.2_clean <- GAP3(FLY389_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(151, 171),c(241,271)))
FLY389_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY389_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_389<- sd(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_389<- sd(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY389_REC_MOT_LD_1.2_clean$relative_diff <- FLY389_REC_MOT_LD_1.2_clean$diff_col / mean(FLY389_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY389_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_393 + LOG_0088 ####
# Import csv
FLY393_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY393_REC_MOT.csv")
# Import LOG 
LOG_0088 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0088.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY393_REC_MOT_clean <- distinct(FLY393_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY393_REC_MOT_ready <- FLY393_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0088_R <- transform(LOG_0088, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0088_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0088_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY393_REC_MOT_LD <- left_join(FLY393_REC_MOT_ready, LOG_0088_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY393_REC_MOT_LD_1 <- FLY393_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY393_REC_MOT_LD_1.2 <- FLY393_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY393_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY393_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY393_REC_MOT_LD_1.2 <- azzera_dataset(FLY393_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY393_REC_MOT_LD_1.2 <- FLY393_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY393")
# Add seconds column
FLY393_REC_MOT_LD_1.2 <- FLY393_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# check results by plotting
ggplot(FLY393_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  #geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-26 16:21:30 (FLY_393 LOG_0088)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY393_REC_MOT_LD_1.2$tilt_corrected <- FLY393_REC_MOT_LD_1.2$tilt_deg - 5
FLY393_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY393_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY393_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY393_REC_MOT_LD_1.2$tilt_corrected, FLY393_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY393_REC_MOT_LD_1.2 <- correct_altitude(FLY393_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY393_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
 # geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-10-26 16:21:30 (FLY_393 LOG_0088)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY393_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY393_REC_MOT_LD_1.2 <- GAP3(FLY393_REC_MOT_LD_1.2, "laser_altitude_m", "osd_data:relativeHeight[meters]", list(c(31, 51),c(65,81),c(91,101),c(111,121),c(141,151),c(161,181),c(191,211)))
FLY393_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY393_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_393<- sd(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_393<- sd(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY393_REC_MOT_LD_1.2$relative_diff <- FLY393_REC_MOT_LD_1.2$diff_col / mean(FLY393_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY393_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_396 + LOG_0090 ####
# Import csv
FLY396_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY396_REC_MOT.csv")
# Import LOG 
LOG_0090 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0090.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY396_REC_MOT_clean <- distinct(FLY396_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY396_REC_MOT_ready <- FLY396_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0090_R <- transform(LOG_0090, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0090_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0090_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY396_REC_MOT_LD <- left_join(FLY396_REC_MOT_ready, LOG_0090_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY396_REC_MOT_LD_1 <- FLY396_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY396_REC_MOT_LD_1.2 <- FLY396_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY396_REC_MOT_LD_1.2_clean <- subset(FLY396_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Azzera accensione motori
FLY396_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY396_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY396_REC_MOT_LD_1.2_clean <- FLY396_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY396")
# Add seconds column
FLY396_REC_MOT_LD_1.2_clean <- FLY396_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY396_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY396_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY396_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 08:45:32 (FLY_396 LOG_0090)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY396_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY396_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY396_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY396_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY396_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY396_REC_MOT_LD_1.2_clean$tilt_corrected, FLY396_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY396_REC_MOT_LD_1.2_clean <- correct_altitude(FLY396_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY396_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 08:45:32 (FLY_396 LOG_0090)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY396_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY396_REC_MOT_LD_1.2_clean <- GAP3(FLY396_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(91, 111), c(131,161), c(191, 231), c(241,261), c(271,281)))
FLY396_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY396_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_396<- sd(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_396<- sd(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY396_REC_MOT_LD_1.2_clean$relative_diff <- FLY396_REC_MOT_LD_1.2_clean$diff_col / mean(FLY396_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY396_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_400 + LOG_0093 ####
# Import csv
FLY400_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY400_REC_MOT.csv")
# Import LOG 
LOG_0093 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0093.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY400_REC_MOT_clean <- distinct(FLY400_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY400_REC_MOT_ready <- FLY400_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0093_R <- transform(LOG_0093, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0093_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0093_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY400_REC_MOT_LD <- left_join(FLY400_REC_MOT_ready, LOG_0093_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY400_REC_MOT_LD_1 <- FLY400_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY400_REC_MOT_LD_1.2 <- FLY400_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY400_REC_MOT_LD_1.2 <- azzera_dataset(FLY400_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY400_REC_MOT_LD_1.2 <- FLY400_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY400")
# Add seconds column
FLY400_REC_MOT_LD_1.2 <- FLY400_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY400_REC_MOT_LD_1.2 <- spike_cleaning(FLY400_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY400_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:47:57 (FLY_400 LOG_0093)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY400_REC_MOT_LD_1.2$tilt_corrected <- FLY400_REC_MOT_LD_1.2$tilt_deg - 5
FLY400_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY400_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY400_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY400_REC_MOT_LD_1.2$tilt_corrected, FLY400_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY400_REC_MOT_LD_1.2 <- correct_altitude(FLY400_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY400_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:47:57 (FLY_400 LOG_0093)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY400_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY400_REC_MOT_LD_1.2 <- GAP3(FLY400_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 61), c(71,81),c(85,151)))
FLY400_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY400_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_400<- sd(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_400<- sd(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY400_REC_MOT_LD_1.2$relative_diff <- FLY400_REC_MOT_LD_1.2$diff_col / mean(FLY400_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY400_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### FLY_401 + LOG_0094 ####
# Import csv
FLY401_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY401_REC_MOT.csv")
# Import LOG 
LOG_0094 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0094.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY401_REC_MOT_clean <- distinct(FLY401_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY401_REC_MOT_ready <- FLY401_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0094_R <- transform(LOG_0094, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0094_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0094_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY401_REC_MOT_LD <- left_join(FLY401_REC_MOT_ready, LOG_0094_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY401_REC_MOT_LD_1 <- FLY401_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY401_REC_MOT_LD_1.2 <- FLY401_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY401_REC_MOT_LD_1.2 <- azzera_dataset(FLY401_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY401_REC_MOT_LD_1.2 <- FLY401_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY401")

# Pulizia spikes 
FLY401_REC_MOT_LD_1.2 <- spike_cleaning(FLY401_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY401_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:57:52 (FLY_401 LOG_0094)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY401_REC_MOT_LD_1.2$tilt_corrected <- FLY401_REC_MOT_LD_1.2$tilt_deg - 5
FLY401_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY401_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY401_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY401_REC_MOT_LD_1.2$tilt_corrected, FLY401_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY401_REC_MOT_LD_1.2 <- correct_altitude(FLY401_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY401_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:57:52 (FLY_401 LOG_0094)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY401_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY401_REC_MOT_LD_1.2 <- GAP3(FLY401_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 91),c(131,158)))
FLY401_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY401_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_401<- sd(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_401<- sd(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY401_REC_MOT_LD_1.2$relative_diff <- FLY401_REC_MOT_LD_1.2$diff_col / mean(FLY401_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY401_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY401_REC_MOT_LD_1.2 <- FLY401_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY401 <- FLY401_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY401 <- df_FLY401 %>% rename(seconds_FLY401 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY401 <- df_FLY401 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY401, aes(x = seconds_FLY401, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_402 + LOG_0095 ####
# Import csv
FLY402_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY402_REC_MOT.csv")
# Import LOG 
LOG_0095 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0095.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY402_REC_MOT_clean <- distinct(FLY402_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY402_REC_MOT_ready <- FLY402_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0095_R <- transform(LOG_0095, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0095_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0095_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY402_REC_MOT_LD <- left_join(FLY402_REC_MOT_ready, LOG_0095_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY402_REC_MOT_LD_1 <- FLY402_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY402_REC_MOT_LD_1.2 <- FLY402_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY402_REC_MOT_LD_1.2 <- azzera_dataset(FLY402_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY402_REC_MOT_LD_1.2 <- FLY402_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY402")
# Add seconds column
FLY402_REC_MOT_LD_1.2 <- FLY402_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY402_REC_MOT_LD_1.2 <- spike_cleaning(FLY402_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY402_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 12:49:01 (FLY_402 LOG_0095)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY402_REC_MOT_LD_1.2$tilt_corrected <- FLY402_REC_MOT_LD_1.2$tilt_deg - 5
FLY402_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY402_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY402_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY402_REC_MOT_LD_1.2$tilt_corrected, FLY402_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY402_REC_MOT_LD_1.2 <- correct_altitude(FLY402_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY402_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 12:49:01 (FLY_402 LOG_0095)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY402_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY402_REC_MOT_LD_1.2 <- GAP3(FLY402_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 41),c(91,141),c(151,171)))
FLY402_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY402_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_402<- sd(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_402<- sd(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY402_REC_MOT_LD_1.2$relative_diff <- FLY402_REC_MOT_LD_1.2$diff_col / mean(FLY402_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY402_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_411 + LOG99 ####

# Import csv
FLY411_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY411_REC_MOT.csv")
# Import LOG 
LOG_0099 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0099.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY411_REC_MOT_clean <- distinct(FLY411_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY411_REC_MOT_ready <- FLY411_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0099_R <- transform(LOG_0099, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0099_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0099_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY411_REC_MOT_LD <- left_join(FLY411_REC_MOT_ready, LOG_0099_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY411_REC_MOT_LD_1 <- FLY411_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY411_REC_MOT_LD_1.2 <- FLY411_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY411_REC_MOT_LD_1.2 <- azzera_dataset(FLY411_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY411_REC_MOT_LD_1.2 <- FLY411_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY411")
# Add seconds column
FLY411_REC_MOT_LD_1.2 <- FLY411_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# check results by plotting
ggplot(FLY411_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  #geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 09:16:31 (FLY_411 LOG_0099)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY411_REC_MOT_LD_1.2$tilt_corrected <- FLY411_REC_MOT_LD_1.2$tilt_deg - 5
FLY411_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY411_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY411_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY411_REC_MOT_LD_1.2$tilt_corrected, FLY411_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY411_REC_MOT_LD_1.2 <- correct_altitude(FLY411_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY411_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  #geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 09:16:31 (FLY_411 LOG_0099)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY411_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY411_REC_MOT_LD_1.2 <- GAP3(FLY411_REC_MOT_LD_1.2, "laser_altitude_m", "osd_data:relativeHeight[meters]", list(c(21, 291)))
FLY411_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY411_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_411<- sd(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_411<- sd(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY411_REC_MOT_LD_1.2$relative_diff <- FLY411_REC_MOT_LD_1.2$diff_col / mean(FLY411_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY411_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_412 + LOG100 ####
# Import csv
FLY412_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY412_REC_MOT.csv")
# Import LOG 
LOG_0100 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0100.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY412_REC_MOT_clean <- distinct(FLY412_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY412_REC_MOT_ready <- FLY412_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0100_R <- transform(LOG_0100, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0100_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0100_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY412_REC_MOT_LD <- left_join(FLY412_REC_MOT_ready, LOG_0100_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY412_REC_MOT_LD_1 <- FLY412_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY412_REC_MOT_LD_1.2 <- FLY412_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30"
FLY412_REC_MOT_LD_1.2_clean <- subset(FLY412_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# offset BAR to match with Lidar, max 3 rows meaning 
FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY412_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY412_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY412_REC_MOT_LD_1.2_clean <- FLY412_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY412")
# Add seconds column
FLY412_REC_MOT_LD_1.2_clean <- FLY412_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY412_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY412_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 70, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY412_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 09:29:32 (FLY_412 LOG_0100)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY412_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY412_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY412_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY412_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY412_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY412_REC_MOT_LD_1.2_clean$tilt_corrected, FLY412_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY412_REC_MOT_LD_1.2_clean <- correct_altitude(FLY412_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY412_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 09:29:32 (FLY_412 LOG_0100)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY412_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY412_REC_MOT_LD_1.2_clean <- GAP3(FLY412_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(81, 101),c(111,161),c(211,235)))
FLY412_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY412_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_412<- sd(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_412<- sd(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY412_REC_MOT_LD_1.2_clean$relative_diff <- FLY412_REC_MOT_LD_1.2_clean$diff_col / mean(FLY412_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY412_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_414 + LOG103 ####
# Import csv
FLY414_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY414_REC_MOT.csv")
# Import LOG 
LOG_0103 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0103.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY414_REC_MOT_clean <- distinct(FLY414_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY414_REC_MOT_ready <- FLY414_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0103_R <- transform(LOG_0103, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0103_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0103_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY414_REC_MOT_LD <- left_join(FLY414_REC_MOT_ready, LOG_0103_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY414_REC_MOT_LD_1 <- FLY414_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY414_REC_MOT_LD_1.2 <- FLY414_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY414_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY414_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY414_REC_MOT_LD_1.2 <- azzera_dataset(FLY414_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY414_REC_MOT_LD_1.2 <- FLY414_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY414")
# Add seconds column
FLY414_REC_MOT_LD_1.2 <- FLY414_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY414_REC_MOT_LD_1.2 <- spike_cleaning(FLY414_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY414_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 16:18:31 (FLY_414 LOG_0103)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY414_REC_MOT_LD_1.2$tilt_corrected <- FLY414_REC_MOT_LD_1.2$tilt_deg - 5
FLY414_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY414_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY414_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY414_REC_MOT_LD_1.2$tilt_corrected, FLY414_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY414_REC_MOT_LD_1.2 <- correct_altitude(FLY414_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY414_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-06 16:18:31 (FLY_414 LOG_0103)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY414_REC_MOT_LD_1.2), by = 20))

 # Calcola Gap 
FLY414_REC_MOT_LD_1.2 <- GAP3(FLY414_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 151), c(191,231)))
FLY414_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY414_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_414<- sd(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_414<- sd(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY414_REC_MOT_LD_1.2$relative_diff <- FLY414_REC_MOT_LD_1.2$diff_col / mean(FLY414_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY414_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_417 + LOG106 ####
# Import csv
FLY417_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY417_REC_MOT.csv")
# Import LOG 
LOG_0106 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0106.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY417_REC_MOT_clean <- distinct(FLY417_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY417_REC_MOT_ready <- FLY417_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0106_R <- transform(LOG_0106, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0106_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0106_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY417_REC_MOT_LD <- left_join(FLY417_REC_MOT_ready, LOG_0106_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY417_REC_MOT_LD_1 <- FLY417_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY417_REC_MOT_LD_1.2 <- FLY417_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY417_REC_MOT_LD_1.2 <- azzera_dataset(FLY417_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY417_REC_MOT_LD_1.2 <- FLY417_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY417")
# Add seconds column
FLY417_REC_MOT_LD_1.2 <- FLY417_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY417_REC_MOT_LD_1.2 <- spike_cleaning(FLY417_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-24 10:29:31 (FLY_417 LOG_0106)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY417_REC_MOT_LD_1.2$tilt_corrected <- FLY417_REC_MOT_LD_1.2$tilt_deg - 5
FLY417_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY417_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY417_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY417_REC_MOT_LD_1.2$tilt_corrected, FLY417_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY417_REC_MOT_LD_1.2 <- correct_altitude(FLY417_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-05-24 10:29:31 (FLY_417 LOG_0106)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY417_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY417_REC_MOT_LD_1.2 <- GAP3(FLY417_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 51), c(61,151), c(191,211), c(371,561),c(608,621)))
FLY417_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_417<- sd(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_417<- sd(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY417_REC_MOT_LD_1.2$relative_diff <- FLY417_REC_MOT_LD_1.2$diff_col / mean(FLY417_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_420 + LOG108 ####
# Import csv
FLY420_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY420_REC_MOT.csv")
# Import LOG 
LOG_0108 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0108.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY420_REC_MOT_clean <- distinct(FLY420_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY420_REC_MOT_ready <- FLY420_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0108_R <- transform(LOG_0108, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0108_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0108_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY420_REC_MOT_LD <- left_join(FLY420_REC_MOT_ready, LOG_0108_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY420_REC_MOT_LD_1 <- FLY420_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY420_REC_MOT_LD_1.2 <- FLY420_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY420_REC_MOT_LD_1.2_clean <- subset(FLY420_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# offset BAR to match with Lidar, max 3 rows meaning 
FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY420_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY420_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY420_REC_MOT_LD_1.2_clean <- FLY420_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY420")
# Add seconds column
FLY420_REC_MOT_LD_1.2_clean <- FLY420_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY420_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY420_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-17 09:19:13 (FLY_420 LOG_0108)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY420_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY420_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY420_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY420_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY420_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY420_REC_MOT_LD_1.2_clean$tilt_corrected, FLY420_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY420_REC_MOT_LD_1.2_clean <- correct_altitude(FLY420_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-17 09:19:13 (FLY_420 LOG_0108)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY420_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY420_REC_MOT_LD_1.2_clean <- GAP3(FLY420_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(25, 40),c(61,161),c(221,421),c(471,491),c(581,591),c(611,631)))
FLY420_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_420<- sd(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_420<- sd(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY420_REC_MOT_LD_1.2_clean$relative_diff <- FLY420_REC_MOT_LD_1.2_clean$diff_col / mean(FLY420_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### FLY_421 + LOG109 ####
# Import csv
FLY421_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY421_REC_MOT.csv")
# Import LOG 
LOG_0109 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0109.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY421_REC_MOT_clean <- distinct(FLY421_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY421_REC_MOT_ready <- FLY421_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0109_R <- transform(LOG_0109, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0109_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0109_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY421_REC_MOT_LD <- left_join(FLY421_REC_MOT_ready, LOG_0109_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY421_REC_MOT_LD_1 <- FLY421_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY421_REC_MOT_LD_1.2 <- FLY421_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY421_REC_MOT_LD_1.2 <- azzera_dataset(FLY421_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY421_REC_MOT_LD_1.2 <- FLY421_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY421")

# Pulizia spikes 
FLY421_REC_MOT_LD_1.2 <- spike_cleaning(FLY421_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 30, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY421_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-17 11:03:01 (FLY_421 LOG_109)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY421_REC_MOT_LD_1.2$tilt_corrected <- FLY421_REC_MOT_LD_1.2$tilt_deg - 5
FLY421_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY421_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY421_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY421_REC_MOT_LD_1.2$tilt_corrected, FLY421_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY421_REC_MOT_LD_1.2 <- correct_altitude(FLY421_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY421_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-17 11:03:01 (FLY_421 LOG_109)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY421_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY421_REC_MOT_LD_1.2 <- GAP3(FLY421_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 155),c(171,191),c(301,311)))
FLY421_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY421_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_421<- sd(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_421<- sd(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY421_REC_MOT_LD_1.2$relative_diff <- FLY421_REC_MOT_LD_1.2$diff_col / mean(FLY421_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY421_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_423 + LOG112 ####
# Import csv
FLY423_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY423_REC_MOT.csv")
# Import LOG 
LOG_0112 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0112.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY423_REC_MOT_clean <- distinct(FLY423_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY423_REC_MOT_ready <- FLY423_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0112_R <- transform(LOG_0112, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0112_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0112_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY423_REC_MOT_LD <- left_join(FLY423_REC_MOT_ready, LOG_0112_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY423_REC_MOT_LD_1 <- FLY423_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY423_REC_MOT_LD_1.2 <- FLY423_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY423_REC_MOT_LD_1.2 <- azzera_dataset(FLY423_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY423_REC_MOT_LD_1.2 <- FLY423_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY423")

# Pulizia spikes 
FLY423_REC_MOT_LD_1.2 <- spike_cleaning(FLY423_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 5, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY423_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 05:37:01 (FLY_423)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY423_REC_MOT_LD_1.2$tilt_corrected <- FLY423_REC_MOT_LD_1.2$tilt_deg - 5
FLY423_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY423_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY423_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY423_REC_MOT_LD_1.2$tilt_corrected, FLY423_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY423_REC_MOT_LD_1.2 <- correct_altitude(FLY423_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY423_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 05:37:01 (FLY_423)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY423_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY423_REC_MOT_LD_1.2 <- GAP3(FLY423_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 51),c(571,581)))
FLY423_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY423_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_423<- sd(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_423<- sd(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY423_REC_MOT_LD_1.2$relative_diff <- FLY423_REC_MOT_LD_1.2$diff_col / mean(FLY423_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY423_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

# add seconds column
FLY423_REC_MOT_LD_1.2 <- FLY423_REC_MOT_LD_1.2 %>%
  mutate(
    seconds = ifelse(!is.na(GPS.dateTimeStamp), cumsum(!is.na(GPS.dateTimeStamp)), NA))

# Plot cloud

df_FLY423 <- FLY423_REC_MOT_LD_1.2 %>% select(seconds, relative_diff)
df_FLY423 <- df_FLY423 %>% rename(seconds_FLY423 = seconds)
# Rinomina le colonne nel dataframe df_FLY304
df_FLY423 <- df_FLY423 %>% rename_all(~sub("relative_diff_", "", .))


ggplot(df_FLY423, aes(x = seconds_FLY423, y = relative_diff)) +
  geom_point(color = "black", size = 0.2) +
  labs(x = "Seconds", y = "Values", title = "Your Plot Title") +
  theme_minimal()

#### FLY_424 + LOG113 ####
# Import csv
FLY424_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY424_REC_MOT.csv")
# Import LOG 
LOG_0113 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0113.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY424_REC_MOT_clean <- distinct(FLY424_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY424_REC_MOT_ready <- FLY424_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0113_R <- transform(LOG_0113, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0113_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0113_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY424_REC_MOT_LD <- left_join(FLY424_REC_MOT_ready, LOG_0113_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY424_REC_MOT_LD_1 <- FLY424_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY424_REC_MOT_LD_1.2 <- FLY424_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY424_REC_MOT_LD_1.2 <- azzera_dataset(FLY424_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY424_REC_MOT_LD_1.2 <- FLY424_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY424")
# Add seconds column
FLY424_REC_MOT_LD_1.2 <- FLY424_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY424_REC_MOT_LD_1.2 <- spike_cleaning(FLY424_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY424_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:02:01 (FLY_424 LOG_113)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY424_REC_MOT_LD_1.2$tilt_corrected <- FLY424_REC_MOT_LD_1.2$tilt_deg - 5
FLY424_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY424_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY424_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY424_REC_MOT_LD_1.2$tilt_corrected, FLY424_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY424_REC_MOT_LD_1.2 <- correct_altitude(FLY424_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY424_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:02:01 (FLY_424 LOG_113)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY424_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY424_REC_MOT_LD_1.2 <- GAP3(FLY424_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 165)))
FLY424_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY424_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_424<- sd(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_424<- sd(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY424_REC_MOT_LD_1.2$relative_diff <- FLY424_REC_MOT_LD_1.2$diff_col / mean(FLY424_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY424_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_425 + LOG115 ####
# Import csv
FLY425_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY425_REC_MOT.csv")
# Import LOG 
LOG_0115 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0115.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY425_REC_MOT_clean <- distinct(FLY425_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY425_REC_MOT_ready <- FLY425_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0115_R <- transform(LOG_0115, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0115_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0115_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY425_REC_MOT_LD <- left_join(FLY425_REC_MOT_ready, LOG_0115_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY425_REC_MOT_LD_1 <- FLY425_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY425_REC_MOT_LD_1.2 <- FLY425_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY425_REC_MOT_LD_1.2 <- azzera_dataset(FLY425_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY425_REC_MOT_LD_1.2 <- FLY425_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY425")
# Add seconds column
FLY425_REC_MOT_LD_1.2 <- FLY425_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY425_REC_MOT_LD_1.2 <- spike_cleaning(FLY425_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY425_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:15:01 (FLY_425 LOG_115)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY425_REC_MOT_LD_1.2$tilt_corrected <- FLY425_REC_MOT_LD_1.2$tilt_deg - 5
FLY425_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY425_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY425_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY425_REC_MOT_LD_1.2$tilt_corrected, FLY425_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY425_REC_MOT_LD_1.2 <- correct_altitude(FLY425_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY425_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:15:01 (FLY_425 LOG_115)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY425_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY425_REC_MOT_LD_1.2 <- GAP3(FLY425_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 161),c(331,365)))
FLY425_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY425_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_425<- sd(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_425<- sd(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY425_REC_MOT_LD_1.2$relative_diff <- FLY425_REC_MOT_LD_1.2$diff_col / mean(FLY425_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY425_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_427 + LOG117 ####
# Import csv
FLY427_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY427_REC_MOT.csv")
# Import LOG 
LOG_0117 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0117.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY427_REC_MOT_clean <- distinct(FLY427_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY427_REC_MOT_ready <- FLY427_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0117_R <- transform(LOG_0117, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0117_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0117_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY427_REC_MOT_LD <- left_join(FLY427_REC_MOT_ready, LOG_0117_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY427_REC_MOT_LD_1 <- FLY427_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY427_REC_MOT_LD_1.2 <- FLY427_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY427_REC_MOT_LD_1.2 <- azzera_dataset(FLY427_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY427_REC_MOT_LD_1.2 <- FLY427_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY427")
# Add seconds column
FLY427_REC_MOT_LD_1.2 <- FLY427_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY427_REC_MOT_LD_1.2 <- spike_cleaning(FLY427_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 1, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY427_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:30:25 (FLY_427 LOG_117)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY427_REC_MOT_LD_1.2$tilt_corrected <- FLY427_REC_MOT_LD_1.2$tilt_deg - 5
FLY427_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY427_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY427_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY427_REC_MOT_LD_1.2$tilt_corrected, FLY427_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY427_REC_MOT_LD_1.2 <- correct_altitude(FLY427_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY427_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 09:30:25 (FLY_427 LOG_117)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY427_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY427_REC_MOT_LD_1.2 <- GAP3(FLY427_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(301, 361),c(371,431)))
FLY427_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY427_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_427<- sd(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_427<- sd(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY427_REC_MOT_LD_1.2$relative_diff <- FLY427_REC_MOT_LD_1.2$diff_col / mean(FLY427_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY427_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_429 + LOG124 ####
# Import csv
FLY429_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY429_REC_MOT.csv")
# Import LOG 
LOG_0124 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0124.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY429_REC_MOT_clean <- distinct(FLY429_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY429_REC_MOT_ready <- FLY429_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0124_R <- transform(LOG_0124, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0124_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0124_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY429_REC_MOT_LD <- left_join(FLY429_REC_MOT_ready, LOG_0124_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY429_REC_MOT_LD_1 <- FLY429_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY429_REC_MOT_LD_1.2 <- FLY429_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY429_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY429_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY429_REC_MOT_LD_1.2 <- azzera_dataset(FLY429_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY429_REC_MOT_LD_1.2 <- FLY429_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY429")
# Add seconds column
FLY429_REC_MOT_LD_1.2 <- FLY429_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY429_REC_MOT_LD_1.2 <- spike_cleaning(FLY429_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 1, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY429_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 14:32:10 (FLY_429 LOG_124)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY429_REC_MOT_LD_1.2$tilt_corrected <- FLY429_REC_MOT_LD_1.2$tilt_deg - 5
FLY429_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY429_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY429_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY429_REC_MOT_LD_1.2$tilt_corrected, FLY429_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY429_REC_MOT_LD_1.2 <- correct_altitude(FLY429_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY429_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 14:32:10 (FLY_429 LOG_124)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY429_REC_MOT_LD_1.2), by = 10))

# Calcola Gap 
FLY429_REC_MOT_LD_1.2 <- GAP3(FLY429_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(16,26), c(41, 121), c(131, 141)))
FLY429_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY429_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_429<- sd(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_429<- sd(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY429_REC_MOT_LD_1.2$relative_diff <- FLY429_REC_MOT_LD_1.2$diff_col / mean(FLY429_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY429_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_430 + LOG125 ####
# Import csv
FLY430_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY430_REC_MOT.csv")
# Import LOG 
LOG_0125 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0125.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY430_REC_MOT_clean <- distinct(FLY430_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY430_REC_MOT_ready <- FLY430_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0125_R <- transform(LOG_0125, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0125_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0125_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY430_REC_MOT_LD <- left_join(FLY430_REC_MOT_ready, LOG_0125_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY430_REC_MOT_LD_1 <- FLY430_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY430_REC_MOT_LD_1.2 <- FLY430_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY430_REC_MOT_LD_1.2_clean <- subset(FLY430_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Azzera accensione motori
FLY430_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY430_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY430_REC_MOT_LD_1.2_clean <- FLY430_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY430")
# Add seconds column
FLY430_REC_MOT_LD_1.2_clean <- FLY430_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY430_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY430_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY430_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 14:38:45 (FLY_430 LOG_125)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY430_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY430_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY430_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY430_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY430_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY430_REC_MOT_LD_1.2_clean$tilt_corrected, FLY430_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY430_REC_MOT_LD_1.2_clean <- correct_altitude(FLY430_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY430_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 14:38:45 (FLY_430 LOG_125)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY430_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY430_REC_MOT_LD_1.2_clean <- GAP3(FLY430_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 101), c(161, 221)))
FLY430_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY430_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_430<- sd(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_430<- sd(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY430_REC_MOT_LD_1.2_clean$relative_diff <- FLY430_REC_MOT_LD_1.2_clean$diff_col / mean(FLY430_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY430_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_431 + LOG127 ####
# Import csv
FLY431_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY431_REC_MOT.csv")
# Import LOG 
LOG_0127 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0127.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY431_REC_MOT_clean <- distinct(FLY431_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY431_REC_MOT_ready <- FLY431_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0127_R <- transform(LOG_0127, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0127_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0127_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY431_REC_MOT_LD <- left_join(FLY431_REC_MOT_ready, LOG_0127_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY431_REC_MOT_LD_1 <- FLY431_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY431_REC_MOT_LD_1.2 <- FLY431_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY431_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY431_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY431_REC_MOT_LD_1.2 <- azzera_dataset(FLY431_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY431_REC_MOT_LD_1.2 <- FLY431_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY431")
# Add seconds column
FLY431_REC_MOT_LD_1.2 <- FLY431_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY431_REC_MOT_LD_1.2 <- spike_cleaning(FLY431_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY431_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 15:01:10 (FLY_431 LOG_127)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY431_REC_MOT_LD_1.2$tilt_corrected <- FLY431_REC_MOT_LD_1.2$tilt_deg - 5
FLY431_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY431_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY431_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY431_REC_MOT_LD_1.2$tilt_corrected, FLY431_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY431_REC_MOT_LD_1.2 <- correct_altitude(FLY431_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY431_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-18 15:01:10 (FLY_431 LOG_127)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY431_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY431_REC_MOT_LD_1.2 <- GAP3(FLY431_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(11, 21), c(81,91), c(111,221)))
FLY431_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY431_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_431<- sd(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_431<- sd(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY431_REC_MOT_LD_1.2$relative_diff <- FLY431_REC_MOT_LD_1.2$diff_col / mean(FLY431_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY431_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_434 + LOG129 ####
# Import csv
FLY434_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY434_REC_MOT.csv")
# Import LOG 
LOG_0129 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0129.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY434_REC_MOT_clean <- distinct(FLY434_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY434_REC_MOT_ready <- FLY434_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0129_R <- transform(LOG_0129, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0129_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0129_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY434_REC_MOT_LD <- left_join(FLY434_REC_MOT_ready, LOG_0129_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY434_REC_MOT_LD_1 <- FLY434_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY434_REC_MOT_LD_1.2 <- FLY434_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY434_REC_MOT_LD_1.2 <- azzera_dataset(FLY434_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY434_REC_MOT_LD_1.2 <- FLY434_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY434")
# Add seconds column
FLY434_REC_MOT_LD_1.2 <- FLY434_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY434_REC_MOT_LD_1.2 <- spike_cleaning(FLY434_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY434_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-27 06:49:40 (FLY_434 LOG_129)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY434_REC_MOT_LD_1.2$tilt_corrected <- FLY434_REC_MOT_LD_1.2$tilt_deg - 5
FLY434_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY434_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY434_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY434_REC_MOT_LD_1.2$tilt_corrected, FLY434_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY434_REC_MOT_LD_1.2 <- correct_altitude(FLY434_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY434_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-06-27 06:49:40 (FLY_434 LOG_129)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY434_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY434_REC_MOT_LD_1.2 <- GAP3(FLY434_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(81, 301), c(361,411)))
FLY434_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY434_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_434<- sd(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_434<- sd(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY434_REC_MOT_LD_1.2$relative_diff <- FLY434_REC_MOT_LD_1.2$diff_col / mean(FLY434_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY434_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_436 + LOG131 ####
# Import csv
FLY436_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY436_REC_MOT.csv")
# Import LOG 
LOG_0131 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0131.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY436_REC_MOT_clean <- distinct(FLY436_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY436_REC_MOT_ready <- FLY436_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0131_R <- transform(LOG_0131, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0131_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0131_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY436_REC_MOT_LD <- left_join(FLY436_REC_MOT_ready, LOG_0131_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY436_REC_MOT_LD_1 <- FLY436_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY436_REC_MOT_LD_1.2 <- FLY436_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY436_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY436_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY436_REC_MOT_LD_1.2 <- azzera_dataset(FLY436_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY436_REC_MOT_LD_1.2 <- FLY436_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY436")
# Add seconds column
FLY436_REC_MOT_LD_1.2 <- FLY436_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY436_REC_MOT_LD_1.2 <- spike_cleaning(FLY436_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY436_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 13:05:32 (FLY_436 LOG_131)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY436_REC_MOT_LD_1.2$tilt_corrected <- FLY436_REC_MOT_LD_1.2$tilt_deg - 5
FLY436_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY436_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY436_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY436_REC_MOT_LD_1.2$tilt_corrected, FLY436_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY436_REC_MOT_LD_1.2 <- correct_altitude(FLY436_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY436_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 13:05:32 (FLY_436 LOG_131)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY436_REC_MOT_LD_1.2), by = 20))

# Calcola Gap221
FLY436_REC_MOT_LD_1.2 <- GAP3(FLY436_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(81, 101), c(121,221)))
FLY436_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY436_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_436<- sd(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_436<- sd(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY436_REC_MOT_LD_1.2$relative_diff <- FLY436_REC_MOT_LD_1.2$diff_col / mean(FLY436_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY436_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_437 + LOG132 ####
# Import csv
FLY437_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY437_REC_MOT.csv")
# Import LOG 
LOG_0132 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0132.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY437_REC_MOT_clean <- distinct(FLY437_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY437_REC_MOT_ready <- FLY437_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0132_R <- transform(LOG_0132, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0132_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0132_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY437_REC_MOT_LD <- left_join(FLY437_REC_MOT_ready, LOG_0132_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY437_REC_MOT_LD_1 <- FLY437_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY437_REC_MOT_LD_1.2 <- FLY437_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY437_REC_MOT_LD_1.2 <- azzera_dataset(FLY437_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY437_REC_MOT_LD_1.2 <- FLY437_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY437")
# Add seconds column
FLY437_REC_MOT_LD_1.2 <- FLY437_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY437_REC_MOT_LD_1.2 <- spike_cleaning(FLY437_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY437_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 13:27:01 (FLY_437 LOG_132)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY437_REC_MOT_LD_1.2$tilt_corrected <- FLY437_REC_MOT_LD_1.2$tilt_deg - 5
FLY437_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY437_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY437_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY437_REC_MOT_LD_1.2$tilt_corrected, FLY437_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY437_REC_MOT_LD_1.2 <- correct_altitude(FLY437_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY437_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 13:27:01 (FLY_437 LOG_132)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY437_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY437_REC_MOT_LD_1.2 <- GAP3(FLY437_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41,61), c(81,161), c(168,191), c(231,245),c(281,301)))
FLY437_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY437_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_437<- sd(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_437<- sd(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY437_REC_MOT_LD_1.2$relative_diff <- FLY437_REC_MOT_LD_1.2$diff_col / mean(FLY437_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY437_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### FLY_438 + LOG133 ####
# Import csv
FLY438_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY438_REC_MOT.csv")
# Import LOG 
LOG_0133 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0133.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY438_REC_MOT_clean <- distinct(FLY438_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY438_REC_MOT_ready <- FLY438_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0133_R <- transform(LOG_0133, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0133_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0133_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY438_REC_MOT_LD <- left_join(FLY438_REC_MOT_ready, LOG_0133_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY438_REC_MOT_LD_1 <- FLY438_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY438_REC_MOT_LD_1.2 <- FLY438_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY438_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY438_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY438_REC_MOT_LD_1.2 <- azzera_dataset(FLY438_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY438_REC_MOT_LD_1.2 <- FLY438_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY438")
# Add seconds column
FLY438_REC_MOT_LD_1.2 <- FLY438_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY438_REC_MOT_LD_1.2 <- spike_cleaning(FLY438_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY438_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 16:25:01 (FLY_438 LOG_133)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY438_REC_MOT_LD_1.2$tilt_corrected <- FLY438_REC_MOT_LD_1.2$tilt_deg - 5
FLY438_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY438_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY438_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY438_REC_MOT_LD_1.2$tilt_corrected, FLY438_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY438_REC_MOT_LD_1.2 <- correct_altitude(FLY438_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY438_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-11 16:25:01 (FLY_438 LOG_133)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY438_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY438_REC_MOT_LD_1.2 <- GAP3(FLY438_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 395), c(411, 431), c(461, 481)))
FLY438_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY438_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_438<- sd(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_438<- sd(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY438_REC_MOT_LD_1.2$relative_diff <- FLY438_REC_MOT_LD_1.2$diff_col / mean(FLY438_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY438_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_440 + LOG134 ####
# Import csv
FLY440_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY440_REC_MOT.csv")
# Import LOG 
LOG_0134 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0134.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY440_REC_MOT_clean <- distinct(FLY440_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY440_REC_MOT_ready <- FLY440_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0134_R <- transform(LOG_0134, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0134_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0134_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY440_REC_MOT_LD <- left_join(FLY440_REC_MOT_ready, LOG_0134_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY440_REC_MOT_LD_1 <- FLY440_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY440_REC_MOT_LD_1.2 <- FLY440_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY440_REC_MOT_LD_1.2 <- azzera_dataset(FLY440_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY440_REC_MOT_LD_1.2 <- FLY440_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY440")
# Add seconds column
FLY440_REC_MOT_LD_1.2 <- FLY440_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY440_REC_MOT_LD_1.2 <- spike_cleaning(FLY440_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY440_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-17 07:19:17 (FLY_440 LOG_134)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY440_REC_MOT_LD_1.2$tilt_corrected <- FLY440_REC_MOT_LD_1.2$tilt_deg - 5
FLY440_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY440_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY440_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY440_REC_MOT_LD_1.2$tilt_corrected, FLY440_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY440_REC_MOT_LD_1.2 <- correct_altitude(FLY440_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY440_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-17 07:19:17 (FLY_440 LOG_134)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY440_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY440_REC_MOT_LD_1.2 <- GAP3(FLY440_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 61), c(161,321), c(341,411),c(521,531),621,631))
FLY440_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY440_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_440<- sd(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_440<- sd(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY440_REC_MOT_LD_1.2$relative_diff <- FLY440_REC_MOT_LD_1.2$diff_col / mean(FLY440_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY440_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_444 + LOG138 #### 
# Import csv
FLY444_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY444_REC_MOT.csv")
# Import LOG 
LOG_0138 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0138.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY444_REC_MOT_clean <- distinct(FLY444_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY444_REC_MOT_ready <- FLY444_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0138_R <- transform(LOG_0138, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0138_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0138_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY444_REC_MOT_LD <- left_join(FLY444_REC_MOT_ready, LOG_0138_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY444_REC_MOT_LD_1 <- FLY444_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY444_REC_MOT_LD_1.2 <- FLY444_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY444_REC_MOT_LD_1.2 <- azzera_dataset(FLY444_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY444_REC_MOT_LD_1.2 <- FLY444_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY444")
# Add seconds column
FLY444_REC_MOT_LD_1.2 <- FLY444_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY444_REC_MOT_LD_1.2 <- spike_cleaning(FLY444_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY444_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-17 11:42:43 (FLY_444 LOG_138)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY444_REC_MOT_LD_1.2$tilt_corrected <- FLY444_REC_MOT_LD_1.2$tilt_deg - 5
FLY444_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY444_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY444_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY444_REC_MOT_LD_1.2$tilt_corrected, FLY444_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY444_REC_MOT_LD_1.2 <- correct_altitude(FLY444_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY444_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-17 11:42:43 (FLY_444 LOG_138)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY444_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY444_REC_MOT_LD_1.2 <- GAP3(FLY444_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(45, 201), c(221, 321), c(431,459)))
FLY444_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY444_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_444<- sd(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_444<- sd(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY444_REC_MOT_LD_1.2$relative_diff <- FLY444_REC_MOT_LD_1.2$diff_col / mean(FLY444_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY444_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### FLY_446 + LOG139 ####
# Import csv
FLY446_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY446_REC_MOT.csv")
# Import LOG 
LOG_0139 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0139.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY446_REC_MOT_clean <- distinct(FLY446_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY446_REC_MOT_ready <- FLY446_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0139_R <- transform(LOG_0139, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0139_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0139_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY446_REC_MOT_LD <- left_join(FLY446_REC_MOT_ready, LOG_0139_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY446_REC_MOT_LD_1 <- FLY446_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY446_REC_MOT_LD_1.2 <- FLY446_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY446_REC_MOT_LD_1.2 <- azzera_dataset(FLY446_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY446_REC_MOT_LD_1.2 <- FLY446_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY446")
# Add seconds column
FLY446_REC_MOT_LD_1.2 <- FLY446_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY446_REC_MOT_LD_1.2 <- spike_cleaning(FLY446_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY446_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-29 12:42:31 (FLY_446 LOG_139)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY446_REC_MOT_LD_1.2$tilt_corrected <- FLY446_REC_MOT_LD_1.2$tilt_deg - 5
FLY446_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY446_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY446_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY446_REC_MOT_LD_1.2$tilt_corrected, FLY446_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY446_REC_MOT_LD_1.2 <- correct_altitude(FLY446_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY446_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-29 12:42:31 (FLY_446 LOG_139)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY446_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY446_REC_MOT_LD_1.2 <- GAP3(FLY446_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 91), c(101,151)))
FLY446_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY446_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_446<- sd(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_446<- sd(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY446_REC_MOT_LD_1.2$relative_diff <- FLY446_REC_MOT_LD_1.2$diff_col / mean(FLY446_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY446_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_447 + LOG140 ####
# Import csv
FLY447_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY447_REC_MOT.csv")
# Import LOG 
LOG_0140 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0140.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY447_REC_MOT_clean <- distinct(FLY447_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY447_REC_MOT_ready <- FLY447_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0140_R <- transform(LOG_0140, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0140_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0140_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY447_REC_MOT_LD <- left_join(FLY447_REC_MOT_ready, LOG_0140_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY447_REC_MOT_LD_1 <- FLY447_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY447_REC_MOT_LD_1.2 <- FLY447_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY447_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY447_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY447_REC_MOT_LD_1.2 <- azzera_dataset(FLY447_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY447_REC_MOT_LD_1.2 <- FLY447_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY447")
# Add seconds column
FLY447_REC_MOT_LD_1.2 <- FLY447_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY447_REC_MOT_LD_1.2 <- spike_cleaning(FLY447_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY447_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "22023-07-29 12:56:18 (FLY_447 LOG_140)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY447_REC_MOT_LD_1.2$tilt_corrected <- FLY447_REC_MOT_LD_1.2$tilt_deg - 5
FLY447_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY447_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY447_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY447_REC_MOT_LD_1.2$tilt_corrected, FLY447_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY447_REC_MOT_LD_1.2 <- correct_altitude(FLY447_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY447_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-07-29 12:56:18 (FLY_447 LOG_140)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY447_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY447_REC_MOT_LD_1.2 <- GAP3(FLY447_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 51),c(81,131)))
FLY447_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY447_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_447<- sd(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_447<- sd(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY447_REC_MOT_LD_1.2$relative_diff <- FLY447_REC_MOT_LD_1.2$diff_col / mean(FLY447_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY447_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_452 + LOG144 ####
# Import csv
FLY452_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY452_REC_MOT.csv")
# Import LOG 
LOG_0144 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0144.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY452_REC_MOT_clean <- distinct(FLY452_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY452_REC_MOT_ready <- FLY452_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0144_R <- transform(LOG_0144, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0144_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0144_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY452_REC_MOT_LD <- left_join(FLY452_REC_MOT_ready, LOG_0144_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY452_REC_MOT_LD_1 <- FLY452_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY452_REC_MOT_LD_1.2 <- FLY452_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY452_REC_MOT_LD_1.2 <- azzera_dataset(FLY452_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY452_REC_MOT_LD_1.2 <- FLY452_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY452")
# Add seconds column
FLY452_REC_MOT_LD_1.2 <- FLY452_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY452_REC_MOT_LD_1.2 <- spike_cleaning(FLY452_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY452_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 13:07:48 (FLY_452 LOG_144)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY452_REC_MOT_LD_1.2$tilt_corrected <- FLY452_REC_MOT_LD_1.2$tilt_deg - 5
FLY452_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY452_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY452_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY452_REC_MOT_LD_1.2$tilt_corrected, FLY452_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY452_REC_MOT_LD_1.2 <- correct_altitude(FLY452_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY452_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 13:07:48 (FLY_452 LOG_144)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY452_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY452_REC_MOT_LD_1.2 <- GAP3(FLY452_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(1, 31), c(68,199),c(241,301)))
FLY452_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY452_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_452<- sd(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_452<- sd(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY452_REC_MOT_LD_1.2$relative_diff <- FLY452_REC_MOT_LD_1.2$diff_col / mean(FLY452_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY452_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_456 + LOG148 ####
# Import csv
FLY456_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY456_REC_MOT.csv")
# Import LOG 
LOG_0148 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0148.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY456_REC_MOT_clean <- distinct(FLY456_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY456_REC_MOT_ready <- FLY456_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0148_R <- transform(LOG_0148, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0148_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0148_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY456_REC_MOT_LD <- left_join(FLY456_REC_MOT_ready, LOG_0148_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY456_REC_MOT_LD_1 <- FLY456_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY456_REC_MOT_LD_1.2 <- FLY456_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY456_REC_MOT_LD_1.2 <- azzera_dataset(FLY456_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY456_REC_MOT_LD_1.2 <- FLY456_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY456")
# Add seconds column
FLY456_REC_MOT_LD_1.2 <- FLY456_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY456_REC_MOT_LD_1.2 <- spike_cleaning(FLY456_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY456_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:14:18 (FLY_456 LOG_148)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY456_REC_MOT_LD_1.2$tilt_corrected <- FLY456_REC_MOT_LD_1.2$tilt_deg - 5
FLY456_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY456_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY456_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY456_REC_MOT_LD_1.2$tilt_corrected, FLY456_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY456_REC_MOT_LD_1.2 <- correct_altitude(FLY456_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY456_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:14:18 (FLY_456 LOG_148)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY456_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY456_REC_MOT_LD_1.2 <- GAP3(FLY456_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(16, 161)))
FLY456_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY456_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_456<- sd(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_456<- sd(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY456_REC_MOT_LD_1.2$relative_diff <- FLY456_REC_MOT_LD_1.2$diff_col / mean(FLY456_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY456_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_457 + LOG149 ####
# Import csv
FLY457_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY457_REC_MOT.csv")
# Import LOG 
LOG_0149 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0149.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY457_REC_MOT_clean <- distinct(FLY457_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY457_REC_MOT_ready <- FLY457_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0149_R <- transform(LOG_0149, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0149_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0149_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY457_REC_MOT_LD <- left_join(FLY457_REC_MOT_ready, LOG_0149_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY457_REC_MOT_LD_1 <- FLY457_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY457_REC_MOT_LD_1.2 <- FLY457_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY457_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0,0, head(FLY457_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -4))
# Azzera accensione motori
FLY457_REC_MOT_LD_1.2 <- azzera_dataset(FLY457_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY457_REC_MOT_LD_1.2 <- FLY457_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY457")
# Add seconds column
FLY457_REC_MOT_LD_1.2 <- FLY457_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY457_REC_MOT_LD_1.2 <- spike_cleaning(FLY457_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY457_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:29:02 (FLY_457 LOG_149)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY457_REC_MOT_LD_1.2$tilt_corrected <- FLY457_REC_MOT_LD_1.2$tilt_deg - 5
FLY457_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY457_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY457_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY457_REC_MOT_LD_1.2$tilt_corrected, FLY457_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY457_REC_MOT_LD_1.2 <- correct_altitude(FLY457_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY457_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:29:02 (FLY_457 LOG_149)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY457_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY457_REC_MOT_LD_1.2 <- GAP3(FLY457_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 111)))
FLY457_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY457_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_457<- sd(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_457<- sd(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY457_REC_MOT_LD_1.2$relative_diff <- FLY457_REC_MOT_LD_1.2$diff_col / mean(FLY457_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY457_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_458 + LOG150 #### 
# Import csv
FLY458_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY458_REC_MOT.csv")
# Import LOG 
LOG_0150 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0150.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY458_REC_MOT_clean <- distinct(FLY458_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY458_REC_MOT_ready <- FLY458_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0150_R <- transform(LOG_0150, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0150_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0150_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY458_REC_MOT_LD <- left_join(FLY458_REC_MOT_ready, LOG_0150_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY458_REC_MOT_LD_1 <- FLY458_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY458_REC_MOT_LD_1.2 <- FLY458_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY458_REC_MOT_LD_1.2 <- azzera_dataset(FLY458_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY458_REC_MOT_LD_1.2 <- FLY458_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY458")
# Add seconds column
FLY458_REC_MOT_LD_1.2 <- FLY458_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY458_REC_MOT_LD_1.2 <- spike_cleaning(FLY458_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY458_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:38:38 (FLY_458 LOG_150)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY458_REC_MOT_LD_1.2$tilt_corrected <- FLY458_REC_MOT_LD_1.2$tilt_deg - 5
FLY458_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY458_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY458_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY458_REC_MOT_LD_1.2$tilt_corrected, FLY458_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY458_REC_MOT_LD_1.2 <- correct_altitude(FLY458_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY458_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-08 15:38:38 (FLY_458 LOG_150)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY458_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY458_REC_MOT_LD_1.2 <- GAP3(FLY458_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31, 141)))
FLY458_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY458_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_458<- sd(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_458<- sd(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY458_REC_MOT_LD_1.2$relative_diff <- FLY458_REC_MOT_LD_1.2$diff_col / mean(FLY458_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY458_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_459 + LOG151 ####
# Import csv
FLY459_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY459_REC_MOT.csv")
# Import LOG 
LOG_0151 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0151.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY459_REC_MOT_clean <- distinct(FLY459_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY459_REC_MOT_ready <- FLY459_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0151_R <- transform(LOG_0151, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0151_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0151_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY459_REC_MOT_LD <- left_join(FLY459_REC_MOT_ready, LOG_0151_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY459_REC_MOT_LD_1 <- FLY459_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY459_REC_MOT_LD_1.2 <- FLY459_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY459_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY459_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY459_REC_MOT_LD_1.2 <- azzera_dataset(FLY459_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY459_REC_MOT_LD_1.2 <- FLY459_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY459")
# Add seconds column
FLY459_REC_MOT_LD_1.2 <- FLY459_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY459_REC_MOT_LD_1.2 <- spike_cleaning(FLY459_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY459_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:57:52 (FLY_401 LOG_0094)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY459_REC_MOT_LD_1.2$tilt_corrected <- FLY459_REC_MOT_LD_1.2$tilt_deg - 5
FLY459_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY459_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY459_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY459_REC_MOT_LD_1.2$tilt_corrected, FLY459_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY459_REC_MOT_LD_1.2 <- correct_altitude(FLY459_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY459_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-02-22 09:57:52 (FLY_401 LOG_0094)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY459_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY459_REC_MOT_LD_1.2 <- GAP3(FLY459_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 151),c(161,411)))
FLY459_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY459_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_459<- sd(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_459<- sd(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY459_REC_MOT_LD_1.2$relative_diff <- FLY459_REC_MOT_LD_1.2$diff_col / mean(FLY459_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY459_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_461 + LOG152 ####
# Import csv
FLY461_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY461_REC_MOT.csv")
# Import LOG 
LOG_0152 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0152.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY461_REC_MOT_clean <- distinct(FLY461_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY461_REC_MOT_ready <- FLY461_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0152_R <- transform(LOG_0152, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0152_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0152_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY461_REC_MOT_LD <- left_join(FLY461_REC_MOT_ready, LOG_0152_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY461_REC_MOT_LD_1 <- FLY461_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY461_REC_MOT_LD_1.2 <- FLY461_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY461_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY461_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY461_REC_MOT_LD_1.2 <- azzera_dataset(FLY461_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY461_REC_MOT_LD_1.2 <- FLY461_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY461")
# Add seconds column
FLY461_REC_MOT_LD_1.2 <- FLY461_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY461_REC_MOT_LD_1.2 <- spike_cleaning(FLY461_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY461_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 09:42:47 (FLY_461 LOG_152)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY461_REC_MOT_LD_1.2$tilt_corrected <- FLY461_REC_MOT_LD_1.2$tilt_deg - 5
FLY461_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY461_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY461_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY461_REC_MOT_LD_1.2$tilt_corrected, FLY461_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY461_REC_MOT_LD_1.2 <- correct_altitude(FLY461_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY461_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 09:42:47 (FLY_461 LOG_152)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY461_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY461_REC_MOT_LD_1.2 <- GAP3(FLY461_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(41, 51), c(61,151), c(185,201), c(231,291),c(311,341),c(360,370),c(441,455)))
FLY461_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY461_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_461<- sd(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_461<- sd(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY461_REC_MOT_LD_1.2$relative_diff <- FLY461_REC_MOT_LD_1.2$diff_col / mean(FLY461_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY461_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_462 + LOG153 ####
# Import csv
FLY462_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY462_REC_MOT.csv")
# Import LOG 
LOG_0153 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0153.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY462_REC_MOT_clean <- distinct(FLY462_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY462_REC_MOT_ready <- FLY462_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0153_R <- transform(LOG_0153, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0153_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0153_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY462_REC_MOT_LD <- left_join(FLY462_REC_MOT_ready, LOG_0153_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY462_REC_MOT_LD_1 <- FLY462_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY462_REC_MOT_LD_1.2 <- FLY462_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY462_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0, head(FLY462_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -2))
# Azzera accensione motori
FLY462_REC_MOT_LD_1.2 <- azzera_dataset(FLY462_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY462_REC_MOT_LD_1.2 <- FLY462_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY462")
# Add seconds column
FLY462_REC_MOT_LD_1.2 <- FLY462_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY462_REC_MOT_LD_1.2 <- spike_cleaning(FLY462_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY462_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 11:30:56 (FLY_462 LOG_153)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY462_REC_MOT_LD_1.2$tilt_corrected <- FLY462_REC_MOT_LD_1.2$tilt_deg - 5
FLY462_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY462_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY462_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY462_REC_MOT_LD_1.2$tilt_corrected, FLY462_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY462_REC_MOT_LD_1.2 <- correct_altitude(FLY462_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY462_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 11:30:56 (FLY_462 LOG_153)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY462_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY462_REC_MOT_LD_1.2 <- GAP3(FLY462_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(11, 25), c(41,95), c(121,176)))
FLY462_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY462_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_462<- sd(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_462<- sd(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY462_REC_MOT_LD_1.2$relative_diff <- FLY462_REC_MOT_LD_1.2$diff_col / mean(FLY462_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY462_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_463 + LOG154  ####
# Import csv
FLY463_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY463_REC_MOT.csv")
# Import LOG 
LOG_0154 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0154.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY463_REC_MOT_clean <- distinct(FLY463_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY463_REC_MOT_ready <- FLY463_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0154_R <- transform(LOG_0154, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0154_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0154_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY463_REC_MOT_LD <- left_join(FLY463_REC_MOT_ready, LOG_0154_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY463_REC_MOT_LD_1 <- FLY463_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY463_REC_MOT_LD_1.2 <- FLY463_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY463_REC_MOT_LD_1.2 <- azzera_dataset(FLY463_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY463_REC_MOT_LD_1.2 <- FLY463_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY463")
# Add seconds column
FLY463_REC_MOT_LD_1.2 <- FLY463_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY463_REC_MOT_LD_1.2 <- spike_cleaning(FLY463_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY463_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 11:40:01 (FLY_463 LOG_154)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY463_REC_MOT_LD_1.2$tilt_corrected <- FLY463_REC_MOT_LD_1.2$tilt_deg - 5
FLY463_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY463_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY463_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY463_REC_MOT_LD_1.2$tilt_corrected, FLY463_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY463_REC_MOT_LD_1.2 <- correct_altitude(FLY463_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY463_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 11:40:01 (FLY_463 LOG_154)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY463_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY463_REC_MOT_LD_1.2 <- GAP3(FLY463_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(51, 65), c(91, 155), c(161, 341),c(401,421)))
FLY463_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY463_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_463<- sd(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_463<- sd(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY463_REC_MOT_LD_1.2$relative_diff <- FLY463_REC_MOT_LD_1.2$diff_col / mean(FLY463_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY463_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### FLY_464 + LOG155 ####
# Import csv
FLY464_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY464_REC_MOT.csv")
# Import LOG 
LOG_0155 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0155.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY464_REC_MOT_clean <- distinct(FLY464_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY464_REC_MOT_ready <- FLY464_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0155_R <- transform(LOG_0155, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0155_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0155_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY464_REC_MOT_LD <- left_join(FLY464_REC_MOT_ready, LOG_0155_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY464_REC_MOT_LD_1 <- FLY464_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY464_REC_MOT_LD_1.2 <- FLY464_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY464_REC_MOT_LD_1.2 <- azzera_dataset(FLY464_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY464_REC_MOT_LD_1.2 <- FLY464_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY464")
# Add seconds column
FLY464_REC_MOT_LD_1.2 <- FLY464_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY464_REC_MOT_LD_1.2 <- spike_cleaning(FLY464_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 50, omit_last_n = 0)

# check results by plotting
ggplot(FLY464_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 12:57:31 (FLY_464 LOG_155)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY464_REC_MOT_LD_1.2$tilt_corrected <- FLY464_REC_MOT_LD_1.2$tilt_deg - 5
FLY464_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY464_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY464_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY464_REC_MOT_LD_1.2$tilt_corrected, FLY464_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY464_REC_MOT_LD_1.2 <- correct_altitude(FLY464_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY464_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 12:57:31 (FLY_464 LOG_155)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY464_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY464_REC_MOT_LD_1.2 <- GAP3(FLY464_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(171, 241), c(271,351),c(381,431),c(471,491),c(561,581)))
FLY464_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY464_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_464<- sd(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_464<- sd(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY464_REC_MOT_LD_1.2$relative_diff <- FLY464_REC_MOT_LD_1.2$diff_col / mean(FLY464_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY464_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_466 + LOG157 ####
# Import csv
FLY466_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY466_REC_MOT.csv")
# Import LOG 
LOG_0157 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0157.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY466_REC_MOT_clean <- distinct(FLY466_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY466_REC_MOT_ready <- FLY466_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0157_R <- transform(LOG_0157, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0157_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0157_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY466_REC_MOT_LD <- left_join(FLY466_REC_MOT_ready, LOG_0157_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY466_REC_MOT_LD_1 <- FLY466_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY466_REC_MOT_LD_1.2 <- FLY466_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "2015-10-21" value
FLY466_REC_MOT_LD_1.2_clean <- subset(FLY466_REC_MOT_LD_1.2, !grepl("2015-10-21", GPS.dateTimeStamp))
# Azzera accensione motori
FLY466_REC_MOT_LD_1.2_clean <- azzera_dataset(FLY466_REC_MOT_LD_1.2_clean)
#Add Flight_ID column for future could plot
FLY466_REC_MOT_LD_1.2_clean <- FLY466_REC_MOT_LD_1.2_clean %>% mutate(Flight_ID = "FLY466")
# Add seconds column
FLY466_REC_MOT_LD_1.2_clean <- FLY466_REC_MOT_LD_1.2_clean %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY466_REC_MOT_LD_1.2_clean <- spike_cleaning(FLY466_REC_MOT_LD_1.2_clean, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY466_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 14:53:11 (FLY_466 LOG_157)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY466_REC_MOT_LD_1.2_clean$tilt_corrected <- FLY466_REC_MOT_LD_1.2_clean$tilt_deg - 5
FLY466_REC_MOT_LD_1.2_clean$tilt_corrected <- ifelse(
  is.na(FLY466_REC_MOT_LD_1.2_clean$tilt_corrected),
  NA,
  ifelse(FLY466_REC_MOT_LD_1.2_clean$tilt_corrected < 0, -FLY466_REC_MOT_LD_1.2_clean$tilt_corrected, FLY466_REC_MOT_LD_1.2_clean$tilt_corrected)
)

# Applicazione correzione tilt
FLY466_REC_MOT_LD_1.2_clean <- correct_altitude(FLY466_REC_MOT_LD_1.2_clean)

# check results by plotting
ggplot(FLY466_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-08-09 14:53:11 (FLY_466 LOG_157)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY466_REC_MOT_LD_1.2_clean), by = 20))

# Calcola Gap 
FLY466_REC_MOT_LD_1.2_clean <- GAP3(FLY466_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 28)))
FLY466_REC_MOT_LD_1.2_clean$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY466_REC_MOT_LD_1.2_clean, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) - sd(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE) + sd(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_466<- sd(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)/mean(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
sd_466<- sd(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)

# differenza relativa
FLY466_REC_MOT_LD_1.2_clean$relative_diff <- FLY466_REC_MOT_LD_1.2_clean$diff_col / mean(FLY466_REC_MOT_LD_1.2_clean$diff_col, na.rm = TRUE)
ggplot(FLY466_REC_MOT_LD_1.2_clean, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_472 + LOG_0161 ####
# Import csv
FLY472_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY472_REC_MOT.csv")
# Import LOG 
LOG_0161 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0161.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY472_REC_MOT_clean <- distinct(FLY472_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY472_REC_MOT_ready <- FLY472_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0161_R <- transform(LOG_0161, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0161_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0161_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY472_REC_MOT_LD <- left_join(FLY472_REC_MOT_ready, LOG_0161_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY472_REC_MOT_LD_1 <- FLY472_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY472_REC_MOT_LD_1.2 <- FLY472_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY472_REC_MOT_LD_1.2 <- azzera_dataset(FLY472_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY472_REC_MOT_LD_1.2 <- FLY472_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY472")
# Add seconds column
FLY472_REC_MOT_LD_1.2 <- FLY472_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY472_REC_MOT_LD_1.2 <- spike_cleaning(FLY472_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY472_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 09:46:27 (FLY_472 LOG_161)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY472_REC_MOT_LD_1.2$tilt_corrected <- FLY472_REC_MOT_LD_1.2$tilt_deg - 5
FLY472_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY472_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY472_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY472_REC_MOT_LD_1.2$tilt_corrected, FLY472_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY472_REC_MOT_LD_1.2 <- correct_altitude(FLY472_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY472_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 09:46:27 (FLY_472 LOG_161)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY472_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY472_REC_MOT_LD_1.2 <- GAP3(FLY472_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(101, 561)))
FLY472_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY472_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_472<- sd(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_472<- sd(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY472_REC_MOT_LD_1.2$relative_diff <- FLY472_REC_MOT_LD_1.2$diff_col / mean(FLY472_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY472_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()



#### FLY_475 + LOG_0164 ####
# Import csv
FLY475_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY475_REC_MOT.csv")
# Import LOG 
LOG_0164 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0164.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY475_REC_MOT_clean <- distinct(FLY475_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY475_REC_MOT_ready <- FLY475_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0164_R <- transform(LOG_0164, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0164_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0164_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY475_REC_MOT_LD <- left_join(FLY475_REC_MOT_ready, LOG_0164_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY475_REC_MOT_LD_1 <- FLY475_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY475_REC_MOT_LD_1.2 <- FLY475_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY475_REC_MOT_LD_1.2 <- azzera_dataset(FLY475_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY475_REC_MOT_LD_1.2 <- FLY475_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY475")
# Add seconds column
FLY475_REC_MOT_LD_1.2 <- FLY475_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY475_REC_MOT_LD_1.2 <- spike_cleaning(FLY475_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY475_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 13:10:01 (FLY_475 LOG_164)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY475_REC_MOT_LD_1.2$tilt_corrected <- FLY475_REC_MOT_LD_1.2$tilt_deg - 5
FLY475_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY475_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY475_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY475_REC_MOT_LD_1.2$tilt_corrected, FLY475_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY475_REC_MOT_LD_1.2 <- correct_altitude(FLY475_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY475_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 13:10:01 (FLY_475 LOG_164)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY475_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY475_REC_MOT_LD_1.2 <- GAP3(FLY475_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 131), c(201,231)))
FLY475_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY475_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_475<- sd(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_475<- sd(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY475_REC_MOT_LD_1.2$relative_diff <- FLY475_REC_MOT_LD_1.2$diff_col / mean(FLY475_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY475_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()



#### FLY_477 + LOG_0166 ####
FLY477_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY477_REC_MOT.csv")
# Import LOG 
LOG_0166 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0166.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY477_REC_MOT_clean <- distinct(FLY477_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY477_REC_MOT_ready <- FLY477_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0166_R <- transform(LOG_0166, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0166_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0166_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY477_REC_MOT_LD <- left_join(FLY477_REC_MOT_ready, LOG_0166_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY477_REC_MOT_LD_1 <- FLY477_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY477_REC_MOT_LD_1.2 <- FLY477_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY477_REC_MOT_LD_1.2 <- azzera_dataset(FLY477_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY477_REC_MOT_LD_1.2 <- FLY477_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY477")
# Add seconds column
FLY477_REC_MOT_LD_1.2 <- FLY477_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY477_REC_MOT_LD_1.2 <- spike_cleaning(FLY477_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY477_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 13:54:01 (FLY_477 LOG_166)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY477_REC_MOT_LD_1.2$tilt_corrected <- FLY477_REC_MOT_LD_1.2$tilt_deg - 5
FLY477_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY477_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY477_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY477_REC_MOT_LD_1.2$tilt_corrected, FLY477_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY477_REC_MOT_LD_1.2 <- correct_altitude(FLY477_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY477_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 13:54:01 (FLY_477 LOG_166)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY477_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY477_REC_MOT_LD_1.2 <- GAP3(FLY477_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 71), c(101,171)))
FLY477_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY477_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_477<- sd(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_477<- sd(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY477_REC_MOT_LD_1.2$relative_diff <- FLY477_REC_MOT_LD_1.2$diff_col / mean(FLY477_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY477_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_478 + LOG_0167 ####
# Import csv
FLY478_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY478_REC_MOT.csv")
# Import LOG 
LOG_0167 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0167.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY478_REC_MOT_clean <- distinct(FLY478_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY478_REC_MOT_ready <- FLY478_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0167_R <- transform(LOG_0167, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0167_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0167_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY478_REC_MOT_LD <- left_join(FLY478_REC_MOT_ready, LOG_0167_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY478_REC_MOT_LD_1 <- FLY478_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY478_REC_MOT_LD_1.2 <- FLY478_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# offset BAR to match with Lidar, max 3 rows meaning 
FLY478_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0, head(FLY478_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))
# Azzera accensione motori
FLY478_REC_MOT_LD_1.2 <- azzera_dataset(FLY478_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY478_REC_MOT_LD_1.2 <- FLY478_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY478")
# Add seconds column
FLY478_REC_MOT_LD_1.2 <- FLY478_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY478_REC_MOT_LD_1.2 <- spike_cleaning(FLY478_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY478_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 14:18:011 (FLY_478 LOG_167)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY478_REC_MOT_LD_1.2$tilt_corrected <- FLY478_REC_MOT_LD_1.2$tilt_deg - 5
FLY478_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY478_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY478_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY478_REC_MOT_LD_1.2$tilt_corrected, FLY478_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY478_REC_MOT_LD_1.2 <- correct_altitude(FLY478_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY478_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-09-11 14:18:011 (FLY_478 LOG_167)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY478_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY478_REC_MOT_LD_1.2 <- GAP3(FLY478_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(31,131),c(291),c(191,201)))
FLY478_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY478_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_478<- sd(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_478<- sd(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY478_REC_MOT_LD_1.2$relative_diff <- FLY478_REC_MOT_LD_1.2$diff_col / mean(FLY478_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY478_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_484 + LOG_0168 ####
# Import csv
FLY484_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY484_REC_MOT.csv")
# Import LOG 
LOG_0168 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0168.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY484_REC_MOT_clean <- distinct(FLY484_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY484_REC_MOT_ready <- FLY484_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0168_R <- transform(LOG_0168, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0168_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0168_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY484_REC_MOT_LD <- left_join(FLY484_REC_MOT_ready, LOG_0168_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY484_REC_MOT_LD_1 <- FLY484_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY484_REC_MOT_LD_1.2 <- FLY484_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY484_REC_MOT_LD_1.2 <- azzera_dataset(FLY484_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY484_REC_MOT_LD_1.2 <- FLY484_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY484")
# Add seconds column
FLY484_REC_MOT_LD_1.2 <- FLY484_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY484_REC_MOT_LD_1.2 <- spike_cleaning(FLY484_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 5, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY484_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-10-13 08:43:40 (FLY_484 LOG_168)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY484_REC_MOT_LD_1.2$tilt_corrected <- FLY484_REC_MOT_LD_1.2$tilt_deg - 5
FLY484_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY484_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY484_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY484_REC_MOT_LD_1.2$tilt_corrected, FLY484_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY484_REC_MOT_LD_1.2 <- correct_altitude(FLY484_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY484_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-10-13 08:43:40 (FLY_484 LOG_168)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY484_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY484_REC_MOT_LD_1.2 <- GAP3(FLY484_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(21, 101),c(211,321),c(561,581)))
FLY484_REC_MOT_LD_1.2$diff_col
mean(FLY484_REC_MOT_LD_1.2$diff_col[561:581],na.rm = TRUE)
# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY484_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_484<- sd(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_484<- sd(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY484_REC_MOT_LD_1.2$relative_diff <- FLY484_REC_MOT_LD_1.2$diff_col / mean(FLY484_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY484_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### FLY_487 + LOG_0170 ####
# Import csv
FLY487_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY487_REC_MOT.csv")
# Import LOG 
LOG_0170 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0170.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY487_REC_MOT_clean <- distinct(FLY487_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY487_REC_MOT_ready <- FLY487_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0170_R <- transform(LOG_0170, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0170_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0170_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY487_REC_MOT_LD <- left_join(FLY487_REC_MOT_ready, LOG_0170_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY487_REC_MOT_LD_1 <- FLY487_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY487_REC_MOT_LD_1.2 <- FLY487_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Azzera accensione motori
FLY487_REC_MOT_LD_1.2 <- azzera_dataset(FLY487_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY487_REC_MOT_LD_1.2 <- FLY487_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY487")
# Add seconds column
FLY487_REC_MOT_LD_1.2 <- FLY487_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY487_REC_MOT_LD_1.2 <- spike_cleaning(FLY487_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 10, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY487_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-10-14 15:19:01 (FLY_487 LOG_170)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY487_REC_MOT_LD_1.2$tilt_corrected <- FLY487_REC_MOT_LD_1.2$tilt_deg - 5
FLY487_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY487_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY487_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY487_REC_MOT_LD_1.2$tilt_corrected, FLY487_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY487_REC_MOT_LD_1.2 <- correct_altitude(FLY487_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY487_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2023-10-14 15:19:01 (FLY_487 LOG_170)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY487_REC_MOT_LD_1.2), by = 20))

# Calcola Gap 
FLY487_REC_MOT_LD_1.2 <- GAP3(FLY487_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(171, 381),c(401,481),c(511,521)))
FLY487_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY487_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_487<- sd(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_487<- sd(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY487_REC_MOT_LD_1.2$relative_diff <- FLY487_REC_MOT_LD_1.2$diff_col / mean(FLY487_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY487_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()


#### nuvola ####

library(dplyr)
library(purrr)

selected_columns <- c("Flight_ID", "seconds", "relative_diff")

all_flights <- bind_rows(
  map_dfr(
    list(
      FLY297_REC_MOT_LD_1.2, FLY298_REC_MOT_LD_1.2, FLY299_REC_MOT_LD_1.2, FLY304_REC_MOT_LD_1.2, FLY306_REC_MOT_LD_1.2,
      FLY307_REC_MOT_LD_1.2, FLY310_REC_MOT_LD_1.2, FLY311_REC_MOT_LD_1.2, FLY312_REC_MOT_LD_1.2, FLY313_REC_MOT_LD_1.2,
      FLY315_REC_MOT_LD_1.2, FLY316_REC_MOT_LD_1.2, FLY317_REC_MOT_LD_1.2, FLY318_REC_MOT_LD_1.2, FLY319_REC_MOT_LD_1.2,
      FLY323_REC_MOT_LD_1.2, FLY327_REC_MOT_LD_1.2, FLY330_REC_MOT_LD_1.2, FLY332_REC_MOT_LD_1.2, FLY352_REC_MOT_LD_1.2,
      FLY353_REC_MOT_LD_1.2, FLY354_REC_MOT_LD_1.2, FLY365_REC_MOT_LD_1.2, FLY368_REC_MOT_LD_1.2_clean, FLY371_REC_MOT_LD_1.2,
      FLY372_REC_MOT_LD_1.2, FLY374_REC_MOT_LD_1.2, FLY382_REC_MOT_LD_1.2, FLY383_REC_MOT_LD_1.2, FLY384_REC_MOT_LD_1.2,
      FLY387_REC_MOT_LD_1.2, FLY388_REC_MOT_LD_1.2, FLY389_REC_MOT_LD_1.2_clean, FLY393_REC_MOT_LD_1.2, FLY396_REC_MOT_LD_1.2_clean,
      FLY400_REC_MOT_LD_1.2, FLY401_REC_MOT_LD_1.2, FLY402_REC_MOT_LD_1.2, FLY411_REC_MOT_LD_1.2, FLY412_REC_MOT_LD_1.2_clean,
      FLY414_REC_MOT_LD_1.2, FLY417_REC_MOT_LD_1.2, FLY420_REC_MOT_LD_1.2_clean, FLY421_REC_MOT_LD_1.2, FLY423_REC_MOT_LD_1.2,
      FLY424_REC_MOT_LD_1.2, FLY425_REC_MOT_LD_1.2, FLY427_REC_MOT_LD_1.2, FLY429_REC_MOT_LD_1.2, FLY430_REC_MOT_LD_1.2_clean,
      FLY431_REC_MOT_LD_1.2, FLY434_REC_MOT_LD_1.2, FLY436_REC_MOT_LD_1.2, FLY437_REC_MOT_LD_1.2, FLY438_REC_MOT_LD_1.2,
      FLY440_REC_MOT_LD_1.2, FLY444_REC_MOT_LD_1.2, FLY446_REC_MOT_LD_1.2, FLY447_REC_MOT_LD_1.2, FLY452_REC_MOT_LD_1.2,
      FLY456_REC_MOT_LD_1.2, FLY457_REC_MOT_LD_1.2, FLY458_REC_MOT_LD_1.2, FLY459_REC_MOT_LD_1.2, FLY461_REC_MOT_LD_1.2,
      FLY462_REC_MOT_LD_1.2, FLY463_REC_MOT_LD_1.2, FLY464_REC_MOT_LD_1.2, FLY466_REC_MOT_LD_1.2_clean, FLY472_REC_MOT_LD_1.2,
      FLY475_REC_MOT_LD_1.2, FLY477_REC_MOT_LD_1.2, FLY478_REC_MOT_LD_1.2, FLY484_REC_MOT_LD_1.2, FLY487_REC_MOT_LD_1.2
    ),
    ~ select(.x, all_of(selected_columns))
  )
)

all_fligts_senza_393 <- subset(all_flights, Flight_ID!="FLY393")
all_flights_clean <- subset(all_fligts_senza_393, is.na(relative_diff)==FALSE)

ggplot(all_flights_clean, aes(x = seconds, y = relative_diff)) +
  geom_point(size = 0.1, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Seconds", y = "Relative Difference", title = "") +
  theme_minimal()

model_nuvola <- lm(relative_diff ~ seconds, data = all_flights)
summary(model_nuvola)

# Mixed effects
mixed_model<- lmer(relative_diff ~ scale(seconds) + (scale(seconds) | Flight_ID) , data = all_flights_clean, REML = FALSE,
     control = lmerControl(optimizer ="Nelder_Mead"))
summary(mixed_model)

plot_all_flights_colored <- ggplot(all_flights_clean, aes(x = seconds, y = relative_diff, color = Flight_ID)) +
                                    geom_point() +
                                    geom_smooth(method = "lm", fill = NA) 


plot_mfx <- plot_model(mixed_model, type = "pred", terms = c("seconds"), show.data = TRUE)

# la differenza diminuisce 
# correlato volo per volo e poi "fatto una 393media" delle varie regressioni 

#### bar vs LID andamento ####

plot(FLY297_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`,FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned)

# Lista dei nomi dei dataframes
dataframes <- c("FLY297_REC_MOT_LD_1.2", "FLY298_REC_MOT_LD_1.2", "FLY299_REC_MOT_LD_1.2", "FLY304_REC_MOT_LD_1.2", "FLY306_REC_MOT_LD_1.2", "FLY307_REC_MOT_LD_1.2", "FLY310_REC_MOT_LD_1.2", "FLY311_REC_MOT_LD_1.2", "FLY312_REC_MOT_LD_1.2",
                "FLY313_REC_MOT_LD_1.2", "FLY315_REC_MOT_LD_1.2", "FLY316_REC_MOT_LD_1.2", "FLY317_REC_MOT_LD_1.2", "FLY318_REC_MOT_LD_1.2", "FLY319_REC_MOT_LD_1.2", "FLY323_REC_MOT_LD_1.2", "FLY327_REC_MOT_LD_1.2", "FLY330_REC_MOT_LD_1.2",
                "FLY332_REC_MOT_LD_1.2", "FLY352_REC_MOT_LD_1.2", "FLY353_REC_MOT_LD_1.2", "FLY354_REC_MOT_LD_1.2", "FLY365_REC_MOT_LD_1.2", "FLY368_REC_MOT_LD_1.2_clean", "FLY371_REC_MOT_LD_1.2", "FLY372_REC_MOT_LD_1.2", "FLY374_REC_MOT_LD_1.2",
                "FLY382_REC_MOT_LD_1.2", "FLY383_REC_MOT_LD_1.2", "FLY384_REC_MOT_LD_1.2", "FLY387_REC_MOT_LD_1.2", "FLY388_REC_MOT_LD_1.2", "FLY389_REC_MOT_LD_1.2_clean", "FLY393_REC_MOT_LD_1.2", "FLY396_REC_MOT_LD_1.2_clean", "FLY400_REC_MOT_LD_1.2",
                "FLY401_REC_MOT_LD_1.2", "FLY402_REC_MOT_LD_1.2", "FLY411_REC_MOT_LD_1.2", "FLY412_REC_MOT_LD_1.2_clean", "FLY414_REC_MOT_LD_1.2", "FLY417_REC_MOT_LD_1.2", "FLY420_REC_MOT_LD_1.2_clean", "FLY421_REC_MOT_LD_1.2", "FLY423_REC_MOT_LD_1.2",
                "FLY424_REC_MOT_LD_1.2", "FLY425_REC_MOT_LD_1.2", "FLY427_REC_MOT_LD_1.2", "FLY429_REC_MOT_LD_1.2", "FLY430_REC_MOT_LD_1.2_clean", "FLY431_REC_MOT_LD_1.2", "FLY434_REC_MOT_LD_1.2", "FLY436_REC_MOT_LD_1.2", "FLY437_REC_MOT_LD_1.2",
                "FLY438_REC_MOT_LD_1.2", "FLY440_REC_MOT_LD_1.2", "FLY444_REC_MOT_LD_1.2", "FLY446_REC_MOT_LD_1.2", "FLY447_REC_MOT_LD_1.2", "FLY452_REC_MOT_LD_1.2", "FLY456_REC_MOT_LD_1.2", "FLY457_REC_MOT_LD_1.2", "FLY458_REC_MOT_LD_1.2",
                "FLY459_REC_MOT_LD_1.2", "FLY461_REC_MOT_LD_1.2", "FLY462_REC_MOT_LD_1.2", "FLY463_REC_MOT_LD_1.2", "FLY464_REC_MOT_LD_1.2", "FLY466_REC_MOT_LD_1.2_clean", "FLY472_REC_MOT_LD_1.2", "FLY475_REC_MOT_LD_1.2", "FLY477_REC_MOT_LD_1.2",
                "FLY478_REC_MOT_LD_1.2", "FLY484_REC_MOT_LD_1.2", "FLY487_REC_MOT_LD_1.2")

# Creazione del dataframe combinato
for (df_name in dataframes) {
  # Controlla se il dataframe esiste
  if(exists(df_name)) {
    df <- get(df_name)
    # Controlla se la colonna laser_altitude_m_cleaned esiste nel dataframe
    if("laser_altitude_m_cleaned" %in% colnames(df)) {
      # Controlla se ci sono dati nel dataframe
      if(nrow(df) > 0) {
        temp_df <- data.frame(
          relativeHeight = df$`osd_data:relativeHeight[meters]`,
          laserAltitude = df$laser_altitude_m_cleaned,
          flight = df_name
        )
        combined_df <- rbind(combined_df, temp_df)
      } else {
        warning(paste("Il dataframe", df_name, "non contiene dati."))
      }
    } else {
      warning(paste("Il dataframe", df_name, "non contiene la colonna laser_altitude_m_cleaned."))
    }
  } else {
    warning(paste("Il dataframe", df_name, "non esiste."))
  }
}

# Creazione del plot con ggplot
LvsB <- ggplot(combined_df, aes(x = relativeHeight, y = laserAltitude)) +
  geom_point(size = 0.8, color = "darkblue") + 
  labs(title = "Plot di laser_altitude in funzione di relativeHeight",
       x = "Relative Height (meters)",
       y = "Laser Altitude (meters)") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_minimal()

# Calcolo del modello di regressione lineare
model_LidarvsBar <- lm(laserAltitude ~ relativeHeight, data = combined_df)
summary(model_LidarvsBar)

# Stampa dei coefficienti del modello
coefficients <- coef(model)
cat("Coefficiente di intercetta (b0):", coefficients[1], "\n")
cat("Coefficiente di pendenza (b1):", coefficients[2], "\n")

# Visualizzazione del plot
print(LvsB)

#### cv totale ####

cv_list <- listc("cv_297", "cv_298", "cv_299", "cv_304", "cv_306", "cv_307", "cv_310", "cv_311", "cv_312", "cv_313", "cv_315", "cv_316", "cv_317", "cv_318", "cv_319",
                 "cv_323", "cv_327", "cv_330", "cv_332", "cv_352", "cv_353", "cv_354", "cv_365", "cv_368", "cv_371", "cv_372", "cv_374", "cv_382", "cv_383", "cv_384",
                 "cv_387", "cv_388", "cv_389", "cv_393", "cv_396", "cv_400", "cv_401", "cv_402", "cv_411", "cv_412", "cv_414", "cv_417", "cv_420", "cv_421", "cv_423",
                 "cv_424", "cv_425", "cv_427", "cv_429", "cv_430", "cv_431", "cv_434", "cv_436", "cv_437", "cv_438", "cv_440", "cv_444", "cv_446", "cv_447", "cv_452",
                 "cv_456", "cv_457", "cv_458", "cv_459", "cv_461", "cv_462", "cv_463", "cv_464", "cv_466", "cv_472", "cv_475", "cv_477", "cv_478", "cv_484", "cv_487")

# Creazione di una lista con gli oggetti cv
cv_list <- list(
  cv_297 = 0.1444723, cv_298 = 0.05579512, cv_299 = 0.1511531,
  cv_304 = 0.0853134, cv_306 = 0.1463682, cv_307 = 0.1472566,
  cv_310 = 0.1912035, cv_311 = 0.1370731, cv_312 = 0.07683446,
  cv_313 = 0.1166739, cv_315 = 0.2086831, cv_316 = 0.1237245,
  cv_317 = 0.09403244, cv_318 = 0.05894148, cv_323 = 0.0763046,
  cv_327 = 0.112316, cv_330 = 0.1414114, cv_332 = 0.1940729,
  cv_352 = 0.1213137, cv_353 = 0.06989027, cv_354 = 0.1247269,
  cv_365 = 0.2502964, cv_368 = 0.09246272, cv_371 = 0.08507439,
  cv_372 = 0.1000387, cv_374 = 0.05035636, cv_382 = 0.08854009,
  cv_383 = 0.1789651, cv_384 = 0.1858621, cv_387 = 0.09244433,
  cv_388 = 0.04981874, cv_389 = 0.06602301, 
  cv_396 = 0.08113355, cv_400 = 0.06378782, cv_401 = 0.111389,
  cv_402 = 0.1283862, cv_411 = 0.0696755, cv_412 = 0.1114703,
  cv_414 = 0.1711357, cv_417 = 0.1776356, cv_420 = 0.1176692,
  cv_421 = 0.07579473, cv_423 = 0.03262628, cv_424 = 0.0780387,
  cv_425 = 0.08402739, cv_427 = 0.09820985, cv_429 = 0.06261618,
  cv_430 = 0.06664889, cv_431 = 0.1264857, cv_434 = 0.08644645,
  cv_436 = 0.0695824, cv_437 = 0.1445564, cv_438 = 0.1299258,
  cv_440 = 0.08172845, cv_444 = 0.2010896, cv_446 = 0.1162806,
  cv_447 = 0.07827293, cv_452 = 0.07640321, cv_456 = 0.1939289,
  cv_457 = 0.06853291, cv_458 = 0.07218754, cv_459 = 0.05388282,
  cv_461 = 0.1287979, cv_462 = 0.0554924, cv_463 = 0.07683863,
  cv_464 = 0.1736759, cv_466 = 0.0260268, cv_472 = 0.06759123,
  cv_475 = 0.09545918, cv_477 = 0.1209379, cv_478 = 0.09199571,
  cv_484 = 0.0824654, cv_487 = 0.1163054
)

# Creazione del dataframe
cv_data <- data.frame(Dataframe = names(cv_list), CV = unlist(cv_list))

# Calcola la media dei coefficienti di variazione
mean_cv <- mean(unlist(cv_list))

# Creazione del grafico
ggplot(cv_data, aes(x = Dataframe, y = CV)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("Media: %.2f", mean_cv)), x = -Inf, y = Inf, hjust = 0, vjust = 1, size = 4, color = "blue") +
  geom_hline(yintercept = mean_cv, linetype = "dashed", color = "darkblue", size = 0.3) +  # Aggiungi una linea orizzontale per la media
  ggtitle("Coefficiente di Variazione della differenza tra LID e BAR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angolo dell'etichetta sull'asse x per la leggibilità

#### Statistiche descrittive ####

dataset_names <- c("FLY297_REC_MOT_LD_1.2", "FLY298_REC_MOT_LD_1.2", "FLY299_REC_MOT_LD_1.2", "FLY304_REC_MOT_LD_1.2", "FLY306_REC_MOT_LD_1.2", "FLY307_REC_MOT_LD_1.2", "FLY310_REC_MOT_LD_1.2", "FLY311_REC_MOT_LD_1.2", "FLY312_REC_MOT_LD_1.2", 
 "FLY313_REC_MOT_LD_1.2", "FLY315_REC_MOT_LD_1.2", "FLY316_REC_MOT_LD_1.2", "FLY317_REC_MOT_LD_1.2", "FLY318_REC_MOT_LD_1.2", "FLY319_REC_MOT_LD_1.2", "FLY323_REC_MOT_LD_1.2", "FLY327_REC_MOT_LD_1.2", "FLY330_REC_MOT_LD_1.2", 
 "FLY332_REC_MOT_LD_1.2", "FLY352_REC_MOT_LD_1.2", "FLY353_REC_MOT_LD_1.2", "FLY354_REC_MOT_LD_1.2", "FLY365_REC_MOT_LD_1.2", "FLY368_REC_MOT_LD_1.2_clean", "FLY371_REC_MOT_LD_1.2", "FLY372_REC_MOT_LD_1.2", "FLY374_REC_MOT_LD_1.2", 
 "FLY382_REC_MOT_LD_1.2", "FLY383_REC_MOT_LD_1.2", "FLY384_REC_MOT_LD_1.2", "FLY387_REC_MOT_LD_1.2", "FLY388_REC_MOT_LD_1.2", "FLY389_REC_MOT_LD_1.2_clean", "FLY393_REC_MOT_LD_1.2", "FLY396_REC_MOT_LD_1.2_clean", "FLY400_REC_MOT_LD_1.2", 
 "FLY401_REC_MOT_LD_1.2", "FLY402_REC_MOT_LD_1.2", "FLY411_REC_MOT_LD_1.2", "FLY412_REC_MOT_LD_1.2_clean", "FLY414_REC_MOT_LD_1.2", "FLY417_REC_MOT_LD_1.2", "FLY420_REC_MOT_LD_1.2_clean", "FLY421_REC_MOT_LD_1.2", "FLY423_REC_MOT_LD_1.2", 
 "FLY424_REC_MOT_LD_1.2", "FLY425_REC_MOT_LD_1.2", "FLY427_REC_MOT_LD_1.2", "FLY429_REC_MOT_LD_1.2", "FLY430_REC_MOT_LD_1.2_clean", "FLY431_REC_MOT_LD_1.2", "FLY434_REC_MOT_LD_1.2", "FLY436_REC_MOT_LD_1.2", "FLY437_REC_MOT_LD_1.2", 
 "FLY438_REC_MOT_LD_1.2", "FLY440_REC_MOT_LD_1.2", "FLY444_REC_MOT_LD_1.2", "FLY446_REC_MOT_LD_1.2", "FLY447_REC_MOT_LD_1.2", "FLY452_REC_MOT_LD_1.2", "FLY456_REC_MOT_LD_1.2", "FLY457_REC_MOT_LD_1.2", "FLY458_REC_MOT_LD_1.2", 
 "FLY459_REC_MOT_LD_1.2", "FLY461_REC_MOT_LD_1.2", "FLY462_REC_MOT_LD_1.2", "FLY463_REC_MOT_LD_1.2", "FLY464_REC_MOT_LD_1.2", "FLY466_REC_MOT_LD_1.2_clean", "FLY472_REC_MOT_LD_1.2", "FLY475_REC_MOT_LD_1.2", "FLY477_REC_MOT_LD_1.2", "FLY478_REC_MOT_LD_1.2", 
 "FLY484_REC_MOT_LD_1.2", "FLY487_REC_MOT_LD_1.2")

dataset_names <- c("FLY297_REC_MOT_LD_1.2", "FLY298_REC_MOT_LD_1.2", "FLY299_REC_MOT_LD_1.2")
mean_diff <- c(mean(FLY297_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE), mean(FLY298_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE), mean(FLY299_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE))
sd_diff <- c(sd(FLY297_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE), sd(FLY298_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE), sd(FLY299_REC_MOT_LD_1.2$relative_diff , na.rm=TRUE))
cv_tot <- c(cv_297, cv_298,cv_299)

# Creazione del dataframe
datastats <- data.frame(Dataframe = dataset_names, Mean = mean_diff, SD = sd_diff)
datastats



ggplot(datastats, aes(x = Dataframe, y = Mean)) +
  #geom_point(color = "darkorange", size = 5) +  # Punto blu per la media
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(0.5), 
                width = 0.2, 
                color = "black") +  # Barre di errore sopra e sotto la media
  ggtitle("Media e Deviazione Standard per ciascun Dataframe") +
  theme_minimal()


mean(FLY298_REC_MOT_LD_1.2$diff_col, na.rm=TRUE)



# GSD = 



