#### VOLI CON MISURAZIONI ####

#### Packages ####

library(pastecs)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hms)
library(stringr)
library(tidyverse)
library(tidyr)
library(purrr)
library(lme4)
library(sjPlot)
library(modelsummary)

### funciotions ####
azzera_dataset <- function(dataset) {
  # Trova l'indice del primo valore maggiore di 2 metri
  indice_inizio <- which(dataset$`osd_data:relativeHeight[meters]` > 2)[1]
  
  # Calcola il nuovo indice di partenza
  nuovo_indice_inizio <- indice_inizio - 3
  
  # Elimina tutti i valori precedenti al nuovo indice
  dataset <- dataset[-c(1:nuovo_indice_inizio), , drop = FALSE]
  
  return(dataset)
}
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

### FIN WHALE ####

####################################################### FLY_310 + LOG_0034 ####################################################### 
# READ ME #
# DJI_0027 starting to climb to 50m 1sec after stop recording (whale video)
# DJI_0028 starting to climb to 50m 1sec after stop recording (pole video)
# LIDAR file contains altitude data for both the flights (whale before and pole after)


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

#### _______________ error ####

# check results by plotting
ggplot(FLY310_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(207,375,461,549), linetype = "solid", color = "gray") +
  geom_vline(xintercept = c(255), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(270), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(286), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(301), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(317), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(334), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(350), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(547), linetype = "solid", color = "tan2") +
  labs(title = "2022-07-10 11:17:31 (FLY_310 LOG_0034)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY310_REC_MOT_LD_1.2), by = 20)) + 
  ylim(0,100)

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_310 <-1.71
PoleValue_lID_310 <- 1.86

BAR_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(547-2):(547+2)])
BAR_SD_pole_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(547-2):(547+2)])
LID_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(547-2):(547+2)])
LID_SD_pole_301 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(547-2):(547+2)])

BAR_corr_310 <- (BAR_pole_310 * PoleStandard)/PoleValue_BAR_310
#LID_corr_310 <-(LID_pole_310 * PoleStandard)/PoleValue_lID_310

#ΔH_BAR_310 <- BAR_corr_310 - BAR_pole_310
#ΔH_LID_310 <- LID_corr_310 - LID_pole_310
correct_h_310 <- 40.86
ΔH_BAR_310 <- correct_h_310 - BAR_pole_310
ΔH_LID_310 <- correct_h_310 - LID_pole_310

BAR_corr_310 <- BAR_pole_310 + ΔH_BAR_310
LID_corr_310 <-LID_pole_310 + ΔH_LID_310

Error_pole_310 <- LID_corr_310 - BAR_corr_310


# frame 1 whale
BAR_1w_310 <- mean(FLY310_REC_MOT_LD_,1.2$`osd_data:relativeHeight[meters]`[(255-2):(255+2)])
BAR_SD_1w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(255-2):(255+2)])
LID_1w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(255-2):(255+2)])
LID_SD_1w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(255-2):(255+2)])
Error_1w_310 <- (LID_1w_310 + ΔH_LID_310) - (BAR_1w_310 + ΔH_BAR_310)

# frame 2 whale 
BAR_2w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(270-2):(270+2)])
BAR_SD_2w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(270-2):(270+2)])
LID_2w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(270-2):(270+2)])
LID_SD_2w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(270-2):(270+2)])
Error_2w_310 <- (LID_2w_310 + ΔH_LID_310) - (BAR_2w_310 + ΔH_BAR_310)

# frame 3 whale 
BAR_3w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(286-2):(286+2)])
BAR_SD_3w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(286-2):(286+2)])
LID_3w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(286-2):(286+2)])
LID_SD_3w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(286-2):(286+2)])
Error_3w_310 <- (LID_3w_310 + ΔH_LID_310) - (BAR_3w_310 + ΔH_BAR_310)

# frame 4 whale 
BAR_4w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(301-2):(301+2)])
BAR_SD_4w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(301-2):(301+2)])
LID_4w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(301-2):(301+2)])
LID_SD_4w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(301-2):(301+2)])
Error_4w_310 <- (LID_4w_310 + ΔH_LID_310) - (BAR_4w_310 + ΔH_BAR_310)

# frame 5 whale 
BAR_5w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(317-2):(317+2)])
BAR_SD_5w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(317-2):(317+2)])
LID_5w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(317-2):(317+2)])
LID_SD_5w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(317-2):(317+2)])
Error_5w_310 <- (LID_5w_310 + ΔH_LID_310) - (BAR_5w_310 + ΔH_BAR_310)

# frame 6 whale 
BAR_6w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(334-2):(334+2)])
BAR_SD_6w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(334-2):(334+2)])
LID_6w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(334-2):(334+2)])
LID_SD_6w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(334-2):(334+2)])
Error_6w_310 <- (LID_6w_310 + ΔH_LID_310) - (BAR_6w_310 + ΔH_BAR_310)

# frame 7 whale 
BAR_7w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(350-2):(350+2)])
BAR_SD_7w_301 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(350-2):(350+2)])
LID_7w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(350):(350+2)])
LID_SD_7w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(350-2):(350+2)])
Error_7w_310 <- (LID_7w_310 + ΔH_LID_310) - (BAR_7w_310 + ΔH_BAR_310)


GSD_BAR_7w_310 <- (((BAR_7w_310 + ΔH_BAR))/24.962606)*0.004223633 %>% GSD_BAR_7w*1000
GSD_LID_7w_310 <- (LID_7w_/24.962606)*0.004223633
# GSD 
GSD_BAR_7w_310 *2000
GSD_LID_7w_310 *2000


df_310 <- data.frame(
  Flight_ID = rep("FLY310", times = 8),  
  second = c(547, 255, 270, 286, 301, 317, 334, 350),  
  Error = c(
    Error_pole_310,
    Error_1w_310,
    Error_2w_310,
    Error_3w_310,
    Error_4w_310,
    Error_5w_310,
    Error_6w_310,
    Error_7w_310))

# Plot Error
ggplot(df_310, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 310",
       x = "Second",
       y = "Error") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_310$second), by = 60))

#### _______________ rest of ####

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

####################################################### FLY_330 + LOG_0050 ####################################################### 

# READ ME #
# climb to high limit after 1sec stopping video, both for the whale video (one video splitted in two different videos) and the pole video
# lidar data are in an unique file containing both whale and pole sampling.

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

#### _______________ error ####

# check results by plotting
ggplot(FLY330_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +  geom_vline(xintercept = c(182,569,638,696), linetype = "dashed", color = "black") +
  geom_vline(xintercept = c(195), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(352), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(372), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(416), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(467), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(528), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(559), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(690), linetype = "solid", color = "tan2") +
  
  labs(title = "2022-07-21 17:18:48 (FLY_330 LOG_0050)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY330_REC_MOT_LD_1.2), by = 20)) +
  ylim(0,100)

# pole
mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])
sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_330 <-1.66
PoleValue_LID_330 <-1.85

BAR_pole_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
BAR_SD_pole_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
LID_pole_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])
LID_SD_pole_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])

#BAR_corr_330 <- (BAR_pole_330 * PoleStandard)/PoleValue_BAR_330
#LID_corr_330 <- (LID_pole_330 * PoleStandard)/PoleValue_LID_330 
#ΔH_BAR_330 <- BAR_corr_330 - BAR_pole_330
#ΔH_LID_330 <- LID_corr_330 - LID_pole_330

correct_h_330 <- 39.16
ΔH_BAR_330 <- correct_h_330 - BAR_pole_330
ΔH_LID_330 <- correct_h_330 - LID_pole_330

BAR_corr_330 <- BAR_pole_330 + ΔH_BAR_330
LID_corr_330 <-LID_pole_330 + ΔH_LID_330

Error_pole_330 <- LID_corr_330 - BAR_corr_330

# frame 1 whale
BAR_1w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(195-2):(195+2)])
BAR_SD_1w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(195-2):(195+2)])
LID_1w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(195-2):(195+2)])
LID_SD_1w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(195-2):(195+2)])
Error_1w_330 <- (LID_1w_330 + ΔH_LID_330) - (BAR_1w_330 + ΔH_BAR_330)

# frame 2 whale
BAR_2w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(352-2):(352+2)])
BAR_SD_2w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(352-2):(352+2)])
LID_2w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(352-2):(352+2)])
LID_SD_2w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(352-2):(352+2)])
Error_2w_330 <- (LID_2w_330 + ΔH_LID_330) - (BAR_2w_330 + ΔH_BAR_330)

# frame 3 whale
BAR_3w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
BAR_SD_3w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
LID_3w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)])
LID_SD_3w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)])
Error_3w_330 <- (LID_3w_330 + ΔH_LID_330) - (BAR_3w_330 + ΔH_BAR_330)

# frame 4 whale
BAR_4w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(416-2):(416+2)])
BAR_SD_4w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(416-2):(416+2)])
LID_4w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(416-2):(416+2)])
LID_SD_4w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(416-2):(416+2)])
Error_4w_330 <- (LID_4w_330 + ΔH_LID_330) - (BAR_4w_330 + ΔH_BAR_330)

# frame 5 whale
BAR_5w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(467-2):(467+2)])
BAR_SD_5w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(467-2):(467+2)])
LID_5w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(467-2):(467+2)])
LID_SD_5w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(467-2):(467+2)])
Error_5w_330 <- (LID_5w_330 + ΔH_LID_330) - (BAR_5w_330 + ΔH_BAR_330)

# frame 6 whale
BAR_6w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
BAR_SD_6w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
LID_6w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528-2):(528+2)])
LID_SD_6w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528):(528+2)])
Error_6w_330 <- (LID_6w_330 + ΔH_LID_330) - (BAR_6w_330 + ΔH_BAR_330)

# frame 7 whale
BAR_7w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(559-2):(559+2)])
BAR_SD_7w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(559-2):(559+2)])
LID_7w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(559-2):(559+2)])
LID_SD_7w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(559):(559+2)])
Error_7w_330 <- (LID_7w_330 + ΔH_LID_330) - (BAR_7w_330 + ΔH_BAR_330)

#### _______________ df_330 ####
df_330 <- data.frame(
  Flight_ID = rep("FLY330", times = 8),  
  second = c(690, 195, 352, 372, 416, 467, 528, 559),  
  Error = c(
    Error_pole_330,
    Error_1w_330,
    Error_2w_330,
    Error_3w_330,
    Error_4w_330,
    Error_5w_330,
    Error_6w_330,
    Error_7w_330
  )
)

# Plot Error
ggplot(df_330, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 330",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_330$second), by = 60))

#### _______________ rest of 330 ####
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
  geom_vline(xintercept = c(227), linetype = "solid", color = "red") +
  geom_vline(xintercept = c(414), linetype = "solid", color = "gray") +
  geom_vline(xintercept = c(412), linetype = "solid", color = "gray") +
  geom_vline(xintercept = c(559), linetype = "solid", color = "red") +
  geom_vline(xintercept = c(664), linetype = "solid", color = "gray") +
  geom_vline(xintercept = c(688), linetype = "solid", color = "gray") +
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

#### _______________ error ####

# check results by plotting
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
 # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(34,361,518,604), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(79), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(152), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(188), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(234), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(267), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(595), linetype = "solid", color = "tan2") +
  labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)
  
# pole
mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])
sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_365 <-1.70
PoleValue_LID_365 <-1.86

BAR_pole_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
BAR_SD_pole_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
LID_pole_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])
LID_SD_pole_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])

#BAR_corr_365 <- (BAR_pole_365 * PoleStandard)/PoleValue_BAR_365
#LID_corr_365 <- (LID_pole_365 * PoleStandard)/PoleValue_LID_365
#ΔH_BAR_365 <- BAR_corr_365 - BAR_pole_365
#ΔH_LID_365  <- LID_corr_365 - LID_pole_365

correct_h_365 <- 39.104

ΔH_BAR_365 <- correct_h_365 - BAR_pole_365
ΔH_LID_365  <- correct_h_365 - LID_pole_365

BAR_corr_365 <- BAR_pole_365 + ΔH_BAR_365
LID_corr_365 <- LID_pole_365 + ΔH_LID_365

Error_pole_365 <- LID_corr_365 - BAR_corr_365

# frame 1 whale
BAR_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
BAR_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
LID_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
LID_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
Error_1w_365 <- (LID_1w_365 + ΔH_LID_365) - (BAR_1w_365 + ΔH_BAR_365) 

# frame 2 whale
BAR_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
BAR_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
LID_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
LID_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
Error_2w_365 <- (LID_2w_365 + ΔH_LID_365) - (BAR_2w_365 + ΔH_BAR_365)

# frame 3 whale
BAR_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
BAR_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
LID_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
LID_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
Error_3w_365 <- (LID_3w_365 + ΔH_LID_365) - (BAR_3w_365 + ΔH_BAR_365)

# frame 4 whale
BAR_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
BAR_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
LID_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
LID_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
Error_4w_365 <- (LID_4w_365 + ΔH_LID_365) - (BAR_4w_365 + ΔH_BAR_365)

# frame 5 whale
BAR_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
BAR_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
LID_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
LID_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
Error_5w_365 <- (LID_5w_365 + ΔH_LID_365) - (BAR_5w_365 + ΔH_BAR_365)


#### _______________ df 365 ####

df_365 <- data.frame(
  Flight_ID = rep("FLY365", times = 6),  
  second = c(595, 79, 152, 188, 234, 267),  
  Error = c(
    Error_pole_365,
    Error_1w_365,
    Error_2w_365,
    Error_3w_365,
    Error_4w_365,
    Error_5w_365
  )
)

# Plot Error
ggplot(df_365, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 365",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_365$second), by = 60))

#### _______________ rest of ####


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
# READ ME # 
# whale video -> stop recording and increase altitude to 50-55m
# pole video -> video with the pole in different part of the frame; stop recording and up to 50-55m

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

#### _______________ error ####

# check results by plotting
ggplot(FLY368_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
#  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(158,260,340,554), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(168), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(185), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(204), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(222), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(235), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(247), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(545), linetype = "solid", color = "tan2") +
  
  labs(title = "2022-08-12 15:11:23 (FLY_368 LOG_0074)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY368_REC_MOT_LD_1.2_clean), by = 20)) +
  ylim(0,100)


# pole
mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(545-2):(545+2)])
sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(545-2):(545+2)])
mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(545-2):(545+2)])
sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(545-2):(545+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_368 <-1.64
PoleValue_LID_368 <-1.86

BAR_pole_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(545-2):(545+2)])
BAR_SD_pole_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(545-2):(545+2)])
LID_pole_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(545-2):(545+2)])
LID_SD_pole_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(545-2):(545+2)])

# BAR_corr_368 <- (BAR_pole_368 * PoleStandard)/PoleValue_BAR_368
#LID_corr_368 <- (LID_pole_368 * PoleStandard)/PoleValue_LID_368
#ΔH_BAR_368 <- BAR_corr_368 - BAR_pole_368
#ΔH_LID_368 <- LID_corr_368 - LID_pole_368

correct_h_368 <- 42.82

ΔH_BAR_368 <- correct_h_368 - BAR_pole_368
ΔH_LID_368 <- correct_h_368 - LID_pole_368
BAR_corr_368 <- BAR_pole_368 + ΔH_BAR_368
LID_corr_368 <- LID_pole_368 + ΔH_LID_368
  
Error_pole_368 <- LID_corr_368 - BAR_corr_368

# frame 1 whale
BAR_1w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(168-2):(168+2)])
BAR_SD_1w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(168-2):(168+2)])
LID_1w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(168-2):(168+2)])
LID_SD_1w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(168-2):(168+2)])
Error_1w_368 <- (LID_1w_368 + ΔH_LID_368) - (BAR_1w_368 + ΔH_BAR_368) 

# frame 2 whale
BAR_2w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
BAR_SD_2w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
LID_2w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(185-2):(185+2)])
LID_SD_2w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(185-2):(185+2)])
Error_2w_368 <- (LID_2w_368 + ΔH_LID_368) - (BAR_2w_368 + ΔH_BAR_368)

# frame 3 whale
BAR_3w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(204-2):(204+2)])
BAR_SD_3w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(204-2):(204+2)])
LID_3w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(204-2):(204+2)])
LID_SD_3w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(204-2):(204+2)])
Error_3w_368 <- (LID_3w_368 + ΔH_LID_368) - (BAR_3w_368 + ΔH_BAR_368)

# frame 4 whale
BAR_4w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(222-2):(222+2)])
BAR_SD_4w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(222-2):(222+2)])
LID_4w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(222-2):(222+2)])
LID_SD_4w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(222-2):(222+2)])
Error_4w_368 <- (LID_4w_368 + ΔH_LID_368) - (BAR_4w_368 + ΔH_BAR_368)

# frame 5 whale
BAR_5w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(235-2):(235+2)])
BAR_SD_5w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(235-2):(235+2)])
LID_5w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(235-2):(235+2)])
LID_SD_5w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(235-2):(235+2)])
Error_5w_368 <- (LID_5w_368 + ΔH_LID_368) - (BAR_5w_368 + ΔH_BAR_368)

# frame 6 whale
BAR_6w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(247-2):(247+2)])
BAR_SD_6w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(247-2):(247+2)])
LID_6w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(247-2):(247+2)])
LID_SD_6w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(247-2):(247+2)])
Error_6w_368 <- (LID_6w_368 + ΔH_LID_368) - (BAR_6w_368 + ΔH_BAR_368)

#### _______________ df 368 ####

df_368 <- data.frame(
  Flight_ID = rep("FLY368", times = 7),  
  second = c(545, 168, 185, 204, 222, 235, 247),  
  Error = c(
    Error_pole_368,
    Error_1w_368,
    Error_2w_368,
    Error_3w_368,
    Error_4w_368,
    Error_5w_368,
    Error_6w_368
  )
)

# Plot Error
ggplot(df_368, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 368",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_368$second), by = 60))


#### _______________ rest of ####

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



#### (LID MALF) FLY_401 + LOG_0094 ####
# READ ME # 
# video and pole in the same flight. After the pole, video stop and altitute down

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

#### _______________ error ####

# check results by plotting
ggplot(FLY401_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(26,161), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(46), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(53), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(69), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(156), linetype = "solid", color = "tan2") +
  labs(title = "2023-02-22 09:57:52 (FLY_401 LOG_0094)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

# pole
mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])
sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_401 <-1.70
PoleValue_LID_401 <-1.85

BAR_pole_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
BAR_SD_pole_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
LID_pole_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])
LID_SD_pole_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])

(BAR_pole_401 * PoleStandard)/PoleValue_BAR_401
(LID_pole_401 * PoleStandard)/PoleValue_LID_401

correct_h_410 <- 48.65
ΔH_BAR_401 <- correct_h_410 - BAR_pole_401
ΔH_LID_401 <- correct_h_410 - LID_pole_401
BAR_corr_401 <- BAR_pole_401 + ΔH_BAR_401
LID_corr_401 <- LID_pole_401 + ΔH_LID_401

Error_pole_401 <- LID_corr_401 - BAR_corr_401

# frame 1 whale
BAR_1w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(46-2):(46+2)])
BAR_SD_1w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(46-2):(46+2)])
LID_1w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(46-2):(46+2)], na.rm = TRUE)
LID_SD_1w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(46-2):(46+2)], na.rm = TRUE)
Error_1w_401 <- (LID_1w_401 + ΔH_LID_401) - (BAR_1w_401 + ΔH_BAR_401) 

# frame 2 whale
BAR_2w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(53-2):(53+2)])
BAR_SD_2w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(53-2):(53+2)])
LID_2w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(53-2):(53+2)], na.rm = TRUE)
LID_SD_2w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(53-2):(53+2)], na.rm = TRUE)
Error_2w_401 <- (LID_2w_401 + ΔH_LID_401) - (BAR_2w_401 + ΔH_BAR_401)

# frame 3 whale
BAR_3w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
BAR_SD_3w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
LID_3w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(69-2):(69+2)], na.rm = TRUE)
LID_SD_3w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(69-2):(69+2)], na.rm = TRUE)
Error_3w_401 <- (LID_3w_401 + ΔH_LID_401) - (BAR_3w_401 + ΔH_BAR_401)

#### _______________ df 401 ####

df_401 <- data.frame(
  Flight_ID = rep("FLY401", times = 4),  
  second = c(156, 46, 53, 69),  
  Error = c(
    Error_pole_401,
    Error_1w_401,
    Error_2w_401,
    Error_3w_401)
)

# Plot Error
ggplot(df_401, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 401",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_401$second), by = 60))

#### _______________ rest of ####

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

#### FLY_412 + LOG100 ####
# READ ME #
# Whale and pole in the same video. beginning of pole >50 alt. and then deacresed to around 35m alt.

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


#### _______________ error ####
# check results by plotting
ggplot(FLY412_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(17,237), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(58), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(80), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(236), linetype = "solid", color = "tan2") +
  labs(title = "2023-05-06 09:29:32 (FLY_412 LOG_0100)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)



# pole
mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(236-2):(236+2)])
sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(236-2):(236+2)])
mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(236-2):(236+2)])
sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(236-2):(236+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_412 <-1.69
PoleValue_LID_412 <-1.88

BAR_pole_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(236-2):(236+2)])
BAR_SD_pole_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(236-2):(236+2)])
LID_pole_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(236-2):(236+2)])
LID_SD_pole_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(236-2):(236+2)])

(BAR_pole_412 * PoleStandard)/PoleValue_BAR_412
(LID_pole_412 * PoleStandard)/PoleValue_LID_412

correct_h_412 <- 37.48
ΔH_BAR_412 <- correct_h_412 - BAR_pole_412
ΔH_LID_412 <- correct_h_412 - LID_pole_412

BAR_corr_412 <- BAR_pole_412 + ΔH_BAR_412
LID_corr_412 <- LID_pole_412 + ΔH_LID_412

Error_pole_412 <- LID_corr_412 - BAR_corr_412

# frame 1 whale
BAR_1w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(58-2):(58+2)])
BAR_SD_1w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(58-2):(58+2)])
LID_1w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(58-2):(58+2)])
LID_SD_1w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(58-2):(58+2)])
Error_1w_412 <- (LID_1w_412 + ΔH_LID_412) - (BAR_1w_412 + ΔH_BAR_412) 

# frame 2 whale
BAR_2w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(80-2):(80+2)])
BAR_SD_2w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(80-2):(80+2)])
LID_2w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(80-2):(80+2)])
LID_SD_2w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(80-2):(80+2)])
Error_2w_412 <- (LID_2w_412 + ΔH_LID_412) - (BAR_2w_412 + ΔH_BAR_412)


#### _______________ df 412 ####

df_412 <- data.frame(
  Flight_ID = rep("FLY412", times = 3),  
  second = c(236, 58, 80),  
  Error = c(
    Error_pole_412,
    Error_1w_412,
    Error_2w_412)
)

# Plot Error
ggplot(df_412, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 412",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_412$second), by = 60))

#### _______________ rest of ####

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


#### FLY_417 + LOG106 #### 
# SERVE PRUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Video unico che è stato spittato in 3 video. Ho stoppato il video solo quando è atterrato.
# La prima balenottera ripresa inizialmente a 30 metri e poi sono passato a 40m -> sono andato dal palo mantenendo  i 40m -> 
# sono passato sulla prua mantenendo i 40m e poi sono sceso a 30m e poi sono sceso per atterrare ma ho 
# interrotto l'atterraggio e sono ripartito su la seconda balena ripresa a 40m e poi sono sceso per tentare foto-id 
# ma è andata via e quindi sono ritornato ad atterrare (mi sono portato prima a 60m e poi sono andato verso la barca) 
# Ho chiuso il video solo dopo che sono atterrato.

# Dati lidar: LOG_0106.CSV

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

#### _______________ error ####

# check results by plotting
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(16,701), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(35), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(47), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(59), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(75), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(88), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(528), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(552), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(183), linetype = "solid", color = "tan2") +
  #geom_vline(xintercept = c(213), linetype = "solid", color = "tan2") + # bow
  #geom_vline(xintercept = c(253), linetype = "solid", color = "tan2") + # bow
  labs(title = "2023-05-24 10:29:31 (FLY_417 LOG_0106)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

# pole
mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(183-2):(183+2)])
sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(183-2):(183+2)])
mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(185-2):(185+2)], na.rm = TRUE)
sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(183-2):(183+2)], na.rm = TRUE)

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_417 <-1.65
PoleValue_LID_417 <-1.87

BAR_pole_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(183-2):(183+2)])
BAR_SD_pole_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(183-2):(183+2)])
LID_pole_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(183-2):(183+2)], na.rm = TRUE)
LID_SD_pole_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(518395-2):(183+2)], na.rm = TRUE)

(BAR_pole_417 * PoleStandard)/PoleValue_BAR_417
(LID_pole_417 * PoleStandard)/PoleValue_LID_417

correct_h_417 <- 45.22
ΔH_BAR_417 <- correct_h_417 - BAR_pole_417
ΔH_LID_417 <- correct_h_417 - LID_pole_417
BAR_corr_417 <- BAR_pole_417 + ΔH_BAR_417
LID_corr_417 <-LID_pole_417 + ΔH_LID_417
Error_pole_417 <- LID_corr_417 - BAR_corr_417

# frame 1 whale
BAR_1w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(35-2):(35+2)])
BAR_SD_1w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(35-2):(35+2)])
LID_1w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(35-2):(35+2)], na.rm = TRUE)
LID_SD_1w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(35-2):(35+2)], na.rm = TRUE)
Error_1w_417 <- (LID_1w_417 + ΔH_LID_417) - (BAR_1w_417 + ΔH_BAR_417) 

# frame 2 whale
BAR_2w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(47-2):(47+2)])
BAR_SD_2w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(47-2):(47+2)])
LID_2w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(47-2):(47+2)], na.rm = TRUE)
LID_SD_2w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(47-2):(47+2)], na.rm = TRUE)
Error_2w_417 <- (LID_2w_417 + ΔH_LID_417) - (BAR_2w_417 + ΔH_BAR_417) 

# frame 3 whale
BAR_3w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(59-2):(59+2)])
BAR_SD_3w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(59-2):(59+2)])
LID_3w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(59-2):(59+2)], na.rm = TRUE)
LID_SD_3w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(59-2):(59+2)], na.rm = TRUE)
Error_3w_417 <- (LID_3w_417 + ΔH_LID_417) - (BAR_3w_417 + ΔH_BAR_417) 

# frame 4 whale
BAR_4w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(75-2):(75+2)])
BAR_SD_4w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(75-2):(75+2)])
LID_4w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(75-2):(75+2)], na.rm = TRUE)
LID_SD_4w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(75-2):(75+2)], na.rm = TRUE)
Error_4w_417 <- (LID_4w_417 + ΔH_LID_417) - (BAR_4w_417 + ΔH_BAR_417) 

# frame 5 whale
BAR_5w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(88-2):(88+2)])
BAR_SD_5w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(88-2):(88+2)])
LID_5w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(88-2):(88+2)], na.rm = TRUE)
LID_SD_5w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(88-2):(88+2)], na.rm = TRUE)
Error_5w_417 <- (LID_5w_417 + ΔH_LID_417) - (BAR_5w_417 + ΔH_BAR_417) 

# frame 6 whale
BAR_6w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
BAR_SD_6w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
LID_6w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528-2):(528+2)], na.rm = TRUE)
LID_SD_6w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528-2):(528+2)], na.rm = TRUE)
Error_6w_417 <- (LID_6w_417 + ΔH_LID_417) - (BAR_6w_417 + ΔH_BAR_417) 

# frame 7 whale
BAR_7w_417 <- mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(552-2):(552+2)])
BAR_SD_7w_417 <- sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(552-2):(552+2)])
LID_7w_417 <- mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(552-2):(552+2)], na.rm = TRUE)
LID_SD_7w_417 <- sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(552-2):(552+2)], na.rm = TRUE)
Error_7w_417 <- (LID_7w_417 + ΔH_LID_417) - (BAR_7w_417 + ΔH_BAR_417) 

#### _______________ df 417 ####

df_417 <- data.frame(
  Flight_ID = rep("FLY417", times = 8),  
  second = c(183, 35, 47, 59, 75, 88, 528, 552),  
  Error = c(
    Error_pole_417,
    Error_1w_417,
    Error_2w_417,
    Error_3w_417,
    Error_4w_417,
    Error_5w_417,
    Error_6w_417,
    Error_7w_417
  ))

# Plot Error
ggplot(df_417, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 417",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_417$second), by = 60))

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
                                              lower_limit = 10, upper_limit = 80, omit_first_n = 5, omit_last_n = 30)

# check results by plotting
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
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

#### _______________ error ####
# check results by plotting
ggplot(FLY420_REC_MOT_LD_1.2_clean, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(-3,739), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(12), linetype = "dotted", color = "skyblue3") +
  geom_vline(xintercept = c(43), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(69), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(89), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(111), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(135), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(625), linetype = "solid", color = "tan2") +
  labs(title = "2023-06-17 09:19:13 (FLY_420 LOG_0108)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY420_REC_MOT_LD_1.2_clean), by = 10))+
  ylim(0,100)

# pole
mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])
sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_420 <-1.65
PoleValue_LID_420 <-1.85

BAR_pole_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
BAR_SD_pole_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
LID_pole_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])
LID_SD_pole_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])

(BAR_pole_420 * PoleStandard)/PoleValue_BAR_420
(LID_pole_420 * PoleStandard)/PoleValue_LID_420

correct_h_420 <- 33.72
ΔH_BAR_420 <- correct_h_420 - BAR_pole_420
ΔH_LID_420 <- correct_h_420 - LID_pole_420

BAR_corr_420 <- BAR_pole_420 + ΔH_BAR_420
LID_corr_420 <-LID_pole_420 + ΔH_LID_420


Error_pole_420 <- LID_corr_420 - BAR_corr_420

# frame 1 whale
BAR_1w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(43-2):(43+2)])
BAR_SD_1w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(43-2):(43+2)])
LID_1w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(43-2):(43+2)])
LID_SD_1w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(43-2):(43+2)])
Error_1w_420 <- (LID_1w_420 + ΔH_LID_420) - (BAR_1w_420 + ΔH_BAR_420) 

# frame 2 whale
BAR_2w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
BAR_SD_2w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
LID_2w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(69-2):(69+2)])
LID_SD_2w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(69-2):(69+2)])
Error_2w_420 <- (LID_2w_420 + ΔH_LID_420) - (BAR_2w_420 + ΔH_BAR_420)

# frame 3 whale
BAR_3w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(89-2):(89+2)])
BAR_SD_3w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(89-2):(89+2)])
LID_3w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(89-2):(89+2)])
LID_SD_3w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(89-2):(89+2)])
Error_3w_420 <- (LID_3w_420 + ΔH_LID_420) - (BAR_3w_420 + ΔH_BAR_420)

# frame 4 whale
BAR_4w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(111-2):(111+2)])
BAR_SD_4w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(111-2):(111+2)])
LID_4w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(111-2):(111+2)])
LID_SD_4w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(111-2):(111+2)])
Error_4w_420 <- (LID_4w_420 + ΔH_LID_420) - (BAR_4w_420 + ΔH_BAR_420)

# frame 5 whale
BAR_5w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(135-2):(135+2)])
BAR_SD_5w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(135-2):(135+2)])
LID_5w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(135-2):(135+2)])
LID_SD_5w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(135-2):(135+2)])
Error_5w_420 <- (LID_5w_420 + ΔH_LID_420) - (BAR_5w_420 + ΔH_BAR_420)

#### _______________ df 420 ####

df_420 <- data.frame(
  Flight_ID = rep("FLY420", times = 6),  
  second = c(625, 43, 69, 89, 111, 135),  
  Error = c(
    Error_pole_420,
    Error_1w_420,
    Error_2w_420,
    Error_3w_420,
    Error_4w_420,
    Error_5w_420
  )
)

# Plot Error
ggplot(df_420, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 420",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_420$second), by = 60))


#### _______________ rest of  ####

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


#### (NO LID) FLY_421 + LOG109 #### 
#manca lidar tho
#Start rec before motore start and Stop rec after motor off.
# Palo e prue a 40m
#Niente biopsia (mentre BP01 e BP02 sono state entrambe biopsate)

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

# _______________ error 

# check results by plotting
ggplot(FLY421_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(-12,533), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(78), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(95), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(113), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(485), linetype = "solid", color = "tan2") +
  labs(title = "2023-06-17 11:03:01 (FLY_421 LOG_109)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY421_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[485]

# pole
mean(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(485-2):(485+2)])
sd(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(485-2):(485+2)])
mean(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(485-2):(485+2)])
sd(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(485-2):(485+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_421 <-1.67
PoleValue_LID_365 <-

BAR_pole_421 <- mean(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(485-2):(485+2)])
BAR_SD_pole_421<- sd(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(485-2):(485+2)])
#LID_pole_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])
#LID_SD_pole_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])

BAR_corr_421 <- (BAR_pole_421 * PoleStandard)/PoleValue_BAR_421
#LID_corr_365 <- (LID_pole_365 * PoleStandard)/PoleValue_LID_365

LID_corr_421 - BAR_corr_421
ΔH_BAR_421 <- BAR_corr_421 - BAR_pole_421

Error_pole_421 <- BAR_corr_421 - (BAR_pole_421 + ΔH_BAR_421)

# frame 1 whale
BAR_1w_421 <- mean(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(78-2):(78+2)])
BAR_SD_1w_421 <- sd(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(78-2):(78+2)])
LID_1w_421 <- mean(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(78-2):(78+2)])
LID_SD_1w_421 <- sd(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(78-2):(78+2)])
Error_1w_421 <- LID_1w_421 - (BAR_1w_421 + ΔH_BAR_421) 

# frame 2 whale
BAR_2w_421 <- mean(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(95-2):(95+2)])
BAR_SD_2w_421 <- sd(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(95-2):(95+2)])
LID_2w_421 <- mean(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(95-2):(95+2)])
LID_SD_2w_421 <- sd(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(95-2):(95+2)])
Error_2w_421 <- LID_2w_421 - (BAR_2w_421 + ΔH_BAR_421)

# frame 3 whale
BAR_3w_421 <- mean(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(113-2):(113+2)])
BAR_SD_3w_421 <- sd(FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(113-2):(113+2)])
LID_3w_421 <- mean(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(113-2):(113+2)])
LID_SD_3w_421 <- sd(FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(113-2):(113+2)])
Error_3w_421 <- LID_3w_421 - (BAR_3w_421 + ΔH_BAR_421)

# _______________ df 421

df_421<- data.frame(
  Flight_ID = rep("FLY421", times = 4),  
  second = c(485, 78, 95, 113),  
  Error = c(
    BAR_corr_421 - (BAR_pole_421 + ΔH_BAR_421),
    LID_1w_421 - (BAR_1w_421 + ΔH_BAR_421),
    LID_2w_421 - (BAR_2w_421 + ΔH_BAR_421),
    LID_3w_421 - (BAR_3w_421 + ΔH_BAR_421))
)

# Plot Error
ggplot(df_421, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 421",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 

# _______________ rest of 

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





#### FLY_425 + LOG115 #### 
# più video di log - capire bene 
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
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 10)

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

#### _______________ error ####

# check results by plotting
ggplot(FLY425_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(-2, 0, 457), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(64), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(86), linetype = "solid", color = "skyblue3") +
 # geom_vline(xintercept = c(207), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(244), linetype = "solid", color = "tan2") +
    labs(title = "2023-06-18 09:15:01 (FLY_425 LOG_115)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY425_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

# pole
mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])
sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_425 <-1.64
PoleValue_LID_425 <-1.85

BAR_pole_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
BAR_SD_pole_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
LID_pole_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])
LID_SD_pole_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])

(BAR_pole_425 * PoleStandard)/PoleValue_BAR_425
#(LID_pole_425 * PoleStandard)/PoleValue_LID_425

correct_h_425 <- 44.36
ΔH_BAR_425 <- correct_h_425 - BAR_pole_425
ΔH_LID_425 <- correct_h_425 - LID_pole_425

BAR_corr_425 <- BAR_pole_425 + ΔH_BAR_425
LID_corr_425 <-LID_pole_425 + ΔH_LID_425


Error_pole_425 <- LID_corr_425 - BAR_corr_425

# frame 1 whale
BAR_1w_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(64-2):(64+2)])
BAR_SD_1w_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(64-2):(64+2)])
LID_1w_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(64-2):(64+2)], na.rm = TRUE)
LID_SD_1w_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(64-2):(64+2)], na.rm = TRUE)
Error_1w_425 <- (LID_1w_425 + ΔH_LID_425) - (BAR_1w_425 + ΔH_BAR_425) 

# frame 2 whale
BAR_2w_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(86-2):(86+2)])
BAR_SD_2w_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(86-2):(86+2)])
LID_2w_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(86-2):(86+2)])
LID_SD_2w_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(86-2):(86+2)])
Error_2w_425 <- (LID_2w_425 + ΔH_LID_425) - (BAR_2w_425 + ΔH_BAR_425)

#### _______________ df 425 ####

df_425 <- data.frame(
  Flight_ID = rep("FLY425", times = 3),  
  second = c(244, 64, 86),  
  Error = c(
    Error_pole_425,
    Error_1w_425,
    Error_2w_425
  ))

# Plot Error
ggplot(df_425, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 424",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_425$second), by = 60))

#### _______________ rest of ####

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

#### (NO LID) FLY_431 + LOG127 ####
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
                                        lower_limit = 5, upper_limit = 80, omit_first_n = 0, omit_last_n = 201)

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
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY431_REC_MOT_LD_1.2), by = 20))

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

# _______________ error 


# check results by plotting
ggplot(FLY431_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(54, 448), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(64), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(86), linetype = "solid", color = "skyblue3") +
  # geom_vline(xintercept = c(207), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(234), linetype = "solid", color = "tan2") +
  labs(title = "2023-06-18 15:01:10 (FLY_431 LOG_127)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY431_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

# pole
mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)], na.rm = TRUE)
sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)], na.rm = TRUE)

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_425 <-1.64
PoleValue_LID_425 <-1.85

BAR_pole_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
BAR_SD_pole_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(244-2):(244+2)])
LID_pole_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])
LID_SD_pole_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(244-2):(244+2)])

BAR_corr_425 <- (BAR_pole_425 * PoleStandard)/PoleValue_BAR_425
LID_corr_425 <- (LID_pole_425 * PoleStandard)/PoleValue_LID_425

ΔH_BAR_425 <- BAR_corr_425 - BAR_pole_425
ΔH_LID_425 <- LID_corr_425 - LID_pole_425

Error_pole_425 <- LID_corr_425 - BAR_corr_425

# frame 1 whale
BAR_1w_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(64-2):(64+2)])
BAR_SD_1w_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(64-2):(64+2)])
LID_1w_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(64-2):(64+2)], na.rm = TRUE)
LID_SD_1w_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(64-2):(64+2)], na.rm = TRUE)
Error_1w_425 <- (LID_1w_425 + ΔH_LID_425) - (BAR_1w_425 + ΔH_BAR_425) 

# frame 2 whale
BAR_2w_425 <- mean(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(86-2):(86+2)])
BAR_SD_2w_425 <- sd(FLY425_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(86-2):(86+2)])
LID_2w_425 <- mean(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(86-2):(86+2)])
LID_SD_2w_425 <- sd(FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(86-2):(86+2)])
Error_2w_425 <- (LID_2w_425 + ΔH_LID_425) - (BAR_2w_425 + ΔH_BAR_425)

# _______________ df 425 

df_425 <- data.frame(
  Flight_ID = rep("FLY425", times = 3),  
  second = c(244, 64, 86),  
  Error = c(
    Error_pole_425,
    Error_1w_425,
    Error_2w_425
  ))

# Plot Error
ggplot(df_425, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 424",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_425$second), by = 60))


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


#### (capire quale vid) FLY_434 + LOG129 ####
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

#### _______________ error ####

# check results by plotting
ggplot(FLY434_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(-12,533), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(78), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(95), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(113), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(485), linetype = "solid", color = "tan2") +
  labs(title = "2023-06-27 06:49:40 (FLY_434 LOG_129)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY434_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)


#### _______________ df 434 ####

#### _______________ rest of ####

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

#### (SOLO PRUE) FLY_436 + LOG131 ####
# balenottera ha reagito con drone a 30m, misura solo prue...ogni tanto in ATTI (quando il drone va molto in diagonale)
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
                                        lower_limit = 10, upper_limit = 80, omit_first_n = 0, omit_last_n = 10)

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

#### _______________ error ####

# check results by plotting
ggplot(FLY436_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(20,20), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(20), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(20), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(20), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(20), linetype = "solid", color = "tan2") +
  labs(title = "2023-07-11 13:05:32 (FLY_436 LOG_131)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY436_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)


#### _______________ df 436 ####

#### _______________ rest of ####

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
# offset BAR to match with Lidar, max 3 rows meaning 
FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0,0,0, head(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -4))


# Pulizia spikes 
FLY437_REC_MOT_LD_1.2 <- spike_cleaning(FLY437_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 80, omit_first_n = 0, omit_last_n = 0)

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


#### _______________ error ####

# check results by plotting
ggplot(FLY437_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(1,360), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(37), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(57), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(87), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(189), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(238), linetype = "solid", color = "tan2") +
  labs(title = "2023-07-11 13:27:01 (FLY_437 LOG_132)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY437_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)

# pole 2
mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(238-2):(238+2)])
sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(238-2):(238+2)])
mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(238-2):(238+2)])
sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(238-2):(238+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_437 <-1.58
PoleValue_LID_437 <-1.83

BAR_pole2_437 <- mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(238-2):(238+2)])
BAR_SD_pole2_437 <- sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(238-2):(238+2)])
LID_pole2_437 <- mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(238-2):(238+2)])
LID_SD_pole2_437 <- sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(238-2):(238+2)])

(BAR_pole2_437 * PoleStandard)/PoleValue_BAR_437
#(LID_pole2_437 * PoleStandard)/PoleValue_LID_437

correct_h_437 <- 35.6
ΔH_BAR_437 <- correct_h_437 - BAR_pole2_437
ΔH_LID_437 <- correct_h_437 - LID_pole2_437

BAR_corr_437 <- BAR_pole2_437 + ΔH_BAR_437
LID_corr_437 <-LID_pole2_437 + ΔH_LID_437


Error_pole_437 <- LID_corr_437 - BAR_corr_437

# frame 1 whale
BAR_1w_437 <- mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(37-2):(37+2)])
BAR_SD_1w_437 <- sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(37-2):(37+2)])
LID_1w_437 <- mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(37-2):(37+2)])
LID_SD_1w_437 <- sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(37-2):(37+2)])
Error_1w_437 <- (LID_1w_437 + ΔH_LID_437) - (BAR_1w_437 + ΔH_BAR_437) 

# frame 2 whale
BAR_2w_437 <- mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(57-2):(57+2)])
BAR_SD_2w_437 <- sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(57-2):(57+2)])
LID_2w_437 <- mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(57-2):(57+2)])
LID_SD_2w_437 <- sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(57-2):(57+2)])
Error_2w_437 <- (LID_2w_437 + ΔH_LID_437) - (BAR_2w_437 + ΔH_BAR_437)

# frame 3 whale
BAR_3w_437 <- mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(87-2):(87+2)])
BAR_SD_3w_437 <- sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(87-2):(87+2)])
LID_3w_437 <- mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(87-2):(87+2)])
LID_SD_3w_437 <- sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(87-2):(87+2)])
Error_3w_437 <- (LID_3w_437 + ΔH_LID_437) - (BAR_3w_437 + ΔH_BAR_437)

# pole 1 
BAR_pole1_437 <- mean(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(189-2):(189+2)])
BAR_SD_pole1_437 <- sd(FLY437_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(189-2):(189+2)])
LID_pole1_437 <- mean(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(189-2):(189+2)])
LID_SD_pole1_437 <- sd(FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(189-2):(189+2)])
Error_pole1_437 <- (LID_pole1_437 + ΔH_LID_437) - (BAR_pole1_437 + ΔH_BAR_437)


#### _______________ df 437 ####
df_437 <- data.frame(
  Flight_ID = rep("FLY437", times = 5),  
  second = c(238, 37 , 57, 87, 189),  
  Error = c(
    Error_pole_437,
    Error_1w_437,
    Error_2w_437,
    Error_3w_437, 
    Error_pole1_437)
)

# Plot Error
ggplot(df_437, aes(x = second, y = Error)) +
  geom_line() + geom_point() + 
  labs(title = "Plot di Error vs Second Flight 437",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_437$second), by = 60))


#### _______________ rest of ####

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
                                        lower_limit = 20, upper_limit = 80, omit_first_n = 20, omit_last_n = 20)

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

#### _______________ error ####

# check results by plotting
ggplot(FLY440_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(-12, 0, 729, 734), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(127), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(185), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(211), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(240), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(275), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(316), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(345), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(406), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(480), linetype = "solid", color = "skyblue3") +
  #geom_vline(xintercept = c(504), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(608), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(622), linetype = "solid", color = "tan2") +
  labs(title = "2023-07-17 07:19:17 (FLY_440 LOG_134)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY440_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)


# pole
mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(622-2):(622+2)])
sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(622-2):(622+2)])
mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(622-2):(622+2)])
sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(622-2):(622+2)])

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_440 <-1.64
PoleValue_LID_440 <-1.85

BAR_pole_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(622-2):(622+2)])
BAR_SD_pole_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(622-2):(622+2)])
LID_pole_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(622-2):(622+2)])
LID_SD_pole_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(622-2):(622+2)])

(BAR_pole_440 * PoleStandard)/PoleValue_BAR_440
(LID_pole_440 * PoleStandard)/PoleValue_LID_440

correct_h_440 <- 45.87
ΔH_BAR_440 <- correct_h_440 - BAR_pole_440
ΔH_LID_440 <- correct_h_440 - LID_pole_440

BAR_corr_440 <- BAR_pole_440 + ΔH_BAR_440
LID_corr_440 <-LID_pole_440 + ΔH_LID_440

Error_pole_440 <- LID_corr_440 - BAR_corr_440

# frame 1 whale
BAR_1w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(127-2):(127+2)])
BAR_SD_1w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(127-2):(127+2)])
LID_1w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(127-2):(127+2)], na.rm = TRUE)
LID_SD_1w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(127-2):(127+2)], na.rm = TRUE)
Error_1w_440 <- (LID_1w_440 + ΔH_LID_440) - (BAR_1w_440 + ΔH_BAR_440) 

# frame 2 whale
BAR_2w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
BAR_SD_2w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
LID_2w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(185-2):(185+2)], na.rm = TRUE)
LID_SD_2w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(185-2):(185+2)], na.rm = TRUE)
Error_2w_440 <- (LID_2w_440 + ΔH_LID_440) - (BAR_2w_440 + ΔH_BAR_440)

# frame 3 whale
BAR_3w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(211-2):(211+2)])
BAR_SD_3w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(211-2):(211+2)])
LID_3w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(211-2):(211+2)], na.rm = TRUE)
LID_SD_3w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(211-2):(211+2)], na.rm = TRUE)
Error_3w_440 <- (LID_3w_440 + ΔH_LID_440) - (BAR_3w_440 + ΔH_BAR_440)

# frame 4 whale
BAR_4w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(240-2):(240+2)])
BAR_SD_4w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(240-2):(240+2)])
LID_4w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(240-2):(240+2)], na.rm = TRUE)
LID_SD_4w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(240-2):(240+2)], na.rm = TRUE)
 Error_4w_440 <- (LID_4w_440 + ΔH_LID_440) - (BAR_4w_440 + ΔH_BAR_440)

# frame 5 whale
BAR_5w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(275-2):(275+2)])
BAR_SD_5w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(275-2):(275+2)])
LID_5w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(275-2):(275+2)], na.rm = TRUE)
LID_SD_5w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(275-2):(275+2)], na.rm = TRUE)
Error_5w_440 <- (LID_5w_440 + ΔH_LID_440) - (BAR_5w_440 + ΔH_BAR_440)

# frame 6 whale
BAR_6w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(316-2):(316+2)])
BAR_SD_6w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(316-2):(316+2)])
LID_6w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(316-2):(316+2)], na.rm = TRUE)
LID_SD_6w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(316-2):(316+2)], na.rm = TRUE)
Error_6w_440 <- (LID_5w_440 + ΔH_LID_440) - (BAR_6w_440 + ΔH_BAR_440)

# frame 7 whale
BAR_7w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(345-2):(345+2)])
BAR_SD_7w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(345-2):(345+2)])
LID_7w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(345-2):(345+2)], na.rm = TRUE)
LID_SD_7w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(345-2):(345+2)], na.rm = TRUE)
Error_7w_440 <- (LID_7w_440 + ΔH_LID_440) - (BAR_7w_440 + ΔH_BAR_440)

# frame 8 whale
BAR_8w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(406-2):(406+2)])
BAR_SD_8w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(406-2):(406+2)])
LID_8w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(406-2):(406+2)], na.rm = TRUE)
LID_SD_8w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(406-2):(406+2)], na.rm = TRUE)
Error_8w_440 <- (LID_8w_440 + ΔH_LID_440) - (BAR_8w_440 + ΔH_BAR_440)

# frame 9 whale
BAR_9w_440 <- mean(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(480-2):(480+2)])
BAR_SD_9w_440 <- sd(FLY440_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(480-2):(480+2)])
LID_9w_440 <- mean(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(480-2):(480+2)], na.rm = TRUE)
LID_SD_9w_440 <- sd(FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(480-2):(480+2)], na.rm = TRUE)
Error_9w_440 <- (LID_9w_440 + ΔH_LID_440) - (BAR_9w_440 + ΔH_BAR_440)

#### _______________ df 440 ####

df_440 <- data.frame(
  Flight_ID = rep("FLY440", times = 10),  
  second = c(622, 127, 185, 211, 240, 275, 316, 345, 406, 480),  
  Error = c(
    Error_pole_440,
    Error_1w_440,
    Error_2w_440,
    Error_3w_440,
    Error_4w_440,
    Error_5w_440,
    Error_6w_440,
    Error_7w_440,
    Error_8w_440,
    Error_9w_440
))

# Plot Error
ggplot(df_440, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 440",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_440$second), by = 60))


#### _______________ rest of ####

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
                                        lower_limit = 1, upper_limit = 80, omit_first_n = 0, omit_last_n = 10)

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

#### _______________ error ####
# check results by plotting
ggplot(FLY444_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(28, 490), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(137), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(166), linetype = "solid", color = "skyblue3") +
  #geom_vline(xintercept = c(212), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(237), linetype = "solid", color = "skyblue3") +
  #geom_vline(xintercept = c(357), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(371), linetype = "solid", color = "tan2") +
  labs(title = "2023-07-17 11:42:43 (FLY_444 LOG_138)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY444_REC_MOT_LD_1.2), by = 10))+
  ylim(0,100)


# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_444 <-1.64
PoleValue_LID_444 <-1.81

# pole
BAR_pole_444 <- mean(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(371-2):(371+2)])
BAR_SD_pole_444 <- sd(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(371-2):(371+2)])
LID_pole_444 <- mean(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(371-2):(371+2)])
LID_SD_pole_444 <- sd(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(371-2):(371+2)])

(BAR_pole_444 * PoleStandard)/PoleValue_BAR_444
#(LID_pole_444 * PoleStandard)/PoleValue_LID_444

correct_h_444 <- 27.45
ΔH_BAR_444 <- correct_h_444 - BAR_pole_444
ΔH_LID_444 <- correct_h_444 - LID_pole_444

BAR_corr_444 <- BAR_pole_444 + ΔH_BAR_444
LID_corr_444 <-LID_pole_444 + ΔH_LID_444


Error_pole_444 <- LID_corr_444 - BAR_corr_444

# frame 1 whale
BAR_1w_444 <- mean(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(137-2):(137+2)])
BAR_SD_1w_444 <- sd(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(137-2):(137+2)])
LID_1w_444 <- mean(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(137-2):(137+2)], na.rm = TRUE)
LID_SD_1w_444 <- sd(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(137-2):(137+2)], na.rm = TRUE)
Error_1w_444 <- (LID_1w_444 + ΔH_LID_444) - (BAR_1w_444 + ΔH_BAR_444) 

# frame 2 whale
BAR_2w_444 <- mean(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(166-2):(166+2)])
BAR_SD_2w_444 <- sd(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(166-2):(166+2)])
LID_2w_444 <- mean(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(166-2):(166+2)], na.rm = TRUE)
LID_SD_2w_444 <- sd(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(166-2):(166+2)], na.rm = TRUE)
Error_2w_444 <- (LID_2w_444 + ΔH_LID_444) - (BAR_2w_444 + ΔH_BAR_444)

# frame 3 whale
BAR_3w_444 <- mean(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(237-2):(237+2)])
BAR_SD_3w_444 <- sd(FLY444_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(237-2):(237+2)])
LID_3w_444 <- mean(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(237-2):(237+2)], na.rm = TRUE)
LID_SD_3w_444 <- sd(FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(237-2):(237+2)], na.rm = TRUE)
Error_3w_444 <- (LID_3w_444 + ΔH_LID_444) - (BAR_3w_444 + ΔH_BAR_444)

#### _______________ df 444 ####

df_444 <- data.frame(
  Flight_ID = rep("FLY44", times = 4),  
  second = c(371, 137, 166, 237),  
  Error = c(
    Error_pole_444,
    Error_1w_444,
    Error_2w_444,
    Error_3w_444
    ))

# Plot Error
ggplot(df_444, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 444",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_444$second), by = 60))


#### _______________ rest of ####

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


#### SPERM WHALE #### 

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

#### _______________ error ####
# check results by plotting
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(350, 570, 671, 715), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(392), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(420), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(435), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(450), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(470), linetype = "solid", color = "skyblue3") +
  #geom_vline(xintercept = c(487), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(507), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(521), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(533), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(710), linetype = "solid", color = "tan2") +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY304_REC_MOT_LD_1.2), by = 20))+
  ylim(0,100)


# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_304 <-1.60
PoleValue_LID_304 <-1.86

# pole
BAR_pole_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(710-2):(710+2)])
BAR_SD_pole_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(710-2):(710+2)])
LID_pole_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(710-2):(710+2)])
LID_SD_pole_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(710-2):(710+2)])

(BAR_pole_304 * PoleStandard)/PoleValue_BAR_304
#(LID_pole_304 * PoleStandard)/PoleValue_LID_304

correct_h_304 <- 35.83
ΔH_BAR_304 <- correct_h_304 - BAR_pole_304
ΔH_LID_304 <- correct_h_304 - LID_pole_304

BAR_corr_304 <- BAR_pole_304 + ΔH_BAR_304
LID_corr_304 <-LID_pole_304 + ΔH_LID_304


Error_pole_304 <- LID_corr_304 - BAR_corr_304

# frame 1 whale
BAR_1w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(392-2):(392+2)])
BAR_SD_1w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(392-2):(392+2)])
LID_1w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(392-2):(392+2)], na.rm = TRUE)
LID_SD_1w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(392-2):(392+2)], na.rm = TRUE)
Error_1w_304 <- (LID_1w_304 + ΔH_LID_304) - (BAR_1w_304 + ΔH_BAR_304) 

# frame 2 whale
BAR_2w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(420-2):(420+2)])
BAR_SD_2w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(420-2):(420+2)])
LID_2w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(420-2):(420+2)], na.rm = TRUE)
LID_SD_2w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(420-2):(420+2)], na.rm = TRUE)
Error_2w_304 <- (LID_2w_304 + ΔH_LID_304) - (BAR_2w_304 + ΔH_BAR_304)

# frame 3 whale
BAR_3w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(435-2):(435+2)])
BAR_SD_3w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(435-2):(435+2)])
LID_3w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(435-2):(435+2)], na.rm = TRUE)
LID_SD_3w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(435-2):(435+2)], na.rm = TRUE)
Error_3w_304 <- (LID_3w_304 + ΔH_LID_304) - (BAR_3w_304 + ΔH_BAR_304)

# frame 4 whale
BAR_4w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(450-2):(450+2)])
BAR_SD_4w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(450-2):(450+2)])
LID_4w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(450-2):(450+2)], na.rm = TRUE)
LID_SD_4w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(450-2):(450+2)], na.rm = TRUE)
Error_4w_304 <- (LID_4w_304 + ΔH_LID_304) - (BAR_4w_304 + ΔH_BAR_304)

# frame 5 whale
BAR_5w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(470-2):(470+2)])
BAR_SD_5w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(470-2):(470+2)])
LID_5w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(470-2):(470+2)], na.rm = TRUE)
LID_SD_5w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(470-2):(470+2)], na.rm = TRUE)
Error_5w_304 <- (LID_5w_304 + ΔH_LID_304) - (BAR_5w_304 + ΔH_BAR_304)

# frame 6 whale
BAR_6w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(507-2):(507+2)])
BAR_SD_6w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(507-2):(507+2)])
LID_6w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(507-2):(507+2)], na.rm = TRUE)
LID_SD_6w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(507-2):(507+2)], na.rm = TRUE)
Error_6w_304 <- (LID_6w_304 + ΔH_LID_304) - (BAR_6w_304 + ΔH_BAR_304)

# frame 7 whale
BAR_7w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(521-2):(521+2)])
BAR_SD_7w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(521-2):(521+2)])
LID_7w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(521-2):(521+2)], na.rm = TRUE)
LID_SD_7w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(521-2):(521+2)], na.rm = TRUE)
Error_7w_304 <- (LID_7w_304 + ΔH_LID_304) - (BAR_7w_304 + ΔH_BAR_304)

# frame 8 whale
BAR_8w_304 <- mean(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(533-2):(533+2)])
BAR_SD_8w_304 <- sd(FLY304_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(533-2):(533+2)])
LID_8w_304 <- mean(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(533-2):(533+2)], na.rm = TRUE)
LID_SD_8w_304 <- sd(FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(533-2):(533+2)], na.rm = TRUE)
Error_8w_304 <- (LID_8w_304 + ΔH_LID_304) - (BAR_8w_304 + ΔH_BAR_304)


#### _______________ df 304 ####

df_304 <- data.frame(
  Flight_ID = rep("FLY304", times = 9),  
  second = c(710, 392, 420, 435, 450, 470, 507, 521, 533),  
  Error = c(
    Error_pole_304,
    Error_1w_304,
    Error_2w_304,
    Error_3w_304,
    Error_4w_304,
    Error_5w_304,
    Error_6w_304,
    Error_7w_304,
    Error_8w_304
  ))

# Plot Error
ggplot(df_304, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 304",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_304$second), by = 60))


#### _______________ rest of ####

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

#### _______________ error ####

# check results by plotting
ggplot(FLY307_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(109, 457, 558, 597), linetype = "dashed", color = "gray") +
  #geom_vline(xintercept = c(155), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(171), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(184), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(197), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(211), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(224), linetype = "solid", color = "skyblue3") +
  #geom_vline(xintercept = c(240), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(254), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(269), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(285), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(318), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(333), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(372), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(403), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(419), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(429), linetype = "solid", color = "skyblue3") +
  
  geom_vline(xintercept = c(594), linetype = "solid", color = "tan2") +
  labs(title = "2022-06-30 15:14:31 (FLY_307 LOG_0032)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY307_REC_MOT_LD_1.2), by = 20))+
  ylim(0,100)


# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_307 <-1.74
PoleValue_LID_307 <-1.84

# pole
BAR_pole_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(594-2):(594+2)])
BAR_SD_pole_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(594-2):(594+2)])
LID_pole_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(594-2):(594+2)])
LID_SD_pole_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(594-2):(594+2)])

(BAR_pole_307 * PoleStandard)/PoleValue_BAR_307
#(LID_pole_307 * PoleStandard)/PoleValue_LID_307


correct_h_307 <- 38.23
ΔH_BAR_307 <- correct_h_307 - BAR_pole_307
ΔH_LID_307 <- correct_h_307 - LID_pole_307

BAR_corr_307 <- BAR_pole_307 + ΔH_BAR_307
LID_corr_307 <-LID_pole_307 + ΔH_LID_307


Error_pole_307 <- LID_corr_307 - BAR_corr_307

# frame 1 whale
BAR_1w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(171-2):(171+2)])
BAR_SD_1w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(171-2):(171+2)])
LID_1w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(171-2):(171+2)], na.rm = TRUE)
LID_SD_1w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(171-2):(171+2)], na.rm = TRUE)
Error_1w_307 <- (LID_1w_307 + ΔH_LID_307) - (BAR_1w_307 + ΔH_BAR_307) 

# frame 2 whale
BAR_2w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(184-2):(184+2)])
BAR_SD_2w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(184-2):(184+2)])
LID_2w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(184-2):(184+2)], na.rm = TRUE)
LID_SD_2w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(184-2):(184+2)], na.rm = TRUE)
Error_2w_307 <- (LID_2w_307 + ΔH_LID_307) - (BAR_2w_307 + ΔH_BAR_307)

# frame 3 whale
BAR_3w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(197-2):(197+2)])
BAR_SD_3w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(197-2):(197+2)])
LID_3w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(197-2):(197+2)], na.rm = TRUE)
LID_SD_3w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(197-2):(197+2)], na.rm = TRUE)
Error_3w_307 <- (LID_3w_307 + ΔH_LID_307) - (BAR_3w_307 + ΔH_BAR_307)

# frame 4 whale
BAR_4w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(211-2):(211+2)])
BAR_SD_4w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(211-2):(211+2)])
LID_4w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(211-2):(211+2)], na.rm = TRUE)
LID_SD_4w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(211-2):(211+2)], na.rm = TRUE)
Error_4w_307 <- (LID_4w_307 + ΔH_LID_307) - (BAR_4w_307 + ΔH_BAR_307)

# frame 5 whale
BAR_5w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(224-2):(224+2)])
BAR_SD_5w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(224-2):(224+2)])
LID_5w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(224-2):(224+2)], na.rm = TRUE)
LID_SD_5w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(224-2):(224+2)], na.rm = TRUE)
Error_5w_307 <- (LID_5w_307 + ΔH_LID_307) - (BAR_5w_307 + ΔH_BAR_307)

# frame 6 whale
BAR_6w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(240-2):(240+2)])
BAR_SD_6w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(240-2):(240+2)])
LID_6w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(240-2):(240+2)], na.rm = TRUE)
LID_SD_6w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(240-2):(240+2)], na.rm = TRUE)
Error_6w_307 <- (LID_6w_307 + ΔH_LID_307) - (BAR_6w_307 + ΔH_BAR_307)

# frame 7 whale
BAR_7w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(254-2):(254+2)])
BAR_SD_7w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(254-2):(254+2)])
LID_7w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(254-2):(254+2)], na.rm = TRUE)
LID_SD_7w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(254-2):(254+2)], na.rm = TRUE)
Error_7w_307 <- (LID_7w_307 + ΔH_LID_307) - (BAR_7w_307 + ΔH_BAR_307)

# frame 8 whale
BAR_8w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(269-2):(269+2)])
BAR_SD_8w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(269-2):(269+2)])
LID_8w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(269-2):(269+2)], na.rm = TRUE)
LID_SD_8w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(269-2):(269+2)], na.rm = TRUE)
Error_8w_307 <- (LID_8w_307 + ΔH_LID_307) - (BAR_8w_307 + ΔH_BAR_307)

# frame 9 whale
BAR_9w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(285-2):(285+2)])
BAR_SD_9w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(285-2):(285+2)])
LID_9w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(285-2):(285+2)], na.rm = TRUE)
LID_SD_9w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(285-2):(285+2)], na.rm = TRUE)
Error_9w_307 <- (LID_9w_307 + ΔH_LID_307) - (BAR_9w_307 + ΔH_BAR_307)

# frame 10 whale
BAR_10w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(318-2):(318+2)])
BAR_SD_10w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(318-2):(318+2)])
LID_10w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(318-2):(318+2)], na.rm = TRUE)
LID_SD_10w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(318-2):(318+2)], na.rm = TRUE)
Error_10w_307 <- (LID_10w_307 + ΔH_LID_307) - (BAR_10w_307 + ΔH_BAR_307)

# frame 11 whale
BAR_11w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(333-2):(333+2)])
BAR_SD_11w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(333-2):(333+2)])
LID_11w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(333-2):(333+2)], na.rm = TRUE)
LID_SD_11w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(333-2):(333+2)], na.rm = TRUE)
Error_11w_307 <- (LID_11w_307 + ΔH_LID_307) - (BAR_11w_307 + ΔH_BAR_307)

# frame 12 whale
BAR_12w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
BAR_SD_12w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
LID_12w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)], na.rm = TRUE)
LID_SD_12w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)], na.rm = TRUE)
Error_12w_307 <- (LID_12w_307 + ΔH_LID_307) - (BAR_12w_307 + ΔH_BAR_307)

# frame 13 whale
BAR_13w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(403-2):(403+2)])
BAR_SD_13w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(403-2):(403+2)])
LID_13w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(403-2):(403+2)], na.rm = TRUE)
LID_SD_13w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(403-2):(403+2)], na.rm = TRUE)
Error_13w_307 <- (LID_13w_307 + ΔH_LID_307) - (BAR_13w_307 + ΔH_BAR_307)

# frame 14 whale
BAR_14w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(419-2):(419+2)])
BAR_SD_14w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(419-2):(419+2)])
LID_14w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(419-2):(419+2)], na.rm = TRUE)
LID_SD_14w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(419-2):(419+2)], na.rm = TRUE)
Error_14w_307 <- (LID_14w_307 + ΔH_LID_307) - (BAR_14w_307 + ΔH_BAR_307)

# frame 15 whale
BAR_15w_307 <- mean(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(429-2):(429+2)])
BAR_SD_15w_307 <- sd(FLY307_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(429-2):(429+2)])
LID_15w_307 <- mean(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(429-2):(429+2)], na.rm = TRUE)
LID_SD_15w_307 <- sd(FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(429-2):(429+2)], na.rm = TRUE)
Error_15w_307 <- (LID_15w_307 + ΔH_LID_307) - (BAR_15w_307 + ΔH_BAR_307)


#### _______________ df 304 ####

df_307 <- data.frame(
  Flight_ID = rep("FLY307", times = 15),  
  second = c(594, 171, 184, 197, 211, 224, 254, 269, 285, 318, 333, 372, 403, 419, 429),  
  Error = c(
    Error_pole_307,
    Error_1w_307,
    Error_2w_307,
    Error_3w_307,
    Error_4w_307,
    Error_5w_307,
    Error_7w_307,
    Error_8w_307,
    Error_9w_307,
    Error_10w_307,
    Error_11w_307,
    Error_12w_307,
    Error_13w_307,
    Error_14w_307,
    Error_15w_307
  ))

# Plot Error
ggplot(df_307, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 307",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, max(df_307$second), by = 60))


#### _______________ rest of ####

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



#### Mixed Effect #### 

all_df <- bind_rows(df_304,df_307,df_310,df_330,df_365,df_368,df_401,
                    df_412,df_417,df_420,df_425,df_437,df_440,df_444)


mixed_model_error <- lmer(Error ~ scale(second) + (scale(second) | Flight_ID) , data = all_df, REML = FALSE,
                   control = lmerControl(optimizer ="Nelder_Mead"))

plot_all_df <- ggplot(all_df, aes(x = second, y = Error, color = Flight_ID)) +
  geom_point() + geom_smooth(method = "lm", fill = NA) 

plot_mfx_error <- plot_model(mixed_model_error, type = "pred", terms = c("second"), show.data = TRUE)

modelsummary::get_gof(mixed_model_error)
# r2.conditional = 0.9033624

