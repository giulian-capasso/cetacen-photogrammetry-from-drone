#### VOLI CON MISURAZIONI ####

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
library(purrr)

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

# heigth frame 1 whale
mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(285-2):(285+2)])
sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(285-2):(285+2)])
mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(285-2):(285+2)])
sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(285-2):(285+2)])

#heigth frame pole
BAR_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(504-2):(504+2)])
sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(504-2):(504+2)])
LID_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(504-2):(504+2)])
sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(504-2):(504+2)])

PoleStandard<-1.88
PoleValue_BAR_310<-1.71
PoleValue_LID_310<-1.86

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_310 <-1.71
PoleValue_lID_310 <-1.86

BAR_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(547-2):(547+2)])
BAR_SD_pole_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(547-2):(547+2)])
LID_pole_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(547-2):(547+2)])
LID_SD_pole_301 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(547-2):(547+2)])

BAR_corr_310 <- ((mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(547-2):(547+2)])) * PoleStandard)/PoleValue_310
LID_corr_310 <-((mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(547-2):(547+2)])) * PoleStandard)/1.86

LID_corr_310 - BAR_corr_310
BAR_corr_310 - BAR_pole_310
ΔH_BAR_310 <- BAR_corr_310 - BAR_pole_310

Error_pole_310 <- LID_pole_310 - (BAR_pole_310 + ΔH_BAR_310)

# frame 1 whale
BAR_1w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(255-2):(255+2)])
BAR_SD_1w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(255-2):(255+2)])
LID_1w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(255-2):(255+2)])
LID_SD_1w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(255-2):(255+2)])
Error_1w_310 <- LID_1w - (BAR_1w + ΔH_BAR)

# frame 2 whale 
BAR_2w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(270-2):(270+2)])
BAR_SD_2w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(270-2):(270+2)])
LID_2w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(270-2):(270+2)])
LID_SD_2w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(270-2):(270+2)])
Error_2w_310 <- LID_2w - (BAR_2w + ΔH_BAR)

# frame 3 whale 
BAR_3w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(286-2):(286+2)])
BAR_SD_3w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(286-2):(286+2)])
LID_3w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(286-2):(286+2)])
LID_SD_3w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(286-2):(286+2)])
Error_3w_310 <- LID_3w - (BAR_3w + ΔH_BAR)

# frame 4 whale 
BAR_4w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(301-2):(301+2)])
BAR_SD_4w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(301-2):(301+2)])
LID_4w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(301-2):(301+2)])
LID_SD_4w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(301-2):(301+2)])
Error_4w_310 <- LID_4w - (BAR_4w + ΔH_BAR)

# frame 5 whale 
BAR_5w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(317-2):(317+2)])
BAR_SD_5w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(317-2):(317+2)])
LID_5w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(317-2):(317+2)])
LID_SD_5w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(317-2):(317+2)])
Error_5w_310 <- LID_5w - (BAR_5w + ΔH_BAR)

# frame 6 whale 
BAR_6w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(334-2):(334+2)])
BAR_SD_6w_310 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(334-2):(334+2)])
LID_6w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(334-2):(334+2)])
LID_SD_6w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(334-2):(334+2)])
Error_6w_310 <- LID_6w - (BAR_6w + ΔH_BAR)

# frame 7 whale 

BAR_7w_310 <- mean(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(350-2):(350+2)])
BAR_SD_7w_301 <- sd(FLY310_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(350-2):(350+2)])
LID_7w_310 <- mean(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(350):(350+2)])
LID_SD_7w_310 <- sd(FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(350-2):(350+2)])
Error_7w_310 <- LID_7w_310 - (BAR_7w_310 + ΔH_BAR_310)
GSD_BAR_7w_310 <- (((BAR_7w_310 + ΔH_BAR))/24.962606)*0.004223633 %>% GSD_BAR_7w*1000
GSD_LID_7w_310 <- (LID_7w_/24.962606)*0.004223633
# GSD 
GSD_BAR_7w_310 *2000
GSD_LID_7w_310 *2000

#### _______________ df 310 ####
df_310 <- data.frame(
  Flight_ID = rep("FLY310", times = 8),  
  second = c(547, 255, 270, 286, 301, 317, 334, 350),  
  Error = c(
    LID_pole_310 - (BAR_pole_310 + ΔH_BAR_310),
    LID_1w_310 - (BAR_1w_310 + ΔH_BAR_310),
    LID_2w_310 - (BAR_2w_310 + ΔH_BAR_310),
    LID_3w_310 - (BAR_3w_310 + ΔH_BAR_310),
    LID_4w_310 - (BAR_4w_310 + ΔH_BAR_310),
    LID_5w_310 - (BAR_5w_310 + ΔH_BAR_310),
    LID_6w_310 - (BAR_6w_310 + ΔH_BAR_310),
    LID_7w_310 - (BAR_7w_310 + ΔH_BAR_310)
  )
)

# Plot Error
ggplot(df_310, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 310",
       x = "Second",
       y = "Error") +
  ylim(c(-2, 2)) 
  #scale_x_continuous(limits = c(0, 800))

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
PoleValue_BAR_330 <-1.65
PoleValue_LID_330 <-1.85

BAR_pole_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
BAR_SD_pole_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(690-2):(690+2)])
LID_pole_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])
LID_SD_pole_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(690-2):(690+2)])

BAR_corr_330 <- (BAR_pole_330 * PoleStandard)/PoleValue_BAR_330
LID_corr_330 <- (LID_pole_330 * PoleStandard)/PoleValue_LID_330 

LID_corr_330 - BAR_corr_330
ΔH_BAR_330 <- BAR_corr_330 - BAR_pole_330

Error_pole <- LID_pole_330 - (BAR_pole_330 + ΔH_BAR_330)

# frame 1 whale
BAR_1w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(195-2):(195+2)])
BAR_SD_1w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(195-2):(195+2)])
LID_1w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(195-2):(195+2)])
LID_SD_1w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(195-2):(195+2)])
Error_1w_330 <- LID_1w_330 - (BAR_1w_330 + ΔH_BAR_330)

# frame 2 whale
BAR_2w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(352-2):(352+2)])
BAR_SD_2w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(352-2):(352+2)])
LID_2w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(352-2):(352+2)])
LID_SD_2w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(352-2):(352+2)])
Error_2w_330 <- LID_2w_330 - (BAR_2w_330 + ΔH_BAR_330)

# frame 3 whale
BAR_3w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
BAR_SD_3w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(372-2):(372+2)])
LID_3w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)])
LID_SD_3w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(372-2):(372+2)])
Error_3w_330 <- LID_3w_330 - (BAR_3w_330 + ΔH_BAR_330)

# frame 4 whale
BAR_4w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(416-2):(416+2)])
BAR_SD_4w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(416-2):(416+2)])
LID_4w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(416-2):(416+2)])
LID_SD_4w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(416-2):(416+2)])
Error_4w_330 <- LID_4w_330 - (BAR_4w_330 + ΔH_BAR_330)

# frame 5 whale
BAR_5w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(467-2):(467+2)])
BAR_SD_5w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(467-2):(467+2)])
LID_5w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(467-2):(467+2)])
LID_SD_5w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(467-2):(467+2)])
Error_5w_330 <- LID_5w_330 - (BAR_5w_330 + ΔH_BAR_330)

# frame 6 whale
BAR_6w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
BAR_SD_6w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(528-2):(528+2)])
LID_6w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528-2):(528+2)])
LID_SD_6w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(528):(528+2)])
Error_6w_330 <- LID_6w_330 - (BAR_6w_330 + ΔH_BAR_330)

# frame 7 whale
BAR_7w_330 <- mean(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(559-2):(559+2)])
BAR_SD_7w_330 <- sd(FLY330_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(559-2):(559+2)])
LID_7w_330 <- mean(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(559-2):(559+2)])
LID_SD_7w_330 <- sd(FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(559):(559+2)])
Error_7w_330 <- LID_7w_330 - (BAR_7w_330 + ΔH_BAR_330)

#### _______________ df_330 ####
df_330 <- data.frame(
  Flight_ID = rep("FLY330", times = 8),  
  second = c(690, 195, 352, 372, 416, 467, 528, 559),  
  Error = c(
    LID_pole_330 - (BAR_pole_330 + ΔH_BAR_330),
    LID_1w_330 - (BAR_1w_330 + ΔH_BAR_330),
    LID_2w_330 - (BAR_2w_330 + ΔH_BAR_330),
    LID_3w_330 - (BAR_3w_330 + ΔH_BAR_330),
    LID_4w_330 - (BAR_4w_330 + ΔH_BAR_330),
    LID_5w_330 - (BAR_5w_330 + ΔH_BAR_330),
    LID_6w_330 - (BAR_6w_330 + ΔH_BAR_330),
    LID_7w_330 - (BAR_7w_330 + ΔH_BAR_330)
  )
)

# Plot Error
ggplot(df_330, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 330",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 

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

BAR_corr_365 <- (BAR_pole_365 * PoleStandard)/PoleValue_BAR_365
LID_corr_365 <- (LID_pole_365 * PoleStandard)/PoleValue_LID_365

LID_corr_365 - BAR_corr_365
ΔH_BAR_365 <- BAR_corr_365 - BAR_pole_365

Error_pole_365 <- LID_pole_365 - (BAR_pole_365 + ΔH_BAR_365)

# frame 1 whale
BAR_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
BAR_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
LID_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
LID_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
Error_1w_365 <- LID_1w_365 - (BAR_1w_365 + ΔH_BAR_365) 

# frame 2 whale
BAR_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
BAR_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
LID_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
LID_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
Error_2w_365 <- LID_2w_365 - (BAR_2w_365 + ΔH_BAR_365)

# frame 3 whale
BAR_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
BAR_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
LID_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
LID_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
Error_3w_365 <- LID_3w_365 - (BAR_3w_365 + ΔH_BAR_365)

# frame 4 whale
BAR_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
BAR_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
LID_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
LID_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
Error_4w_365 <- LID_4w_365 - (BAR_4w_365 + ΔH_BAR_365)

# frame 5 whale
BAR_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
BAR_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
LID_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
LID_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
Error_5w_365 <- LID_5w_365 - (BAR_5w_365 + ΔH_BAR_365)


#### _______________ df 365 ####

df_365 <- data.frame(
  Flight_ID = rep("FLY365", times = 6),  
  second = c(595, 79, 152, 188, 234, 267),  
  Error = c(
    LID_pole_365 - (BAR_pole_365 + ΔH_BAR_365),
    LID_1w_365 - (BAR_1w_365 + ΔH_BAR_365),
    LID_2w_365 - (BAR_2w_365 + ΔH_BAR_365),
    LID_3w_365 - (BAR_3w_365 + ΔH_BAR_365),
    LID_4w_365 - (BAR_4w_365 + ΔH_BAR_365),
    LID_5w_365 - (BAR_5w_365 + ΔH_BAR_365)
  )
)

# Plot Error
ggplot(df_365, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 365",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 

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

BAR_corr_368 <- (BAR_pole_368 * PoleStandard)/PoleValue_BAR_368
LID_corr_368 <- (LID_pole_368 * PoleStandard)/PoleValue_LID_368

LID_corr_368 - BAR_corr_368
ΔH_BAR_368 <- BAR_corr_368 - BAR_pole_368

Error_pole_368 <- LID_pole_368 - (BAR_pole_368 + ΔH_BAR_368)

# frame 1 whale
BAR_1w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(168-2):(168+2)])
BAR_SD_1w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(168-2):(168+2)])
LID_1w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(168-2):(168+2)])
LID_SD_1w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(168-2):(168+2)])
Error_1w_368 <- LID_1w_368 - (BAR_1w_368 + ΔH_BAR_368) 

# frame 2 whale
BAR_2w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
BAR_SD_2w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(185-2):(185+2)])
LID_2w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(185-2):(185+2)])
LID_SD_2w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(185-2):(185+2)])
Error_2w_368 <- LID_2w_368 - (BAR_2w_368 + ΔH_BAR_368)

# frame 3 whale
BAR_3w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(204-2):(204+2)])
BAR_SD_3w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(204-2):(204+2)])
LID_3w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(204-2):(204+2)])
LID_SD_3w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(204-2):(204+2)])
Error_3w_368 <- LID_3w_368 - (BAR_3w_368 + ΔH_BAR_368)

# frame 4 whale
BAR_4w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(222-2):(222+2)])
BAR_SD_4w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(222-2):(222+2)])
LID_4w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(222-2):(222+2)])
LID_SD_4w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(222-2):(222+2)])
Error_4w_368 <- LID_4w_368 - (BAR_4w_368 + ΔH_BAR_368)

# frame 5 whale
BAR_5w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(235-2):(235+2)])
BAR_SD_5w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(235-2):(235+2)])
LID_5w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(235-2):(235+2)])
LID_SD_5w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(235-2):(235+2)])
Error_5w_368 <- LID_5w_368 - (BAR_5w_368 + ΔH_BAR_368)

# frame 6 whale
BAR_6w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(247-2):(247+2)])
BAR_SD_6w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(247-2):(247+2)])
LID_6w_368 <- mean(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(247-2):(247+2)])
LID_SD_6w_368 <- sd(FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(247-2):(247+2)])
Error_6w_368 <- LID_6w_368 - (BAR_6w_368 + ΔH_BAR_368)

#### _______________ df 368 ####

df_368 <- data.frame(
  Flight_ID = rep("FLY368", times = 7),  
  second = c(545, 168, 185, 204, 222, 235, 247),  
  Error = c(
    LID_pole_368 - (BAR_pole_368 + ΔH_BAR_368),
    LID_1w_368 - (BAR_1w_368 + ΔH_BAR_368),
    LID_2w_368 - (BAR_2w_368 + ΔH_BAR_368),
    LID_3w_368 - (BAR_3w_368 + ΔH_BAR_368),
    LID_4w_368 - (BAR_4w_368 + ΔH_BAR_368),
    LID_5w_368 - (BAR_5w_368 + ΔH_BAR_368),
    LID_6w_368 - (BAR_6w_368 + ΔH_BAR_368)
  )
)

# Plot Error
ggplot(df_368, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 368",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 


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



#### FLY_401 + LOG_0094 ####
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
PoleValue_LID_401 <-1.84

BAR_pole_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
BAR_SD_pole_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(156-2):(156+2)])
LID_pole_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])
LID_SD_pole_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(156-2):(156+2)])

BAR_corr_401 <- (BAR_pole_401 * PoleStandard)/PoleValue_BAR_401
LID_corr_401 <- (LID_pole_401 * PoleStandard)/PoleValue_LID_401

LID_corr_401 - BAR_corr_401
ΔH_BAR_401 <- BAR_corr_401 - BAR_pole_401

Error_pole_401 <- LID_pole_401 - (BAR_pole_401 + ΔH_BAR_401)

# frame 1 whale
BAR_1w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(46-2):(46+2)])
BAR_SD_1w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(46-2):(46+2)])
LID_1w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(46-2):(46+2)])
LID_SD_1w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(46-2):(46+2)])
Error_1w_401 <- LID_1w_401 - (BAR_1w_401 + ΔH_BAR_401) 

# frame 2 whale
BAR_2w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(53-2):(53+2)])
BAR_SD_2w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(53-2):(53+2)])
LID_2w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(53-2):(53+2)])
LID_SD_2w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(53-2):(53+2)])
Error_2w_401 <- LID_2w_401 - (BAR_2w_401 + ΔH_BAR_401)

# frame 3 whale
BAR_3w_401 <- mean(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
BAR_SD_3w_401 <- sd(FLY401_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
LID_3w_401 <- mean(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(69-2):(69+2)])
LID_SD_3w_401 <- sd(FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(69-2):(69+2)])
Error_3w_401 <- LID_3w_401 - (BAR_3w_401 + ΔH_BAR_401)

#### _______________ df 401 ####

#### _______________ df 336 ####

df_401 <- data.frame(
  Flight_ID = rep("FLY401", times = 4),  
  second = c(156, 46, 53, 69),  
  Error = c(
    LID_pole_365 - (BAR_pole_365 + ΔH_BAR_365),
    LID_1w_365 - (BAR_1w_365 + ΔH_BAR_365),
    LID_2w_365 - (BAR_2w_365 + ΔH_BAR_365),
    LID_3w_365 - (BAR_3w_365 + ΔH_BAR_365))
)

# Plot Error
ggplot(df_401, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 401",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 

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

BAR_corr_412 <- (BAR_pole_412 * PoleStandard)/PoleValue_BAR_412
LID_corr_412 <- (LID_pole_412 * PoleStandard)/PoleValue_LID_412

LID_corr_412 - BAR_corr_412
ΔH_BAR_412 <- BAR_corr_412 - BAR_pole_412

Error_pole_412 <- LID_pole_412 - (BAR_pole_412 + ΔH_BAR_412)

# frame 1 whale
BAR_1w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(58-2):(58+2)])
BAR_SD_1w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(58-2):(58+2)])
LID_1w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(58-2):(58+2)])
LID_SD_1w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(58-2):(58+2)])
Error_1w_412 <- LID_1w_412 - (BAR_1w_412 + ΔH_BAR_412) 

# frame 2 whale
BAR_2w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(80-2):(80+2)])
BAR_SD_2w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(80-2):(80+2)])
LID_2w_412 <- mean(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(80-2):(80+2)])
LID_SD_2w_412 <- sd(FLY412_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(80-2):(80+2)])
Error_2w_412 <- LID_2w_412 - (BAR_2w_412 + ΔH_BAR_412)


#### _______________ df 412 ####

df_412 <- data.frame(
  Flight_ID = rep("FLY412", times = 3),  
  second = c(595, 79, 152, 188, 234, 267),  
  Error = c(
    LID_pole_412 - (BAR_pole_412 + ΔH_BAR_412),
    LID_1w_412 - (BAR_1w_412 + ΔH_BAR_412),
    LID_2w_412 - (BAR_2w_412 + ΔH_BAR_412))
)

# Plot Error
ggplot(df_412, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 412",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 

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



#### FLY_417 + LOG106 #### SERVE PRUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

# check results by plotting
ggplot(FLY417_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  # geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(16,685), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = c(19), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(31), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(43), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(59), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(72), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(185), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(207), linetype = "solid", color = "skyblue3") +
  geom_vline(xintercept = c(177), linetype = "solid", color = "pink") +
  geom_vline(xintercept = c(213), linetype = "solid", color = "tan2") +
  geom_vline(xintercept = c(253), linetype = "solid", color = "tan2") +
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
mean(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(165-2):(165+2)])
sd(FLY417_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(165-2):(165+2)])
mean(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(165-2):(165+2)], na.rm = TRUE)
sd(FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(165-2):(165+2)], na.rm = TRUE)

# Error calculation 
PoleStandard <- 1.88  
PoleValue_BAR_365 <-1.70
PoleValue_LID_365 <-1.86

BAR_pole_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
BAR_SD_pole_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(595-2):(595+2)])
LID_pole_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])
LID_SD_pole_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(595-2):(595+2)])

BAR_corr_365 <- (BAR_pole_365 * PoleStandard)/PoleValue_BAR_365
LID_corr_365 <- (LID_pole_365 * PoleStandard)/PoleValue_LID_365

LID_corr_365 - BAR_corr_365
ΔH_BAR_365 <- BAR_corr_365 - BAR_pole_365

Error_pole <- LID_pole_365 - (BAR_pole_365 + ΔH_BAR_365)

# frame 1 whale
BAR_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
BAR_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(79-2):(79+2)])
LID_1w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
LID_SD_1w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(79-2):(79+2)])
Error_1w_365 <- LID_1w_365 - (BAR_1w_365 + ΔH_BAR_365) 

# frame 2 whale
BAR_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
BAR_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(152-2):(152+2)])
LID_2w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
LID_SD_2w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(152-2):(152+2)])
Error_2w_365 <- LID_2w_365 - (BAR_2w_365 + ΔH_BAR_365)

# frame 3 whale
BAR_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
BAR_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(188-2):(188+2)])
LID_3w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
LID_SD_3w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(188-2):(188+2)])
Error_3w_365 <- LID_3w_365 - (BAR_3w_365 + ΔH_BAR_365)

# frame 4 whale
BAR_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
BAR_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(234-2):(234+2)])
LID_4w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
LID_SD_4w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(234-2):(234+2)])
Error_4w_365 <- LID_4w_365 - (BAR_4w_365 + ΔH_BAR_365)

# frame 5 whale
BAR_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
BAR_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[(267-2):(267+2)])
LID_5w_365 <- mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
LID_SD_5w_365 <- sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[(267-2):(267+2)])
Error_5w_365 <- LID_5w_365 - (BAR_5w_365 + ΔH_BAR_365)


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
PoleValue_BAR_420 <-1.66
PoleValue_LID_420 <-1.86

BAR_pole_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
BAR_SD_pole_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(625-2):(625+2)])
LID_pole_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])
LID_SD_pole_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(625-2):(625+2)])

BAR_corr_420 <- (BAR_pole_420 * PoleStandard)/PoleValue_BAR_420
LID_corr_420 <- (LID_pole_420 * PoleStandard)/PoleValue_LID_420

LID_corr_420 - BAR_corr_420
ΔH_BAR_420 <- BAR_corr_420 - BAR_pole_420

Error_pole_420 <- LID_pole_420 - (BAR_pole_420 + ΔH_BAR_420)

# frame 1 whale
BAR_1w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(43-2):(43+2)])
BAR_SD_1w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(43-2):(43+2)])
LID_1w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(43-2):(43+2)])
LID_SD_1w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(43-2):(43+2)])
Error_1w_420 <- LID_1w_420 - (BAR_1w_420 + ΔH_BAR_420) 

# frame 2 whale
BAR_2w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
BAR_SD_2w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(69-2):(69+2)])
LID_2w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(69-2):(69+2)])
LID_SD_2w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(69-2):(69+2)])
Error_2w_420 <- LID_2w_420 - (BAR_2w_420 + ΔH_BAR_420)

# frame 3 whale
BAR_3w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(89-2):(89+2)])
BAR_SD_3w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(89-2):(89+2)])
LID_3w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(89-2):(89+2)])
LID_SD_3w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(89-2):(89+2)])
Error_3w_420 <- LID_3w_420 - (BAR_3w_420 + ΔH_BAR_420)

# frame 4 whale
BAR_4w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(111-2):(111+2)])
BAR_SD_4w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(111-2):(111+2)])
LID_4w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(111-2):(111+2)])
LID_SD_4w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(111-2):(111+2)])
Error_4w_420 <- LID_4w_420 - (BAR_4w_420 + ΔH_BAR_420)

# frame 5 whale
BAR_5w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(135-2):(135+2)])
BAR_SD_5w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$`osd_data:relativeHeight[meters]`[(135-2):(135+2)])
LID_5w_420 <- mean(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(135-2):(135+2)])
LID_SD_5w_420 <- sd(FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned[(135-2):(135+2)])
Error_5w_420 <- LID_5w_420 - (BAR_5w_420 + ΔH_BAR_420)

#### _______________ df 420 ####

df_420 <- data.frame(
  Flight_ID = rep("FLY420", times = 6),  
  second = c(625, 43, 69, 89, 111, 135),  
  Error = c(
    LID_pole_420 - (BAR_pole_420 + ΔH_BAR_420),
    LID_1w_420 - (BAR_1w_420 + ΔH_BAR_420),
    LID_2w_420 - (BAR_2w_420 + ΔH_BAR_420),
    LID_3w_420 - (BAR_3w_420 + ΔH_BAR_420),
    LID_4w_420 - (BAR_4w_420 + ΔH_BAR_420),
    LID_5w_420 - (BAR_5w_420 + ΔH_BAR_420)
  )
)

# Plot Error
ggplot(df_420, aes(x = second, y = Error)) +
  geom_line() +
  labs(title = "Plot di Error vs Second Flight 420",
       x = "Second",
       y = "Error in meters") +
  ylim(c(-5, 5)) 


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


