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
ggplot(FLY304_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-06-30 08:15:31 (FLY_304 LOG_0030)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))
