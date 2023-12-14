#### Tilt Laser analysis ####
plot_tilt_424 <- ggplot(FLY424_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = FLY424_REC_MOT_LD_1.2$laser_altitude_m, color = "Variable 1"), size = 0.5) +
  geom_line(aes(y = FLY424_REC_MOT_LD_1.2$tilt_deg, color = "Variable 2"), size = 0.5) +
  scale_color_manual(values = c("Variable 1" = "blue", "Variable 2" = "darkorange")) +
  labs(x = "Time", y = "Altezza Laser / Tilt Laser") +
  theme_minimal()

plot_tilt_424 + scale_y_continuous(
  sec.axis = sec_axis(~./5, name = "Variable 2", breaks = seq(-5, 5, 1)))

# plot Lidar & tilt 
LD_TILT_285 <- ggplot(FLY285_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "Height"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = tilt_deg, color = "Tilt"), size = 0.7, linetype = "solid") +
  labs(title = "2021-04-22 17:10:01 (FLY_285) Lidar Tilt", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "darkgray")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
LD_TILT_285

#### Pulizia outliers con mediana #####
# Pulizia Outliers
apply_median_filter <- function(x, window_size) {
  result <- x
  for (i in 1:length(x)) {
    # Evita i bordi della serie
    if (!is.na(x[i])) {
      start <- max(1, i - window_size)
      end <- min(length(x), i + window_size)
      
      # Conta il numero di valori non NA nella finestra
      non_na_count <- sum(!is.na(x[start:end]))
      
      # Se ci sono abbastanza valori non NA, calcola la mediana
      if (non_na_count >= window_size) {
        result[i] <- median(x[start:end], na.rm = TRUE)
      }

#### LIDAR FLITER FROM BAR ####
lidar_filter3 <- function(lidar, baro, window_size, lower_limit, upper_limit) {
  result <- lidar
  for (i in 1:length(lidar)) {
    # Omit borders
    if (!is.na(lidar[i])) {
      start <- max(1, i - window_size)
      end <- min(length(lidar), i + window_size)
      
      # Calcola la differenza tra il valore Lidar e il valore corrispondente nel barometrico
      differences <- lidar[start:end] - baro[start:end]
      
      # Applica la correzione SOLO se il valore Lidar è maggiore di 60 o minore di 2
      if (lidar[i] > 60 || lidar[i] < 2) {
        # Calcola la mediana della differenza escludendo gli outliers
        valid_differences <- differences[differences >= lower_limit & differences <= upper_limit]
        median_difference <- median(valid_differences)
        result[i] <- baro[i] + median_difference
      }
    }
  }
  return(result)
}

# Esempio di utilizzo
FLY299_REC_MOT_LD_1.2$laser_altitude_m_cleaned3 <- lidar_filter3(
  FLY299_REC_MOT_LD_1.2$laser_altitude_m,
  FLY299_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`,
  window_size = 40,
  lower_limit = 0,
  upper_limit = 60
)
clean_299 <- ggplot(FLY299_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned3, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "Confronto tra Altezza Lidar originale, pulita e Barometrica",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top")
clean_299
      # Altrimenti, lascia invariato il valore
    }
  }
  return(result)
}

# Applica il filtro mediano alla colonna con window_size = 10
FLY424_REC_MOT_LD_1.2$laser_altitude_m_cleaned <- apply_median_filter(FLY424_REC_MOT_LD_1.2$laser_altitude_m, window_size = 10)

confronto_md <- ggplot(FLY424_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar Altitude"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Altitude"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR Altitude"), linetype = "solid", size = 0.5) +
  labs(title = "Confronto tra laser_altitude_m, laser_altitude_m_cleaned e BAR_altitude",
       y = "Altitude",
       x = "Date/Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  theme(legend.position = "top")
confronto_md

# funzione per quando si vuole riempire il lidar con i dati precedeti e successivi una volta eliminati gli outliers. 
# solitamente da fare dopo lidar_filter function
Fill_mediana <- function(colonna) {
  indici_na <- which(is.na(colonna))
  for (i in indici_na) {
    indici_prepost <- seq(i - 5, i + 5)
    
    # Rimuovi gli indici che sono fuori dai limiti della colonna
    indici_prepost <- indici_prepost[indici_prepost > 0 & indici_prepost <= length(colonna)]
    
    # Se ci sono abbastanza valori non NA, sostituisci il NA con la mediana
    if (length(na.omit(colonna[indici_prepost])) >= 5) {
      colonna[i] <- median(colonna[indici_prepost], na.rm = TRUE)
    }
  }
  
  return(colonna)
}

