#### Tilt Laser analysis ####
plot_tilt_424 <- ggplot(FLY424_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = FLY424_REC_MOT_LD_1.2$laser_altitude_m, color = "Variable 1"), size = 0.5) +
  geom_line(aes(y = FLY424_REC_MOT_LD_1.2$tilt_deg, color = "Variable 2"), size = 0.5) +
  scale_color_manual(values = c("Variable 1" = "blue", "Variable 2" = "darkorange")) +
  labs(x = "Time", y = "Altezza Laser / Tilt Laser") +
  theme_minimal()

plot_tilt_424 + scale_y_continuous(
  sec.axis = sec_axis(~./5, name = "Variable 2", breaks = seq(-5, 5, 1)))

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

