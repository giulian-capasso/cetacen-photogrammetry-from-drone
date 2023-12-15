#Lidar spikes cleaning > substitute with baro + median btw (lidar - baro)

lidar_filter3 <- function(lidar, baro, window_size, lower_limit, upper_limit, omit_first_n = 0, omit_last_n = 0) {
  result <- lidar
  for (i in 1:length(lidar)) {
    # Omit borders, including the first and last n values
    if (!is.na(lidar[i]) && i > omit_first_n && i <= (length(lidar) - omit_last_n)) {
      start <- max(1, i - window_size)
      end <- min(length(lidar), i + window_size)
      
      # Calcola la differenza tra il valore Lidar e il valore corrispondente nel barometrico
      differences <- lidar[start:end] - baro[start:end]
      
      # Applica la correzione SOLO se il valore Lidar è maggiore di 60 o minore di 2
      if (lidar[i] > 60 || lidar[i] < 2) {
        # Calcola la mediana della differenza escludendo gli outliers
        valid_differences <- differences[!is.na(differences) & differences >= lower_limit & differences <= upper_limit]
        median_difference <- median(valid_differences)
        result[i] <- baro[i] + median_difference
      }
    }
  }
  return(result)
}

FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned <- lidar_filter3(
  FLY421_REC_MOT_LD_1.2$laser_altitude_m,
  FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`,
  window_size = 40,
  lower_limit = 30,
  upper_limit = 60,
  omit_first_n = 10,
  omit_last_n = 5
)

# Fill Lidar cleaned (meaning resulting NAs) with baro data + median (lidar - baro)
fill_wB <- function(lidar_col, baro_col, window_size, omit_first_n = 0, omit_last_n = 0) {
  # Calcola la differenza tra lidar_col e baro_col
  differences <- lidar_col - baro_col
  
  # Calcola la mediana della differenza escludendo gli outliers
  valid_differences <- differences[!is.na(differences)]
  median_BL <- median(valid_differences)
  
  # Sostituisci i valori NA di lidar_col con quelli di baro_col + median_BL
  for (i in which(is.na(lidar_col))) {
    indici_prepost <- seq(i - 5, i + 5)
    
    # Rimuovi gli indici che sono fuori dai limiti della colonna
    indici_prepost <- indici_prepost[indici_prepost > (omit_first_n) & indici_prepost <= (length(lidar_col) - omit_last_n)]
    
    # Se ci sono abbastanza valori non NA, sostituisci il NA con il valore corrispondente di baro_col + median_BL
    if (length(na.omit(lidar_col[indici_prepost])) >= 5) {
      lidar_col[i] <- baro_col[i] + median_BL
    }
  }
  
  return(lidar_col)
}

FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned3 <- fill_wB(
  FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned3,
  FLY421_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`,
  window_size = 15,
  omit_first_n = 10,
  omit_last_n = 5
)

# Lidar filter senza baro
# Definisci la funzione lidar_filter_with_omission
lidar_filter_with_omission <- function(x, window_size, lower_limit, upper_limit, omit_first_n = 0, omit_last_n = 0) {
  result <- x
  for (i in 1:length(x)) {
    # Omit borders, including the first and last n values
    if (!is.na(x[i]) && i > omit_first_n && i <= (length(x) - omit_last_n)) {
      start <- max(1, i - window_size)
      end <- min(length(x), i + window_size)

      # Applica la correzione SOLO se il valore Lidar è maggiore di 60 o minore di 2
      if (x[i] > 60 || x[i] < 2) {
        result[i] <- NA
      }
    }
  }
  return(result)
}

# Ora puoi utilizzare lidar_filter_with_omission nel tuo caso specifico
FLY423_REC_MOT_LD_1.2$laser_altitude_m_cleaned <- lidar_filter_with_omission(
  FLY423_REC_MOT_LD_1.2$laser_altitude_m,
  window_size = 1,
  lower_limit = 0,
  upper_limit = 80,
  omit_first_n = 0,
  omit_last_n = 0
)

