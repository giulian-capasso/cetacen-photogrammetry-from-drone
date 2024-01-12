# Creazione di una nuova colonna all'interno del nostro dataset "FLY_" con i valori del tilt sottratti di 5 ovvero il suo zero e poi corretti i valori negativi in positivi

FLY304_REC_MOT_LD_1.2$tilt_corrected <- ifelse(FLY304_REC_MOT_LD_1.2$tilt_corrected < 0 & !is.na(FLY304_REC_MOT_LD_1.2$tilt_corrected),
                                               -FLY304_REC_MOT_LD_1.2$tilt_corrected, 
                                               FLY304_REC_MOT_LD_1.2$tilt_corrected)


correct_altitude <- function(dataset) {
  if (!("laser_altitude_m_cleaned" %in% colnames(dataset)) ||
      !("tilt_deg" %in% colnames(dataset))) {
    stop("Le colonne richieste non sono presenti nel dataset.")
  }
  
  dataset$laser_altitude_tilt_corrected <- ifelse(
    is.na(dataset$laser_altitude_m_cleaned) | is.na(dataset$tilt_deg),
    dataset$laser_altitude_m_cleaned,
    round(dataset$laser_altitude_m_cleaned * cos(dataset$tilt_deg * pi / 180), 2)
  )
  
  return(dataset)
}

FLY304_REC_MOT_LD_1.2 <- correct_altitude(FLY304_REC_MOT_LD_1.2)
