#### ----------------------------------- LIDAR|BAR GAP ------------------------------------- ####
# ---------------------------------------- Function ------------------------------------------- #

GAP <- function(data, col1, col2, start_index, end_index) {
  # Estrai le colonne di interesse nel range specificato
  subset_data <- data.frame(
    col1 = data[[col1]][start_index:end_index],
    col2 = data[[col2]][start_index:end_index]
  )
  
  # Calcola la differenza riga per riga
  differences <- subset_data$col1 - subset_data$col2
  
  # Rimuovi le differenze con NA
  valid_differences <- differences[!is.na(differences)]
  
  # Calcola media, moda e mediana delle differenze
  mean_diff <- mean(valid_differences, na.rm = TRUE)
  
  # Calcola la moda solo se ci sono differenze valide
  mode_diff <- ifelse(length(valid_differences) > 0, as.numeric(names(table(valid_differences))[which.max(table(valid_differences))]), NA)
  
  median_diff <- median(valid_differences, na.rm = TRUE)
  
  # Restituisci i risultati
  return(list(mean_diff = mean_diff, mode_diff = mode_diff, median_diff = median_diff))
}

# Esempio di utilizzo della funzione
GAP(FLY297_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 287, 300)

#### ----------------------------------- Analysis ------------------------------------- ####


# FLY 297 + LOG0027 
FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_297
GAP_297 <- GAP(FLY297_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 287, 300)
GAP_297

# FLY 298 + LOG0028
FLY298_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_298
GAP_298 <- GAP(FLY298_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 550)
GAP_298

# FLY 299 + LOG0029
FLY299_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_299
GAP_299 <- GAP(FLY299_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 65, 680)
GAP_299

# FLY 304 + LOG0030
FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_304
GAP_304 <- GAP(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 265, 925)
GAP_304

# FLY 306 + LOG0031
FLY306_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_306
GAP_306 <- GAP(FLY306_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 61, 340)
GAP_306

# FLY 307 + LOG0032
FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_307
GAP_307 <- GAP(FLY307_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 620)
GAP_307

# FLY 310 + LOG0034
FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_310
GAP_310 <- GAP(FLY310_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 600)
GAP_310

# FLY 311 + LOG0035
FLY311_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_311
GAP_311 <- GAP(FLY311_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 470)
GAP_311

# FLY 312 + LOG0036
FLY312_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_312
GAP_312 <- GAP(FLY312_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 85, 225)
GAP_312

# FLY 313 + LOG0037 # errore su moda
FLY313_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_313
GAP_313 <- GAP(FLY313_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 280)
GAP_313

# FLY 315 + LOG0038
FLY315_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_315
GAP_315 <- GAP(FLY315_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 170, 435)
GAP_315

# FLY 316 + LOG0039 decollo laser 1.92m
FLY316_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_316
GAP_316 <- GAP(FLY316_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 125, 195)
GAP_316

# FLY 317 + LOG0040
FLY317_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_317
GAP_317 <- GAP(FLY317_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 430)
GAP_317

# FLY 318 + LOG0041 (decollo da 3.125BAR e 7.025833LASER) ( se sottraiamo 3 alla mediana di 8 torniamo a 5)
FLY318_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_318
GAP_318 <- GAP(FLY318_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 55, 200)
GAP_318

# FLY 319 + LOG0042
FLY319_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_319
GAP_319 <- GAP(FLY319_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 156, 290)
GAP_319

# FLY 323 + LOG0045
FLY323_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_323
GAP_323 <- GAP(FLY323_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 345, 440)
GAP_323

# FLY 327 + LOG0048
FLY327_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_327
GAP_327 <- GAP(FLY327_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 86, 140)
GAP_327

# FLY 330 + LOG0050
FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_330
GAP_330 <- GAP(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 750)
GAP_330

# FLY 332 + LOG0052
FLY332_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_332
GAP_330 <- GAP(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 190, 325)
GAP_330

# FLY 352 + LOG0068
FLY352_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_352
GAP_352 <- GAP(FLY352_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 390, 525)
GAP_352

# FLY 353 + LOG0069
FLY353_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_353
GAP_353 <- GAP(FLY353_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 400)
GAP_353


