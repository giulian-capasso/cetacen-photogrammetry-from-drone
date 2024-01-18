#### IDEX: Statistica descrittiva ####
# N voli (LID-BAR): 75
# Durata media voli + istogramma durata voli. Categorie 3/6/8/10min etc
# Altezza media voli 
# Percentuale media di plateau di ogni volo 

# Percentuale media di copertura lidar per ogni volo:
# - Frequenza peaks lidar > n tot peaks / n voli totali 
# - Intensity: numero medio di picchi per ogni volo che ha almeno un picco 
# - Peak prevalence: frequenza dei voli con almeno un picco
# - Peak abundance : numero medio picchi per volo
# - Porzione media di volo che viene interessata da ogni picco 
# - Porzione media di volo che viene interessata da tutti i picchi (solo nei voli con i picchi)



#### 1) N voli (LID-BAR) ####
Flights <- c("FLY297_REC_MOT_LD_1.2", "FLY298_REC_MOT_LD_1.2", "FLY299_REC_MOT_LD_1.2", "FLY304_REC_MOT_LD_1.2", "FLY306_REC_MOT_LD_1.2", "FLY307_REC_MOT_LD_1.2", "FLY310_REC_MOT_LD_1.2", "FLY311_REC_MOT_LD_1.2", "FLY312_REC_MOT_LD_1.2", 
"FLY313_REC_MOT_LD_1.2", "FLY315_REC_MOT_LD_1.2", "FLY316_REC_MOT_LD_1.2", "FLY317_REC_MOT_LD_1.2", "FLY318_REC_MOT_LD_1.2", "FLY319_REC_MOT_LD_1.2", "FLY323_REC_MOT_LD_1.2", "FLY327_REC_MOT_LD_1.2", "FLY330_REC_MOT_LD_1.2", 
"FLY332_REC_MOT_LD_1.2", "FLY352_REC_MOT_LD_1.2", "FLY353_REC_MOT_LD_1.2", "FLY354_REC_MOT_LD_1.2", "FLY365_REC_MOT_LD_1.2", "FLY368_REC_MOT_LD_1.2_clean", "FLY371_REC_MOT_LD_1.2", "FLY372_REC_MOT_LD_1.2", "FLY374_REC_MOT_LD_1.2", 
"FLY382_REC_MOT_LD_1.2", "FLY383_REC_MOT_LD_1.2", "FLY384_REC_MOT_LD_1.2", "FLY387_REC_MOT_LD_1.2", "FLY388_REC_MOT_LD_1.2", "FLY389_REC_MOT_LD_1.2_clean", "FLY393_REC_MOT_LD_1.2", "FLY396_REC_MOT_LD_1.2_clean", "FLY400_REC_MOT_LD_1.2", 
"FLY401_REC_MOT_LD_1.2", "FLY402_REC_MOT_LD_1.2", "FLY411_REC_MOT_LD_1.2", "FLY412_REC_MOT_LD_1.2_clean", "FLY414_REC_MOT_LD_1.2", "FLY417_REC_MOT_LD_1.2", "FLY420_REC_MOT_LD_1.2_clean", "FLY421_REC_MOT_LD_1.2", "FLY423_REC_MOT_LD_1.2", 
"FLY424_REC_MOT_LD_1.2", "FLY425_REC_MOT_LD_1.2", "FLY427_REC_MOT_LD_1.2", "FLY429_REC_MOT_LD_1.2", "FLY430_REC_MOT_LD_1.2_clean", "FLY431_REC_MOT_LD_1.2", "FLY434_REC_MOT_LD_1.2", "FLY436_REC_MOT_LD_1.2", "FLY437_REC_MOT_LD_1.2", 
"FLY438_REC_MOT_LD_1.2", "FLY440_REC_MOT_LD_1.2", "FLY444_REC_MOT_LD_1.2", "FLY446_REC_MOT_LD_1.2", "FLY447_REC_MOT_LD_1.2", "FLY452_REC_MOT_LD_1.2", "FLY456_REC_MOT_LD_1.2", "FLY457_REC_MOT_LD_1.2", "FLY458_REC_MOT_LD_1.2", 
"FLY459_REC_MOT_LD_1.2", "FLY461_REC_MOT_LD_1.2", "FLY462_REC_MOT_LD_1.2", "FLY463_REC_MOT_LD_1.2", "FLY464_REC_MOT_LD_1.2", "FLY466_REC_MOT_LD_1.2_clean", "FLY472_REC_MOT_LD_1.2", "FLY475_REC_MOT_LD_1.2", "FLY477_REC_MOT_LD_1.2", "FLY478_REC_MOT_LD_1.2", 
"FLY484_REC_MOT_LD_1.2", "FLY487_REC_MOT_LD_1.2")

length(Flights)
# [1] 75

#### 2) Durata media voli + istogramma durata voli. Categorie 3/6/8/10min etc ####
length_297 <- length(df_FLY297$seconds_FLY297)
length_298 <- length(df_FLY298$seconds_FLY298)
length_299 <- length(df_FLY299$seconds_FLY299)
length_304 <- length(df_FLY304$seconds_FLY304)
length_306 <- length(df_FLY306$seconds_FLY306)
length_307 <- length(df_FLY307$seconds_FLY307)
length_310 <- length(df_FLY310$seconds_FLY310)
length_311 <- length(df_FLY311$seconds_FLY311)
length_312 <- length(df_FLY312$seconds_FLY312)
length_313 <- length(df_FLY313$seconds_FLY313)
length_315 <- length(df_FLY315$seconds_FLY315)
length_316 <- length(df_FLY316$seconds_FLY316)
length_317 <- length(df_FLY317$seconds_FLY317)
length_318 <- length(df_FLY318$seconds_FLY318)
length_319 <- length(df_FLY319$seconds_FLY319)
length_323 <- length(df_FLY323$seconds_FLY323)
length_327 <- length(df_FLY327$seconds_FLY327)
length_330 <- length(df_FLY330$seconds_FLY330)
length_332 <- length(df_FLY332$seconds_FLY332)
length_352 <- length(df_FLY352$seconds_FLY352)
length_353 <- length(df_FLY353$seconds_FLY353)
length_354 <- length(df_FLY354$seconds_FLY354)
length_365 <- length(df_FLY365$seconds_FLY365)
length_368 <- length(df_FLY368$seconds_FLY368)
length_371 <- length(df_FLY371$seconds_FLY371)
length_372 <- length(df_FLY372$seconds_FLY372)
length_374 <- length(df_FLY374$seconds_FLY374)
length_382 <- length(df_FLY382$seconds_FLY382)
length_383 <- length(df_FLY383$seconds_FLY383)
length_384 <- length(df_FLY384$seconds_FLY384)
length_387 <- length(df_FLY387$seconds_FLY387)
length_388 <- length(df_FLY388$seconds_FLY388)
length_389 <- length(df_FLY389$seconds_FLY389)
length_393 <- length(df_FLY393$seconds_FLY393)
length_396 <- length(df_FLY396$seconds_FLY396)
length_400 <- length(df_FLY400$seconds_FLY400)
length_401 <- length(df_FLY401$seconds_FLY401)
length_402 <- length(df_FLY402$seconds_FLY402)
length_411 <- length(df_FLY411$seconds_FLY411)
length_412 <- length(df_FLY412$seconds_FLY412)
length_414 <- length(df_FLY414$seconds_FLY414)
length_417 <- length(df_FLY417$seconds_FLY417)
length_420 <- length(df_FLY420$seconds_FLY420)
length_421 <- length(df_FLY421$seconds_FLY421)
length_423 <- length(df_FLY423$seconds_FLY423)
length_424 <- length(df_FLY424$seconds_FLY424)
length_425 <- length(df_FLY425$seconds_FLY425)
length_427 <- length(df_FLY427$seconds_FLY427)
length_429 <- length(df_FLY429$seconds_FLY429)
length_430 <- length(df_FLY430$seconds_FLY430)
length_431 <- length(df_FLY431$seconds_FLY431)
length_434 <- length(df_FLY434$seconds_FLY434)
length_436 <- length(df_FLY436$seconds_FLY436)
length_437 <- length(df_FLY437$seconds_FLY437)
length_438 <- length(df_FLY438$seconds_FLY438)
length_440 <- length(df_FLY440$seconds_FLY440)
length_444 <- length(df_FLY444$seconds_FLY444)
length_446 <- length(df_FLY446$seconds_FLY446)
length_447 <- length(df_FLY447$seconds_FLY447)
length_452 <- length(df_FLY452$seconds_FLY452)
length_456 <- length(df_FLY456$seconds_FLY456)
length_457 <- length(df_FLY457$seconds_FLY457)
length_458 <- length(df_FLY458$seconds_FLY458)
length_459 <- length(df_FLY459$seconds_FLY459)
length_461 <- length(df_FLY461$seconds_FLY461)
length_462 <- length(df_FLY462$seconds_FLY462)
length_463 <- length(df_FLY463$seconds_FLY463)
length_464 <- length(df_FLY464$seconds_FLY464)
length_466 <- length(df_FLY466$seconds_FLY466)
length_472 <- length(df_FLY472$seconds_FLY472)
length_475 <- length(df_FLY475$seconds_FLY475)
length_477 <- length(df_FLY477$seconds_FLY477)
length_478 <- length(df_FLY478$seconds_FLY478)
length_484 <- length(df_FLY484$seconds_FLY484)
length_487 <- length(df_FLY487$seconds_FLY487)

lengths <- c(length_297, length_298, length_299, length_304, length_306, length_307, length_310, length_311, length_312,
             length_313, length_315, length_316, length_317, length_318, length_319, length_323, length_327, length_330,
             length_332, length_352, length_353, length_354, length_365, length_368, length_371, length_372, length_374,
             length_382, length_383, length_384, length_387, length_388, length_389, length_393, length_396, length_400,
             length_401, length_402, length_411, length_412, length_414, length_417, length_420, length_421, length_423,
             length_424, length_425, length_427, length_429, length_430, length_431, length_434, length_436, length_437,
             length_438, length_440, length_444, length_446, length_447, length_452, length_456, length_457, length_458,
             length_459, length_461, length_462, length_463, length_464, length_466, length_472, length_475, length_477,
             length_478, length_484, length_487)

summary(lengths)
# MEAN lenght = 408.7 sec = 6.78 min
# SD lenght = 192.9973 sec = 3.22 min
# min lenght = 117.0 sec = 1.95 min
# max lenght = 774.0 sec = 12.90 min


# Lista di valori elencati, se servono
# lengths <- c(397, 531, 735, 774, 345, 658, 633, 507, 281, 239, 431, 207, 537, 228, 225, 271, 150, 768, 216, 325, 370, 477, 694, 607, 233, 172, 175, 186, 502, 558, 148, 117,
#             476, 259, 328, 219, 219, 230, 323, 293, 298, 701, 734, 533, 757, 250, 457, 633, 167, 288, 448, 510, 356, 360, 550, 729, 489, 226, 211, 369, 201, 222, 287, 621, 532,
#             209, 487, 669, 622, 680, 264, 243, 238, 710, 555)


# Creazione di un dataframe con i dati
data <- data.frame(lengths)

# Creazione dell'istogramma con ggplot2
ggplot(data, aes(x = lengths)) +
  geom_histogram(binwidth = 60, color = "white", fill = "skyblue", boundary = 0) +
  labs(title = "Durata Volo in secondi",
       x = "Secondi",
       y = "Frequenza") +
  scale_x_continuous(breaks = seq(0, 800, by = 60)) +
  theme_minimal()


#### 3) # Altezza media voli ####
Flights <- c("FLY297_REC_MOT_LD_1.2", "FLY298_REC_MOT_LD_1.2", "FLY299_REC_MOT_LD_1.2", "FLY304_REC_MOT_LD_1.2", "FLY306_REC_MOT_LD_1.2", "FLY307_REC_MOT_LD_1.2", "FLY310_REC_MOT_LD_1.2", "FLY311_REC_MOT_LD_1.2", "FLY312_REC_MOT_LD_1.2", 
             "FLY313_REC_MOT_LD_1.2", "FLY315_REC_MOT_LD_1.2", "FLY316_REC_MOT_LD_1.2", "FLY317_REC_MOT_LD_1.2", "FLY318_REC_MOT_LD_1.2", "FLY319_REC_MOT_LD_1.2", "FLY323_REC_MOT_LD_1.2", "FLY327_REC_MOT_LD_1.2", "FLY330_REC_MOT_LD_1.2", 
             "FLY332_REC_MOT_LD_1.2", "FLY352_REC_MOT_LD_1.2", "FLY353_REC_MOT_LD_1.2", "FLY354_REC_MOT_LD_1.2", "FLY365_REC_MOT_LD_1.2", "FLY368_REC_MOT_LD_1.2_clean", "FLY371_REC_MOT_LD_1.2", "FLY372_REC_MOT_LD_1.2", "FLY374_REC_MOT_LD_1.2", 
             "FLY382_REC_MOT_LD_1.2", "FLY383_REC_MOT_LD_1.2", "FLY384_REC_MOT_LD_1.2", "FLY387_REC_MOT_LD_1.2", "FLY388_REC_MOT_LD_1.2", "FLY389_REC_MOT_LD_1.2_clean", "FLY393_REC_MOT_LD_1.2", "FLY396_REC_MOT_LD_1.2_clean", "FLY400_REC_MOT_LD_1.2", 
             "FLY401_REC_MOT_LD_1.2", "FLY402_REC_MOT_LD_1.2", "FLY411_REC_MOT_LD_1.2", "FLY412_REC_MOT_LD_1.2_clean", "FLY414_REC_MOT_LD_1.2", "FLY417_REC_MOT_LD_1.2", "FLY420_REC_MOT_LD_1.2_clean", "FLY421_REC_MOT_LD_1.2", "FLY423_REC_MOT_LD_1.2", 
             "FLY424_REC_MOT_LD_1.2", "FLY425_REC_MOT_LD_1.2", "FLY427_REC_MOT_LD_1.2", "FLY429_REC_MOT_LD_1.2", "FLY430_REC_MOT_LD_1.2_clean", "FLY431_REC_MOT_LD_1.2", "FLY434_REC_MOT_LD_1.2", "FLY436_REC_MOT_LD_1.2", "FLY437_REC_MOT_LD_1.2", 
             "FLY438_REC_MOT_LD_1.2", "FLY440_REC_MOT_LD_1.2", "FLY444_REC_MOT_LD_1.2", "FLY446_REC_MOT_LD_1.2", "FLY447_REC_MOT_LD_1.2", "FLY452_REC_MOT_LD_1.2", "FLY456_REC_MOT_LD_1.2", "FLY457_REC_MOT_LD_1.2", "FLY458_REC_MOT_LD_1.2", 
             "FLY459_REC_MOT_LD_1.2", "FLY461_REC_MOT_LD_1.2", "FLY462_REC_MOT_LD_1.2", "FLY463_REC_MOT_LD_1.2", "FLY464_REC_MOT_LD_1.2", "FLY466_REC_MOT_LD_1.2_clean", "FLY472_REC_MOT_LD_1.2", "FLY475_REC_MOT_LD_1.2", "FLY477_REC_MOT_LD_1.2", "FLY478_REC_MOT_LD_1.2", 
             "FLY484_REC_MOT_LD_1.2", "FLY487_REC_MOT_LD_1.2")

# Funzione per calcolare la moda delle colonne specificate
custom_mode <- function(df) {
  moda_relativeHeight <- table(df$`osd_data:relativeHeight[meters]`)
  moda_altitude_cleaned <- table(df$laser_altitude_m_cleaned)
  
  # Trova la moda per `osd_data:relativeHeight[meters]`
  moda_values_relativeHeight <- as.numeric(names(moda_relativeHeight[moda_relativeHeight == max(moda_relativeHeight)]))
  
  # Trova la moda per `laser_m_altitude_cleaned`
  moda_values_altitude_cleaned <- as.numeric(names(moda_altitude_cleaned[moda_altitude_cleaned == max(moda_altitude_cleaned)]))
  
  risultato <- data.frame(
    Altezza_Moda_relativeHeight = moda_values_relativeHeight,
    Altezza_Moda_altitude_cleaned = moda_values_altitude_cleaned
  )
  
  return(risultato)
}

# Lista di dataframe associati a ciascun volo
lista_df <- list()

# Itera attraverso ciascun volo nella lista Flights
for (flight in Flights) {
  # Ottieni il dataframe associato al volo
  df <- get(flight)
  
  # Calcola la moda delle altezze
  moda_altezze <- custom_mode(df)
  
  # Salva il risultato nella lista
  risultato <- data.frame(Flight = flight, moda_altezze)
  lista_df <- c(lista_df, list(risultato))
}

# Unisci tutti i risultati in un unico dataframe
risultato_finale <- do.call(rbind, lista_df)
risultato_finale

mean(risultato_finale$Altezza_Moda_relativeHeight)
# media delle altezze piu frequenti (moda) baromentrico = 37.30732
as.numeric(names(table(risultato_finale$Altezza_Moda_relativeHeight)[table(risultato_finale$Altezza_Moda_relativeHeight) == max(table(risultato_finale$Altezza_Moda_relativeHeight))]))
# altezza barometrico piu frequnete  32.2, 40.9

mean(risultato_finale$Altezza_Moda_altitude_cleaned)
# media delle altezze piu frequenti (moda) lidar = 44.51024
as.numeric(names(table(risultato_finale$Altezza_Moda_altitude_cleaned)[table(risultato_finale$Altezza_Moda_altitude_cleaned) == max(table(risultato_finale$Altezza_Moda_altitude_cleaned))]))
# altezza lidar piu frequnete  52.13 55.08 >>> secondo me non affidabile perché non abbiamo abbastanza coprtura lidar. Megllio aggiungere la media dei GAP al barometrico


#### 4) Percentuale plateau per ogni volo ####
calcola_percentuale_plateau <- function(volo) {
  # Ottieni il dataframe associato al volo
  df <- get(volo)
  
  # Rimuovi i valori mancanti (NA) dalla colonna di interesse
  df_height <- df$`osd_data:relativeHeight[meters]`
  df_height <- df_height[!is.na(df_height)]
  
  # Inizializza variabili
  durata_totale <- length(df_height)
  durata_plateau <- 0
  plateau_attuale <- 0
  
  # Scorrere i dati
  for (i in 2:durata_totale) {
    # Verifica se c'è un cambiamento entro 1 metro
    cambio_altezza <- abs(df_height[i] - df_height[i - 1])
    
    if (cambio_altezza <= 1) {
      # Incrementa la durata specifica del plateau
      plateau_attuale <- plateau_attuale + 1
    }
    
    # Incrementa la durata totale del plateau indipendentemente dalla durata specifica
    durata_plateau <- durata_plateau + 1
  }
  
  # Calcola la percentuale di plateau rispetto alla durata totale
  percentuale_plateau <- (plateau_attuale / durata_plateau) * 100
  
  return(data.frame(Flight = volo, Percentuale_Plateau = percentuale_plateau))
}

# Applica la funzione a ciascun volo nella lista Flights
risultato_plateau <- lapply(Flights, calcola_percentuale_plateau)

# Unisci tutti i risultati in un unico dataframe
risultato_finale_plateau <- do.call(rbind, risultato_plateau)

# Visualizza il risultato finale
print(risultato_finale_plateau)

## controprova ## 

sum(abs(diff(FLY297_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`)) <= 1) / length(diff(FLY297_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`)) * 100
sum(abs(diff(FLY374_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`)) <= 1) / length(diff(FLY374_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`)) * 100

