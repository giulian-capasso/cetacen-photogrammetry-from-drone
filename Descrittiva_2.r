# Descrittiva 2 

#### IDEX: Statistica descrittiva ####
# N voli (LID-BAR): 75
# Durata media voli + istogramma durata voli: 6min.... Categorie 3/6/8/10min etc
# Altezza media voli:  
# Percentuale media di plateau di ogni volo: [1] 89.5925

# Percentuale media di copertura lidar per ogni volo:
# - Frequenza peaks lidar > n tot peaks / n voli totali:  68% 
# - Intensity: numero medio di picchi per ogni volo che ha almeno un picco:  69.4
# - Peak prevalence: frequenza dei voli con almeno un picco:  98.6 %"
# - Peak abundance : numero medio picchi per volo: ????????? 
# - Porzione media di volo che viene interessata da ogni picco: ??????????
# - Porzione media di volo che viene interessata da tutti i picchi (solo nei voli con i picchi) ????????



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
length_297 <- length(FLY297_REC_MOT_LD_1.2$seconds)
length_298 <- length(FLY298_REC_MOT_LD_1.2$seconds)
length_299 <- length(FLY299_REC_MOT_LD_1.2$seconds)
length_304 <- length(FLY304_REC_MOT_LD_1.2$seconds)
length_306 <- length(FLY306_REC_MOT_LD_1.2$seconds)
length_307 <- length(FLY307_REC_MOT_LD_1.2$seconds)
length_310 <- length(FLY310_REC_MOT_LD_1.2$seconds)
length_311 <- length(FLY311_REC_MOT_LD_1.2$seconds)
length_312 <- length(FLY312_REC_MOT_LD_1.2$seconds)
length_313 <- length(FLY313_REC_MOT_LD_1.2$seconds)
length_315 <- length(FLY315_REC_MOT_LD_1.2$seconds)
length_316 <- length(FLY316_REC_MOT_LD_1.2$seconds)
length_317 <- length(FLY317_REC_MOT_LD_1.2$seconds)
length_318 <- length(FLY318_REC_MOT_LD_1.2$seconds)
length_319 <- length(FLY318_REC_MOT_LD_1.2$seconds)
length_323 <- length(FLY323_REC_MOT_LD_1.2$seconds)
length_327 <- length(FLY327_REC_MOT_LD_1.2$seconds)
length_330 <- length(FLY330_REC_MOT_LD_1.2$seconds)
length_332 <- length(FLY332_REC_MOT_LD_1.2$seconds)
length_352 <- length(FLY352_REC_MOT_LD_1.2$seconds)
length_353 <- length(FLY353_REC_MOT_LD_1.2$seconds)
length_354 <- length(FLY354_REC_MOT_LD_1.2$seconds)
length_365 <- length(FLY365_REC_MOT_LD_1.2$seconds)
length_368 <- length(FLY368_REC_MOT_LD_1.2_clean$seconds)
length_371 <- length(FLY371_REC_MOT_LD_1.2$seconds)
length_372 <- length(FLY372_REC_MOT_LD_1.2$seconds)
length_374 <- length(FLY374_REC_MOT_LD_1.2$seconds)
length_382 <- length(FLY382_REC_MOT_LD_1.2$seconds)
length_383 <- length(FLY383_REC_MOT_LD_1.2$seconds)
length_384 <- length(FLY384_REC_MOT_LD_1.2$seconds)
length_387 <- length(FLY387_REC_MOT_LD_1.2$seconds)
length_388 <- length(FLY388_REC_MOT_LD_1.2$seconds)
length_389 <- length(FLY389_REC_MOT_LD_1.2_clean$seconds)
length_393 <- length(FLY393_REC_MOT_LD_1.2$seconds)
length_396 <- length(FLY396_REC_MOT_LD_1.2_clean$seconds)
length_400 <- length(FLY400_REC_MOT_LD_1.2$seconds)
length_401 <- length(FLY401_REC_MOT_LD_1.2$seconds)
length_402 <- length(FLY402_REC_MOT_LD_1.2$seconds)
length_411 <- length(FLY411_REC_MOT_LD_1.2$seconds)
length_412 <- length(FLY412_REC_MOT_LD_1.2_clean$seconds)
length_414 <- length(FLY414_REC_MOT_LD_1.2$seconds)
length_417 <- length(FLY417_REC_MOT_LD_1.2$seconds)
length_420 <- length(FLY420_REC_MOT_LD_1.2_clean$seconds)
length_421 <- length(FLY421_REC_MOT_LD_1.2$seconds)
length_423 <- length(FLY423_REC_MOT_LD_1.2$seconds)
length_424 <- length(FLY424_REC_MOT_LD_1.2$seconds)
length_425 <- length(FLY425_REC_MOT_LD_1.2$seconds)
length_427 <- length(FLY427_REC_MOT_LD_1.2$seconds)
length_429 <- length(FLY429_REC_MOT_LD_1.2$seconds)
length_430 <- length(FLY430_REC_MOT_LD_1.2_clean$seconds)
length_431 <- length(FLY431_REC_MOT_LD_1.2$seconds)
length_434 <- length(FLY434_REC_MOT_LD_1.2$seconds)
length_436 <- length(FLY436_REC_MOT_LD_1.2$seconds)
length_437 <- length(FLY437_REC_MOT_LD_1.2$seconds)
length_438 <- length(FLY438_REC_MOT_LD_1.2$seconds)
length_440 <- length(FLY440_REC_MOT_LD_1.2$seconds)
length_444 <- length(FLY444_REC_MOT_LD_1.2$seconds)
length_446 <- length(FLY446_REC_MOT_LD_1.2$seconds)
length_447 <- length(FLY447_REC_MOT_LD_1.2$seconds)
length_452 <- length(FLY452_REC_MOT_LD_1.2$seconds)
length_456 <- length(FLY456_REC_MOT_LD_1.2$seconds)
length_457 <- length(FLY457_REC_MOT_LD_1.2$seconds)
length_458 <- length(FLY458_REC_MOT_LD_1.2$seconds)
length_459 <- length(FLY459_REC_MOT_LD_1.2$seconds)
length_461 <- length(FLY461_REC_MOT_LD_1.2$seconds)
length_462 <- length(FLY462_REC_MOT_LD_1.2$seconds)
length_463 <- length(FLY463_REC_MOT_LD_1.2$seconds)
length_464 <- length(FLY464_REC_MOT_LD_1.2$seconds)
length_466 <- length(FLY466_REC_MOT_LD_1.2_clean$seconds)
length_472 <- length(FLY472_REC_MOT_LD_1.2$seconds)
length_475 <- length(FLY475_REC_MOT_LD_1.2$seconds)
length_477 <- length(FLY477_REC_MOT_LD_1.2$seconds)
length_478 <- length(FLY478_REC_MOT_LD_1.2$seconds)
length_484 <- length(FLY484_REC_MOT_LD_1.2$seconds)
length_487 <- length(FLY487_REC_MOT_LD_1.2$seconds)

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
# mode length = 219 sec = 3.65 min

as.numeric(lapply(data_lengths, function(x) as.numeric(names(table(x))[which.max(table(x))])))
# Lista di valori elencati, se servono
lengths_in_values <- c(397, 531, 735, 774, 345, 658, 633, 507, 281, 239, 431, 207, 537, 228, 225, 271, 150, 768, 216, 325, 370, 477, 694, 607, 233, 172, 175, 186, 502, 558, 148, 117,
                       476, 259, 328, 219, 219, 230, 323, 293, 298, 701, 734, 533, 757, 250, 457, 633, 167, 288, 448, 510, 356, 360, 550, 729, 489, 226, 211, 369, 201, 222, 287, 621, 532,
                       209, 487, 669, 622, 680, 264, 243, 238, 710, 555)

as.numeric(names(table(lengths_in_values))[which.max(table(lengths_in_values))])

# Creazione di un dataframe con i dati
data_lengths <- data.frame(lengths)

# Creazione dell'istogramma con ggplot2
ggplot(data_lengths, aes(x = lengths)) +
  geom_histogram(binwidth = 100, color = "white", fill = "skyblue", boundary = 0) +
  labs(title = "Durata Volo in secondi",
       x = "Secondi",
       y = "Frequenza") +
  scale_x_continuous(breaks = seq(0, 800, by = 60)) +
  theme_minimal()

#### 3) # Altezza media voli ####
# BAROMETRICO # 
# Lista di dataframe associati a ciascun volo
lista_df <- list()

# Itera attraverso ciascun volo nella lista Flights
for (flight in Flights) {
  # Ottieni il dataframe associato al volo
  df <- get(flight)
  
  # Seleziona solo la colonna `osd_data:relativeHeight[meters]`
  subset_df <- df[, "osd_data:relativeHeight[meters]", drop = FALSE]
  
  # Aggiungi il risultato alla lista
  lista_df <- c(lista_df, list(subset_df))
}

# Unisci tutti i risultati in un unico dataframe
subset_finale <- do.call(rbind, lista_df)
subset_finale

mean(subset_finale$`osd_data:relativeHeight[meters]`, na.rm = TRUE) 
#36.25528
as.numeric(names(table(subset_finale$`osd_data:relativeHeight[meters]`)[table(subset_finale$`osd_data:relativeHeight[meters]`) == max(table(subset_finale$`osd_data:relativeHeight[meters]`))]))
# 49.9

# LIDAR #

# Lista di dataframe associati a ciascun volo
lista_df <- list()

# Itera attraverso ciascun volo nella lista Flights
for (flight in Flights) {
  # Ottieni il dataframe associato al volo
  df <- get(flight)
  
  # Seleziona solo la colonna `laser_altitude_m_cleaned`
  subset_df <- df[, "laser_altitude_m_cleaned", drop = FALSE]
  
  # Aggiungi il risultato alla lista
  lista_df <- c(lista_df, list(subset_df))
}

# Unisci tutti i risultati in un unico dataframe
subset_finale <- do.call(rbind, lista_df)
subset_finale

mean(subset_finale$laser_altitude_m_cleaned, na.rm = TRUE)
# 41.48988
as.numeric(names(table(subset_finale$laser_altitude_m_cleaned)[table(subset_finale$laser_altitude_m_cleaned) == max(table(subset_finale$laser_altitude_m_cleaned))]))
# 55.11


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

## plot ##

# Crea un dataframe con i risultati
risultati_df <- data.frame(
  Flight = risultato_finale_plateau$Flight,
  Percentuale_Plateau = risultato_finale_plateau$Percentuale_Plateau
)

# Rimuovi il testo indesiderato nei nomi dei voli
risultato_finale_plateau$Flight <- sub("FLY(\\d+)_REC_MOT_LD_1\\.2", "\\1", risultato_finale_plateau$Flight)
risultato_finale_plateau$Flight <- sub("_clean$", "", risultato_finale_plateau$Flight)

# Calcola la media delle percentuali di plateau
media_percentuale_plateau <- mean(risultato_finale_plateau$Percentuale_Plateau)
# [1] 89.5925
# Crea il grafico a barre con la media
ggplot(risultato_finale_plateau, aes(x = Flight, y = Percentuale_Plateau)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = media_percentuale_plateau, linetype = "solid", color = "black") +
  theme_minimal() +
  labs(
    title = "Percentuale di Plateau per Volo",
    x = "Volo",
    y = "Percentuale Plateau"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### 5.1) Frequenza peaks lidar = n tot peaks / n voli totali ####
# Per ogni volo, devi calcolare la frequenza di malfunzionamenti in una colonna specifica, $laser_altitude_m. U
# n malfunzionamento è definito come un valore uguale a 130 o un valore minore di 2, 
# ma rilevato solo dopo il primo valore maggiore di 2.

#  Identificare i malfunzionamenti nella colonna $laser_altitude_m secondo le specifiche fornite.
# Calcolare il numero totale di malfunzionamenti per ciascun volo.
# Calcolare la frequenza di malfunzionamenti per ciascun volo, calcolare il numero totale di malfunzionamenti di tutti i voli e dividerlo per il totale di voli.
# Applicare la procedura a tutti i tuoi voli.

# Definisci la funzione per il calcolo dei picchi
calcola_picchi <- function(df) {
  # Consideriamo solo la colonna di interesse
  colonna_interesse <- df$laser_altitude_m
  
  # Inizializziamo il contatore di picchi
  picchi <- 0
  
  # Flag per indicare se abbiamo superato il primo valore maggiore di 2
  superato_primo_valore <- FALSE
  
  # Scorrere la colonna (escludendo le ultime 10 righe)
  for (i in 1:(length(colonna_interesse) - 10)) {
    valore <- colonna_interesse[i]
    
    if (!is.na(valore)) {
      # Verifica se il valore è un picco
      if (valore == 130 || (valore < 2 && superato_primo_valore)) {
        picchi <- picchi + 1
      }
      
      # Se il valore supera 2, impostiamo il flag
      if (valore > 2) {
        superato_primo_valore <- TRUE
      }
    }
  }
  
  return(picchi)
}

# Inizializza il totale dei picchi
totale_picchi_freq <- 0

# Itera su tutti i voli
for (volo in Flights) {
  # Applica la funzione e somma i picchi
  picchi_volo <- calcola_picchi(get(volo))
  totale_picchi_freq <- totale_picchi_freq + picchi_volo
}

# Calcola la media dei picchi per volo
frequenza_picchi <- totale_picchi_freq / length(Flights)

# Visualizza i risultati
print(paste("Totale Picchi:", totale_picchi_freq)) # 5137
print(paste("Numero totale di voli:", length(Flights))) # 75
print(paste("Frequenza Picchi:", frequenza_picchi)) # 68%

calcola_picchi(get(Flights[1]))




