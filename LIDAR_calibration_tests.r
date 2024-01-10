# Prove calibrazione x Dicembre, su scrivania, misura reale 10 (da lente) 15,5(da velcro)
# misurata tra 7 e 8 cm

# Prove calibrazione 13 Dicembre, da finestra, misura reale 5.45m ----- RIFARE!!!!!!!!!!!!!!!!!!!!!!!
LOG_0186 <- read_table2("Desktop/LOG_0186.CSV", skip = 2)
LOG_0186$laser_altitude_cm
ggplot(LOG_0186) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylim(0, NA)

# Prove calibrazione 13 Dicembre in Stanza, misura reale 5.64m
LOG_0190 <- read_table2("Desktop/LOG_0190.CSV", skip = 2)
LOG_0190$laser_altitude_cm
ggplot(LOG_0190) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylim(0, NA)

# Prove calibrazione 13 Dicembre da Palazzo a Palazzo misura reale 19.9/20m
LOG_0191 <- read_table2("Desktop/LOG_0191.CSV", skip = 2)
LOG_0191$laser_altitude_cm
LOG_0191$tilt_deg
ggplot(LOG_0191) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  ylim(0, NA)

# Prove calibrazione 13 Dicembre da sedia, misura reale 61,5 (da lente) 67(da velcro)
LOG_0192 <- read_table2("Desktop/LOG_0192.CSV", skip = 2) 
LOG_0192$laser_altitude_cm

mean(LOG_0191$laser_altitude_cm)
median(LOG_0191$laser_altitude_cm)
as.numeric(names(which.max(table(LOG_0191$laser_altitude_cm))))

ggplot(LOG_0192) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Prove calibrazione 13 Dicembre, parete corta su rosso e su bianco, misura reale 1,36m
LOG_0193 <- read_table2("Desktop/LOG_0193.CSV", skip = 2) 
LOG_0193$laser_altitude_cm

ggplot(LOG_0193) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Prove calibrazione 13 Dicembre, corridoio buio da porta a porta, misura reale 9,95m
LOG_0199 <- read_table2("Desktop/LOG_0199.CSV", skip = 2) 
LOG_0199$laser_altitude_cm

ggplot(LOG_0199) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))


# Prova calibrazione 14 Dicembre, palazzo esterno in orizzontale, 40m + muretto 6cm con contrasti di luce 
LOG_0200 <- read_table2("prove_calibrazione_LOG/LOG_0200.CSV", skip = 2)
LOG_0200$laser_altitude_cm

ggplot(LOG_0200) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))


# Prova calibrazione 14 Dicembre, palazzo esterno in orizzontale, ombra 35,5m - 30,5 - 20 - 10 - 5 - 2,5- 1
LOG_0201 <- read_table2("prove_calibrazione_LOG/LOG_0201.CSV", skip = 2)
LOG_0201$laser_altitude_cm

ggplot(LOG_0201) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
   theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))


LOG_0202 <- read_table2("prove_calibrazione_LOG/LOG_0202.CSV", skip = 2)
LOG_0202$laser_altitude_cm
ggplot(LOG_0202) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Prova calibrazione 14 Dicembre, su tetto stanza per vedere eventuale interferenza con vibrazioni 
# anche se toccato, nessuna interferenza

LOG_0203 <- read_table2("prove_calibrazione_LOG/LOG_0203.CSV", skip = 2)
LOG_0203$laser_altitude_cm
ggplot(LOG_0203) +
  geom_line(aes(x = gmt_time, y = laser_altitude_cm, color = "Height", group = 1), size = 0.7, linetype = "solid") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
    axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("Height" = "darkorange", "Tilt" = "black")) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Prove di Volo 13 Dicembre LOG + FLY 

#### Tilt correction funtion ####

# Definizione della funzione `correct_altitude` che corregge l'altezza misurata in base all'angolo di tilt
correct_altitude <- function(dataset) {
  # Verifica se le colonne richieste sono presenti nel dataset
  if (!("laser_altitude_m_cleaned" %in% colnames(dataset)) ||
      !("tilt_deg" %in% colnames(dataset))) {
    stop("Le colonne richieste non sono presenti nel dataset.")
  }

  # Crea e calcola la colonna corretta `laser_altitude_tilt_corrected`
  # L'equazione viene applicata solo ai valori di angolo superiori a 5 gradi(visto che lo zero del tilt è a 5° ma non registra dati in negativo, quindi per tutto ciò che è onferiore di 5 è meglio non correggere) 
  # e ai valori non NA nella colonna laser_altitude_m_cleaned poi il risultato è arrotondato a due decimali
  dataset$laser_altitude_tilt_corrected <- ifelse(
    is.na(dataset$laser_altitude_m_cleaned) | dataset$tilt_deg <= 5,
    dataset$laser_altitude_m_cleaned,
    round(dataset$laser_altitude_m_cleaned * cos((dataset$tilt_deg - 5) * pi / 180), 2)
  )

  # Restituisci il dataset modificato
  return(dataset)
}

# Utilizzo della funzione con il tuo dataset
FLY484_REC_MOT_LD_1.2 <- correct_altitude(FLY484_REC_MOT_LD_1.2)


