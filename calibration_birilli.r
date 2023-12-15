# Prova volo con BIRILLI misure note + Lidar giorno 13 Dicembre 
FLY_503 <- read_csv("prove_calibrazione_drone/FLY503.csv")

# ---------------------------------------------- FLY_504 + LOG0187 ------------------------------------------ #
# -------------------------------------------- 2023-12-13 12:36:53 ------------------------------------------ #
# ------------------------- volo 1, decollo e atterraggio a terra, + tilt su palazzo ------------------------ #

FLY_504 <- read_csv("prove_calibrazione_drone/FLY504.csv")
# Import log
LOG_0187 <- read_table2("prove_calibrazione_LOG/LOG_0187.CSV", skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY504_REC_MOT_clean <- distinct(FLY_504, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY504_REC_MOT_ready <- FLY504_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0187_R <- transform(LOG_0187, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0187_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0187_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY504_REC_MOT_LD <- left_join(FLY504_REC_MOT_ready, LOG_0187_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY504_REC_MOT_LD_1 <- FLY504_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY504_REC_MOT_LD_1.2 <- FLY504_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_504 <- ggplot(FLY504_REC_MOT_LD_1.2 ) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.5, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.5, linetype = "solid") +
  labs(title = "2023-12-13 12:36:53 (FLY_504 LOG 197)\n(volo 1, decollo e atterraggio a terra, + tilt su palazzo)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 8, face = "bold"))
plot_504

# ---------------------------------------------- FLY_505 + LOG0188 ------------------------------------------ #
# -------------------------------------------- 2023-12-13 12:36:53 ------------------------------------------ #
# --------------------------------volo 2e3 , decollo in mano, atterraggio a terra ------------------------------ #

FLY_505 <- read_csv("prove_calibrazione_drone/FLY505.csv")
# Import log
LOG_0188 <- read_table2("prove_calibrazione_LOG/LOG_0188.CSV", skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY505_REC_MOT_clean <- distinct(FLY_505, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY505_REC_MOT_ready <- FLY505_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0188_R <- transform(LOG_0188, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0188_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0188_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY505_REC_MOT_LD <- left_join(FLY505_REC_MOT_ready, LOG_0188_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY505_REC_MOT_LD_1 <- FLY505_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY505_REC_MOT_LD_1.2 <- FLY505_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_505 <- ggplot(FLY505_REC_MOT_LD_1.2 ) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.5, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.5, linetype = "solid") +
  labs(title = "2023-12-13 12:48:57 (FLY_504 LOG 197)\n(volo 2 e 3)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 8, face = "bold"))
plot_505

# ---------------------------------------------- FLY_506 + LOG0189 ------------------------------------------ #
# -------------------------------------------- 2023-12-13 12:59:20 ------------------------------------------ #
# --------------------------------volo 4 ------------------------------ #


FLY_506 <- read_csv("prove_calibrazione_drone/FLY506.csv")
# Import log
LOG_0189 <- read_table2("prove_calibrazione_LOG/LOG_0189.CSV", skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY506_REC_MOT_clean <- distinct(FLY_506, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY506_REC_MOT_ready <- FLY506_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0189_R <- transform(LOG_0189, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0189_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0189_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY506_REC_MOT_LD <- left_join(FLY506_REC_MOT_ready, LOG_0189_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY506_REC_MOT_LD_1 <- FLY506_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY506_REC_MOT_LD_1.2 <- FLY506_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY506_REC_MOT_LD_1.2_clean <- subset(FLY506_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Plot 
plot_506 <- ggplot(FLY506_REC_MOT_LD_1.2_clean ) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.5, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.5, linetype = "solid") +
  labs(title = "2023-12-13 12:48:57 (FLY_504 LOG 197) \n volo 4", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 8, face = "bold"))
plot_506
FLY506_REC_MOT_LD_1.2_clean$laser_altitude_m


par(mfrow=c(1,2))
plot_504 + plot_505 + plot_506

