library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hms)

# PROCESS 

# Import CSV
FLYXXX_REC_MOT <- read_csv("Desktop/x/x/FLYXXX_REC_MOT.csv")

# Import LOG With frist 2 columns deleted and new names 
LOG_00XX <- read_table2("Desktop/x/x/LOG_00XX.CSV", 
                        skip = 2)

# Clean repetitions in `GPS:dateTimeStamp`
FLYXXX_REC_MOT_clean <- distinct(FLYXXX_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)

# Reneme FLY "`GPS:dateTimeStamp`" name 
FLYXXX_REC_MOT_ready <- FLYXXX_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)

# Merge LOG Date and Time 
LOG_00XX_R <- transform(LOG_00XX, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))

# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_00XX_R$GPS.dateTimeStamp <- as.POSIXct(LOG_00XX_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Join FLY and LOG columns by GPS.dateTimeStamp
FLYXXX_REC_MOT_LD <- left_join(FLYXXX_REC_MOT_ready, LOG_00XX_R, by = "GPS.dateTimeStamp")

# Laser data from cm to m
FLYXXX_REC_MOT_LD_1 <- FLYXXX_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)

# Rename to meters
FLYXXX_REC_MOT_LD_1.2 <- FLYXXX_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)

# Plot 
plot_XXX <- ggplot(FLYXXX_REC_MOT_ready) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "*date* *time* (FLY_XXX)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_XXX


# Upload Series 

# ---------------------------------------------- FLY_263 + NOLOG--------------------------------------------- #
# -------------------------------------------- 2020-09-29 14:10:01 ------------------------------------------ #

# Import csv
FLY263_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY263_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY263_REC_MOT_clean <- distinct(FLY263_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY263_REC_MOT_ready <- FLY263_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_263 <- ggplot(FLY263_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2020-09-29 14:10:01 (FLY_263)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_263


# ---------------------------------------------- FLY_264 + NOLOG--------------------------------------------- #
# -------------------------------------------- 2020-09-29 14:10:01 ------------------------------------------ #

# Import csv
FLY264_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY264_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY264_REC_MOT_clean <- distinct(FLY264_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY264_REC_MOT_ready <- FLY264_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_264 <- ggplot(FLY264_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2020-09-29 (FLY_264)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_264

# ---------------------------------------------- FLY_266 + NOLOG -------------------------------------------- #
# -------------------------------------------- 2021-02-11 11:13:53 ------------------------------------------ #

# Import csv
FLY269_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY269_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY269_REC_MOT_clean <- distinct(FLY269_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY269_REC_MOT_ready <- FLY269_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_269 <- ggplot(FLY269_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2021-02-11 11:13:53 (FLY_269)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_269

# ---------------------------------------------- FLY_270 + NOLOG -------------------------------------------- #
# -------------------------------------------- 2021-02-11 11:29:06 ------------------------------------------ #

# Import csv
FLY270_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY270_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY270_REC_MOT_clean <- distinct(FLY270_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY270_REC_MOT_ready <- FLY270_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_270 <- ggplot(FLY270_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2021-02-11 11:29:06 (FLY_270)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_270

# ---------------------------------------------- FLY_285 + LOG0021 ------------------------------------------ #
# -------------------------------------------- 2021-04-22 17:10:01 ------------------------------------------ #

# Import csv
FLY285_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY285_REC_MOT.csv")
# Import log
LOG_0021 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0021.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY285_REC_MOT_clean <- distinct(FLY285_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY285_REC_MOT_ready <- FLY285_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0021_R <- transform(LOG_0021, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0021_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0021_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY285_REC_MOT_LD <- left_join(FLY285_REC_MOT_ready, LOG_0021_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY285_REC_MOT_LD_1 <- FLY285_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY285_REC_MOT_LD_1.2 <- FLY285_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_285 <- ggplot(FLY285_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2021-04-22 17:10:01 (FLY_285)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_285

# ---------------------------------------------- FLY_288 + LOG0022 ------------------------------------------ #
# -------------------------------------------- 2021-12-01 13:23:01 ------------------------------------------ #

# Import csv
FLY288_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY288_REC_MOT.csv")
# Import log
LOG_0022 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0022.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY288_REC_MOT_clean <- distinct(FLY288_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY288_REC_MOT_ready <- FLY288_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0022_R <- transform(LOG_0022, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0022_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0022_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY288_REC_MOT_LD <- left_join(FLY288_REC_MOT_ready, LOG_0022_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY288_REC_MOT_LD_1 <- FLY288_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY288_REC_MOT_LD_1.2 <- FLY288_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_288 <- ggplot(FLY288_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2021-12-01 13:23:01 (FLY_288)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_288

# ---------------------------------------------- FLY_289 + NOLOG ------------------------------------------ #
# --------------------------------------------- 2021-12-01 13:42:45 ----------------------------------------- #

# Import csv
FLY289_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY289_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY289_REC_MOT_clean <- distinct(FLY289_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY289_REC_MOT_ready <- FLY289_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_289 <- ggplot(FLY289_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2021-12-01 13:42:45 (FLY_289)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_289

# ---------------------------------------------- FLY_296 + NOLOG ------------------------------------------ #
# -------------------------------------------- 2022-06-11 07:52:02 ------------------------------------------ #

# Import csv
FLY296_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY296_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY296_REC_MOT_clean <- distinct(FLY296_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY296_REC_MOT_ready <- FLY296_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_296 <- ggplot(FLY296_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-11 07:52:02 (FLY_296)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_296

# --------------------------------------------- FLY_297 + LOG0027 ------------------------------------------ #
# -------------------------------------------- 2022-06-12 14:49:31 ----------------------------------------- #

# Import csv
FLY297_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY297_REC_MOT.csv")
# Import log
LOG_0027 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0027.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY297_REC_MOT_clean <- distinct(FLY297_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY297_REC_MOT_ready <- FLY297_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0027_R <- transform(LOG_0027, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0027_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0027_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY297_REC_MOT_LD <- left_join(FLY297_REC_MOT_ready, LOG_0027_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY297_REC_MOT_LD_1 <- FLY297_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY297_REC_MOT_LD_1.2 <- FLY297_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_297 <- ggplot(FLY297_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-12 14:49:31 (FLY_297)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_297

# --------------------------------------------- FLY_298 + LOG0028 ------------------------------------------ #
# -------------------------------------------- 2022-06-16 06:19:01 ----------------------------------------- #

# Import csv
FLY298_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY298_REC_MOT.csv")
# Import log
LOG_0028 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0028.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY298_REC_MOT_clean <- distinct(FLY298_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY298_REC_MOT_ready <- FLY298_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0028_R <- transform(LOG_0028, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0028_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0028_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY298_REC_MOT_LD <- left_join(FLY298_REC_MOT_ready, LOG_0028_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY298_REC_MOT_LD_1 <- FLY298_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY298_REC_MOT_LD_1.2 <- FLY298_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_298 <- ggplot(FLY298_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-16 06:19:01 (FLY_298)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_298

# --------------------------------------------- FLY_299 + LOG0029 ------------------------------------------ #
# -------------------------------------------- 2022-06-16 07:20:01 ----------------------------------------- #

# Import csv
FLY299_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY299_REC_MOT.csv")
# Import log
LOG_0029 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0029.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY299_REC_MOT_clean <- distinct(FLY299_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY299_REC_MOT_ready <- FLY299_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0029_R <- transform(LOG_0029, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0029_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0029_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY299_REC_MOT_LD <- left_join(FLY299_REC_MOT_ready, LOG_0029_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY299_REC_MOT_LD_1 <- FLY299_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY299_REC_MOT_LD_1.2 <- FLY299_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_299 <- ggplot(FLY299_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-16 07:20:01 (FLY_299)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_299

# --------------------------------------------- FLY_304 + LOG_0030 ----------------------------------------------- #
# -------------------------------------------- 2022-06-30 08:15:31 ----------------------------------------------- #

# Import CSV 
FLY304_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY304_REC_MOT.csv")
# Import LOG 
LOG_0030 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0030.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY304_REC_MOT_clean <- distinct(FLY304_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY304_REC_MOT_ready <- FLY304_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0030_R <- transform(LOG_0030, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0030_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0030_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY304_REC_MOT_LD <- left_join(FLY304_REC_MOT_ready, LOG_0030_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY304_REC_MOT_LD_1 <- FLY304_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY304_REC_MOT_LD_1.2 <- FLY304_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_304 <- ggplot(FLY304_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-30 08:15:31 (FLY_304)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_304


# --------------------------------------------- FLY_306 + LOG_0031 ----------------------------------------------- #
# -------------------------------------------- 2022-06-30 14:05:05 ----------------------------------------------- #

FLY306_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY306_REC_MOT.csv")
# Import LOG 
LOG_0031 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0031.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY306_REC_MOT_clean <- distinct(FLY306_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY306_REC_MOT_ready <- FLY306_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0031_R <- transform(LOG_0031, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0031_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0031_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY306_REC_MOT_LD <- left_join(FLY306_REC_MOT_ready, LOG_0031_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY306_REC_MOT_LD_1 <- FLY306_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY306_REC_MOT_LD_1.2 <- FLY306_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_306 <- ggplot(FLY306_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-30 14:05:05 (FLY_306)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_306


# ---------------------------------------------- FLY_307 + LOG_0032--------------------------------------------- #
# --------------------------------------------- 2022-06-30 15:14:31 -------------------------------------------- #

FLY307_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY307_REC_MOT.csv")
# Import LOG 
LOG_0032 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0032.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY307_REC_MOT_clean <- distinct(FLY307_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY307_REC_MOT_ready <- FLY307_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0032_R <- transform(LOG_0032, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0032_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0032_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY307_REC_MOT_LD <- left_join(FLY307_REC_MOT_ready, LOG_0032_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY307_REC_MOT_LD_1 <- FLY307_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY307_REC_MOT_LD_1.2 <- FLY307_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
# Plot 
plot_307 <- ggplot(FLY307_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-06-30 15:14:31 (FLY_307)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_307

# ---------------------------------------------- FLY_310 + LOG_0034--------------------------------------------- #
# --------------------------------------------- 2022-07-10 11:17:31 -------------------------------------------- #

# Import csv
FLY310_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY310_REC_MOT.csv")
# Import LOG 
LOG_0034 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0034.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY310_REC_MOT_clean <- distinct(FLY310_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY310_REC_MOT_ready <- FLY310_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0034_R <- transform(LOG_0034, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0034_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0034_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY310_REC_MOT_LD <- left_join(FLY310_REC_MOT_ready, LOG_0034_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY310_REC_MOT_LD_1 <- FLY310_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY310_REC_MOT_LD_1.2 <- FLY310_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_310 <- ggplot(FLY310_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-10 11:17:31 (FLY_310)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_310

# ---------------------------------------------- FLY_311 + LOG_0035--------------------------------------------- #
# --------------------------------------------- 2022-07-10 12:34:10 -------------------------------------------- #

# Import csv
FLY311_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY311_REC_MOT.csv")
# Import LOG 
LOG_0035 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0035.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY311_REC_MOT_clean <- distinct(FLY311_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY311_REC_MOT_ready <- FLY311_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0035_R <- transform(LOG_0035, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0035_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0035_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY311_REC_MOT_LD <- left_join(FLY311_REC_MOT_ready, LOG_0035_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY311_REC_MOT_LD_1 <- FLY311_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY311_REC_MOT_LD_1.2 <- FLY311_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_311 <- ggplot(FLY311_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-10 12:34:10 (FLY_311)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_311

# ---------------------------------------------- FLY_312 + LOG_0036--------------------------------------------- #
# --------------------------------------------- 2022-07-12 08:54:10 -------------------------------------------- #

# Import csv
FLY312_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY312_REC_MOT.csv")
# Import LOG 
LOG_0036 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0036.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY312_REC_MOT_clean <- distinct(FLY312_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY312_REC_MOT_ready <- FLY312_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0036_R <- transform(LOG_0036, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0036_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0036_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY312_REC_MOT_LD <- left_join(FLY312_REC_MOT_ready, LOG_0036_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY312_REC_MOT_LD_1 <- FLY312_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY312_REC_MOT_LD_1.2 <- FLY312_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_312 <- ggplot(FLY312_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-12 08:54:10 (FLY_312)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_312

# ---------------------------------------------- FLY_313 + LOG_0037--------------------------------------------- #
# --------------------------------------------- 2022-07-10 09:05:38 -------------------------------------------- #

# Import csv
FLY313_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY313_REC_MOT.csv")
# Import LOG 
LOG_0037 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0037.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY313_REC_MOT_clean <- distinct(FLY313_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY313_REC_MOT_ready <- FLY313_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0037_R <- transform(LOG_0037, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0037_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0037_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY313_REC_MOT_LD <- left_join(FLY313_REC_MOT_ready, LOG_0037_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY313_REC_MOT_LD_1 <- FLY313_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY313_REC_MOT_LD_1.2 <- FLY313_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_313 <- ggplot(FLY313_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-10 09:05:38 (FLY_313)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_313

# ---------------------------------------------- FLY_315 + LOG_0038--------------------------------------------- #
# --------------------------------------------- 2022-07-13 16:38:25 -------------------------------------------- #

# Import csv
FLY315_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY315_REC_MOT.csv")
# Import LOG 
LOG_0038 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0038.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY315_REC_MOT_clean <- distinct(FLY315_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY315_REC_MOT_ready <- FLY315_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0038_R <- transform(LOG_0038, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0038_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0038_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY315_REC_MOT_LD <- left_join(FLY315_REC_MOT_ready, LOG_0038_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY315_REC_MOT_LD_1 <- FLY315_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY315_REC_MOT_LD_1.2 <- FLY315_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_315 <- ggplot(FLY315_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-13 16:38:25 (FLY_315)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_315

# ---------------------------------------------- FLY_316 + LOG_0039--------------------------------------------- #
# --------------------------------------------- 2022-07-13 17:23:33 -------------------------------------------- #

# Import csv
FLY316_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY316_REC_MOT.csv")
# Import LOG 
LOG_0039 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0039.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY316_REC_MOT_clean <- distinct(FLY316_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY316_REC_MOT_ready <- FLY316_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0039_R <- transform(LOG_0039, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0039_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0039_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY316_REC_MOT_LD <- left_join(FLY316_REC_MOT_ready, LOG_0039_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY316_REC_MOT_LD_1 <- FLY316_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY316_REC_MOT_LD_1.2 <- FLY316_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_316 <- ggplot(FLY316_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-13 17:23:33 (FLY_316)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_316

# ---------------------------------------------- FLY_317 + LOG_0040--------------------------------------------- #
# --------------------------------------------- 2022-07-13 18:04:20 -------------------------------------------- #

# Import csv
FLY317_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY317_REC_MOT.csv")
# Import LOG 
LOG_0040 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0040.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY317_REC_MOT_clean <- distinct(FLY317_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY317_REC_MOT_ready <- FLY317_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0040_R <- transform(LOG_0040, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0040_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0040_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY317_REC_MOT_LD <- left_join(FLY317_REC_MOT_ready, LOG_0040_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY317_REC_MOT_LD_1 <- FLY317_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY317_REC_MOT_LD_1.2 <- FLY317_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_317 <- ggplot(FLY317_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-13 18:04:20 (FLY_317)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_317
