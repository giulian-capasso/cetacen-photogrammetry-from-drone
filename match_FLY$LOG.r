library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hms)
library(stringr)

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

# ---------------------------------------------- FLY_318 + LOG_0041--------------------------------------------- #
# --------------------------------------------- 2022-07-14 13:13:38 -------------------------------------------- #

# Import csv
FLY318_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY318_REC_MOT.csv")
# Import LOG 
LOG_0041 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0041.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY318_REC_MOT_clean <- distinct(FLY318_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY318_REC_MOT_ready <- FLY318_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0041_R <- transform(LOG_0041, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0041_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0041_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY318_REC_MOT_LD <- left_join(FLY318_REC_MOT_ready, LOG_0041_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY318_REC_MOT_LD_1 <- FLY318_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY318_REC_MOT_LD_1.2 <- FLY318_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_318 <- ggplot(FLY318_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-14 13:13:38 (FLY_318)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_318

# ---------------------------------------------- FLY_319 + LOG_0042--------------------------------------------- #
# --------------------------------------------- 2022-07-14 13:29:54 -------------------------------------------- #

# Import csv
FLY319_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY319_REC_MOT.csv")
# Import LOG 
LOG_0042 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0042.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY319_REC_MOT_clean <- distinct(FLY319_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY319_REC_MOT_ready <- FLY319_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0042_R <- transform(LOG_0042, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0042_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0042_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY319_REC_MOT_LD <- left_join(FLY319_REC_MOT_ready, LOG_0042_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY319_REC_MOT_LD_1 <- FLY319_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY319_REC_MOT_LD_1.2 <- FLY319_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_319 <- ggplot(FLY319_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-14 13:29:54 (FLY_319)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_319

# ---------------------------------------------- FLY_323 + LOG_0045--------------------------------------------- #
# --------------------------------------------- 2022-07-20 16:00:52 -------------------------------------------- #

# Import csv
FLY323_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY323_REC_MOT.csv")
# Import LOG 
LOG_0045 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0045.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY323_REC_MOT_clean <- distinct(FLY323_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY323_REC_MOT_ready <- FLY323_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0045_R <- transform(LOG_0045, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0045_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0045_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY323_REC_MOT_LD <- left_join(FLY323_REC_MOT_ready, LOG_0045_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY323_REC_MOT_LD_1 <- FLY323_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY323_REC_MOT_LD_1.2 <- FLY323_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_323 <- ggplot(FLY323_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-20 16:00:52 (FLY_323)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_323

# ---------------------------------------------- FLY_327 + LOG_0048--------------------------------------------- #
# --------------------------------------------- 2022-07-21 12:32:46 -------------------------------------------- #

# Import csv
FLY327_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY327_REC_MOT.csv")
# Import LOG 
LOG_0048 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0048.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY327_REC_MOT_clean <- distinct(FLY327_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY327_REC_MOT_ready <- FLY327_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0048_R <- transform(LOG_0048, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0048_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0048_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY327_REC_MOT_LD <- left_join(FLY327_REC_MOT_ready, LOG_0048_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY327_REC_MOT_LD_1 <- FLY327_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY327_REC_MOT_LD_1.2 <- FLY327_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_327 <- ggplot(FLY327_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-21 12:32:46 (FLY_327)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_327

# ---------------------------------------------- FLY_330 + LOG_0050--------------------------------------------- #
# --------------------------------------------- 2022-07-21 17:18:48 -------------------------------------------- #

# Import csv
FLY330_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY330_REC_MOT.csv")
# Import LOG 
LOG_0050 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0050.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY330_REC_MOT_clean <- distinct(FLY330_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY330_REC_MOT_ready <- FLY330_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0050_R <- transform(LOG_0050, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0050_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0050_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY330_REC_MOT_LD <- left_join(FLY330_REC_MOT_ready, LOG_0050_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY330_REC_MOT_LD_1 <- FLY330_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY330_REC_MOT_LD_1.2 <- FLY330_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_330 <- ggplot(FLY330_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-21 17:18:48 (FLY_330)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_330

# ---------------------------------------------- FLY_332 + LOG_0052--------------------------------------------- #
# --------------------------------------------- 2022-07-22 10:24:31 -------------------------------------------- #

# Import csv
FLY332_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY332_REC_MOT.csv")
# Import LOG 
LOG_0052 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0052.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY332_REC_MOT_clean <- distinct(FLY332_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY332_REC_MOT_ready <- FLY332_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0052_R <- transform(LOG_0052, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0052_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0052_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY332_REC_MOT_LD <- left_join(FLY332_REC_MOT_ready, LOG_0052_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY332_REC_MOT_LD_1 <- FLY332_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY332_REC_MOT_LD_1.2 <- FLY332_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_332 <- ggplot(FLY332_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-07-22 10:24:31 (FLY_332)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_332

# ---------------------------------------------- FLY_346 + NOLOG ----------------------------------------------- #
# --------------------------------------------- 2022-08-02 08:14:01 -------------------------------------------- #

# Import csv
FLY346_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY346_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY346_REC_MOT_clean <- distinct(FLY346_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY346_REC_MOT_ready <- FLY346_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_346 <- ggplot(FLY346_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-02 08:14:01 (FLY_346)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_346

# ---------------------------------------------- FLY_348 + NOLOG--------------------------------------------- #
# -------------------------------------------- 2022-08-02 09:01:01 -------------------------------------------- #

# Import csv
FLY348_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY348_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY348_REC_MOT_clean <- distinct(FLY348_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY348_REC_MOT_ready <- FLY348_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_348 <- ggplot(FLY348_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-02 09:01:01 (FLY_348)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_348

# ----------------------------------------------- FLY_349 + NOLOG--------------------------------------------- #
# --------------------------------------------- 2022-08-02 10:40:18 -------------------------------------------- #

# Import csv
FLY349_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY349_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY349_REC_MOT_clean <- distinct(FLY349_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY349_REC_MOT_ready <- FLY349_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_349 <- ggplot(FLY349_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-02 10:40:18 (FLY_349)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_349

# ---------------------------------------------- FLY_352 + LOG_0068--------------------------------------------- #
# --------------------------------------------- 2022-08-04 06:04:10 -------------------------------------------- #

# Import csv
FLY352_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY352_REC_MOT.csv")
# Import LOG 
LOG_0068 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0068.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY352_REC_MOT_clean <- distinct(FLY352_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY352_REC_MOT_ready <- FLY352_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0068_R <- transform(LOG_0068, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0068_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0068_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY352_REC_MOT_LD <- left_join(FLY352_REC_MOT_ready, LOG_0068_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY352_REC_MOT_LD_1 <- FLY352_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY352_REC_MOT_LD_1.2 <- FLY352_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_352 <- ggplot(FLY352_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-04 06:04:10 (FLY_352)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_352

# ---------------------------------------------- FLY_353 + LOG_0069--------------------------------------------- #
# --------------------------------------------- 2022-08-04 09:10:48 -------------------------------------------- #

# Import csv
FLY353_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY353_REC_MOT.csv")
# Import LOG 
LOG_0069 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0069.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY353_REC_MOT_clean <- distinct(FLY353_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY353_REC_MOT_ready <- FLY353_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0069_R <- transform(LOG_0069, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0069_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0069_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY353_REC_MOT_LD <- left_join(FLY353_REC_MOT_ready, LOG_0069_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY353_REC_MOT_LD_1 <- FLY353_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY353_REC_MOT_LD_1.2 <- FLY353_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_353 <- ggplot(FLY353_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-04 09:10:48 (FLY_353)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_353

# ---------------------------------------------- FLY_354 + LOG_0070--------------------------------------------- #
# --------------------------------------------- 2022-08-04 10:34:59-------------------------------------------- #

# Import csv
FLY354_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY354_REC_MOT.csv")
# Import LOG 
LOG_0070 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0070.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY354_REC_MOT_clean <- distinct(FLY354_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY354_REC_MOT_ready <- FLY354_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0070_R <- transform(LOG_0070, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0070_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0070_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY354_REC_MOT_LD <- left_join(FLY354_REC_MOT_ready, LOG_0070_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY354_REC_MOT_LD_1 <- FLY354_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY354_REC_MOT_LD_1.2 <- FLY354_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_354 <- ggplot(FLY354_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-04 10:34:59 (FLY_354)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_354

# ---------------------------------------------- FLY_356 + NOLOG--------------------------------------------- #
# --------------------------------------------- 2022-08-11 10:00:44-------------------------------------------- #

# Import csv
FLY356_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY356_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY356_REC_MOT_clean <- distinct(FLY356_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY356_REC_MOT_ready <- FLY356_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_356 <- ggplot(FLY356_REC_MOT_ready) +
  #geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-11 10:00:44 (FLY_356)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_356

# ---------------------------------------------- FLY_357 + NOLOG --------------------------------------------- #
# --------------------------------------------- 2022-08-11 10:11:28 -------------------------------------------- #

# Import csv
FLY357_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY357_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY357_REC_MOT_clean <- distinct(FLY357_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY357_REC_MOT_ready <- FLY357_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_357 <- ggplot(FLY357_REC_MOT_ready) +
 # geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-11 10:11:28 (FLY_357)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_357

# ---------------------------------------------- FLY_362 + NOLOG --------------------------------------------- #
# ---------------------------------------------------- NA ---------------------------------------------------- #

# Import csv
FLY362_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY362_REC_MOT.csv")
### EMPTY ####

# ---------------------------------------------- FLY_363 + NOLOG --------------------------------------------- #
# --------------------------------------------- 2022-08-11 11:08:14 -------------------------------------------- #

# Import csv
FLY363_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY363_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY363_REC_MOT_clean <- distinct(FLY363_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY363_REC_MOT_ready <- FLY363_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_363 <- ggplot(FLY363_REC_MOT_ready) +
  # geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-11 11:08:14 (FLY_363)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_363

# --------------------------------------------- FLY_365 + LOG_0071 --------------------------------------------- #
# -------------------------------------------- 2022-08-12 08:06:40 -------------------------------------------- #

# Import csv
FLY365_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY365_REC_MOT.csv")
# Import LOG 
LOG_0071 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0071.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY365_REC_MOT_clean <- distinct(FLY365_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY365_REC_MOT_ready <- FLY365_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0071_R <- transform(LOG_0071, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0071_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0071_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY365_REC_MOT_LD <- left_join(FLY365_REC_MOT_ready, LOG_0071_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY365_REC_MOT_LD_1 <- FLY365_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_365 <- ggplot(FLY365_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-12 08:06:40  (FLY_365)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_365

# --------------------------------------------- FLY_368 + LOG_0074 --------------------------------------------- #
# -------------------------------------------- 2022-08-12 15:11:23 -------------------------------------------- #

# Import csv
FLY368_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY368_REC_MOT.csv")
# Import LOG 
LOG_0074 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0074.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY368_REC_MOT_clean1 <- distinct(FLY368_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY368_REC_MOT_ready <- FLY368_REC_MOT_clean2 %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0074_R <- transform(LOG_0074, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0074_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0074_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY368_REC_MOT_LD <- left_join(FLY368_REC_MOT_ready, LOG_0074_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY368_REC_MOT_LD_1 <- FLY368_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY368_REC_MOT_LD_1.2 <- FLY368_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY368_REC_MOT_LD_1.2_clean <- subset(FLY368_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Plot 
plot_368 <- ggplot(FLY368_REC_MOT_LD_1.2_clean) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-12 15:11:23 (FLY_368)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_368

# --------------------------------------------- FLY_370 + NOLOG --------------------------------------------- #
# ------------------------------------------- 2022-08-14 07:32:11 -------------------------------------------- #

# Import csv
FLY370_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY370_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY370_REC_MOT_clean <- distinct(FLY370_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY370_REC_MOT_ready <- FLY370_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Clean "0002-11-30 00:00:00 UTC" value
FLY370_REC_MOT_ready_clean <- subset(FLY370_REC_MOT_ready, !grepl("2015-10-18", GPS.dateTimeStamp))
# Plot 
plot_370 <- ggplot(FLY370_REC_MOT_ready_clean) +
 # geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-14 07:32:11 (FLY_370)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_370

# --------------------------------------------- FLY_371 + LOG_0075 --------------------------------------------- #
# -------------------------------------------- 2022-08-14 09:16:17 -------------------------------------------- #

# Import csv
FLY371_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY371_REC_MOT.csv")
# Import LOG 
LOG_0075 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0075.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY371_REC_MOT_clean <- distinct(FLY371_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY371_REC_MOT_ready <- FLY371_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0075_R <- transform(LOG_0075, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0075_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0075_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY371_REC_MOT_LD <- left_join(FLY371_REC_MOT_ready, LOG_0075_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY371_REC_MOT_LD_1 <- FLY371_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY371_REC_MOT_LD_1.2 <- FLY371_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_371 <- ggplot(FLY371_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-14 09:16:17 (FLY_371)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
plot_371

# --------------------------------------------- FLY_372 + LOG_0076 --------------------------------------------- #
 # -------------------------------------------- 2022-08-14 09:42:01 ------------------------------------------- #
  # ------------------------------------------------ file bugs ----------------------------------------------- #

# Import csv
FLY372_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY372_REC_MOT.csv")
# Import LOG 
LOG_0076 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0076.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY372_REC_MOT_clean <- distinct(FLY372_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY372_REC_MOT_ready <- FLY372_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0076_R <- transform(LOG_0076, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0076_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0076_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY372_REC_MOT_LD <- left_join(FLY372_REC_MOT_ready, LOG_0076_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY372_REC_MOT_LD_1 <- FLY372_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY372_REC_MOT_LD_1.2 <- FLY372_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_372 <- ggplot(FLY372_REC_MOT_LD_1.2 ) +
  geom_line(aes(x = `Clock:offsetTime`, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = `Clock:offsetTime`, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-08-14 09:42:01 (FLY_372)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold"))
 plot_372
# ONLY BAROMETER PLOT
 plot_372B <- ggplot(FLY372_REC_MOT) +
  # geom_line(aes(x = `Clock:offsetTime`, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = `Clock:offsetTime`, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-08-14 09:42:01 (FLY_372) BAR", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_372B
 
 # ONLY LIDAR PLOT
 # Laser data from cm to m
LOG_0076_RM <- LOG_0076_R %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
LOG_0076_RM2 <- LOG_0076_RM %>% rename(laser_altitude_m = laser_altitude_cm)
 
 plot_372L <- ggplot(LOG_0076_RM2) +
   geom_line(aes(x = gmt_time , y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  # geom_line(aes(x = `Clock:offsetTime`, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-08-14 09:42:01 (FLY_372) LIDAR", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_372L
 

 # --------------------------------------------- FLY_374 + LOG_0078 --------------------------------------------- #
 # -------------------------------------------- 2022-08-14 10:16:31 -------------------------------------------- #
 
 # Import csv
 FLY374_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY374_REC_MOT.csv")
 # Import LOG 
 LOG_0078 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0078.CSV", 
                         skip = 2)
 # Clean repetitions in `GPS:dateTimeStamp`
 FLY374_REC_MOT_clean <- distinct(FLY374_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
 # Reneme FLY "`GPS:dateTimeStamp`" name 
 FLY374_REC_MOT_ready <- FLY374_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
 # Merge LOG Date and Time 
 LOG_0078_R <- transform(LOG_0078, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
 # Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
 LOG_0078_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0078_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 # Join FLY and LOG columns by GPS.dateTimeStamp
 FLY374_REC_MOT_LD <- left_join(FLY374_REC_MOT_ready, LOG_0078_R, by = "GPS.dateTimeStamp")
 # Laser data from cm to m
 FLY374_REC_MOT_LD_1 <- FLY374_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
 FLY374_REC_MOT_LD_1.2 <- FLY374_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
 # Plot 
 plot_374 <- ggplot(FLY374_REC_MOT_LD_1.2) +
   geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-08-14 10:16:31 (FLY_374)", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_374
 
 # --------------------------------------------- FLY_382 + LOG_0079 --------------------------------------------- #
 # -------------------------------------------- 2022-10-18 09:41:40 -------------------------------------------- #
 
 # Import csv
 FLY382_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY382_REC_MOT.csv")
 # Import LOG 
 LOG_0079 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0079.CSV", 
                         skip = 2)
 # Clean repetitions in `GPS:dateTimeStamp`
 FLY382_REC_MOT_clean <- distinct(FLY382_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
 # Reneme FLY "`GPS:dateTimeStamp`" name 
 FLY382_REC_MOT_ready <- FLY382_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
 # Merge LOG Date and Time 
 LOG_0079_R <- transform(LOG_0079, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
 # Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
 LOG_0079_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0079_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 # Join FLY and LOG columns by GPS.dateTimeStamp
 FLY382_REC_MOT_LD <- left_join(FLY382_REC_MOT_ready, LOG_0079_R, by = "GPS.dateTimeStamp")
 # Laser data from cm to m
 FLY382_REC_MOT_LD_1 <- FLY382_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
 FLY382_REC_MOT_LD_1.2 <- FLY382_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
 # Plot 
 plot_382 <- ggplot(FLY382_REC_MOT_LD_1.2) +
   geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-10-18 09:41:40 (FLY_382)", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_382

 
 # --------------------------------------------- FLY_383 + LOG_0080 --------------------------------------------- #
 # -------------------------------------------- 2022-10-18 10:04:40 -------------------------------------------- #
 
 # Import csv
 FLY383_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY383_REC_MOT.csv")
 # Import LOG 
 LOG_0080 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0080.CSV", 
                         skip = 2)
 # Clean repetitions in `GPS:dateTimeStamp`
 FLY383_REC_MOT_clean <- distinct(FLY383_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
 # Reneme FLY "`GPS:dateTimeStamp`" name 
 FLY383_REC_MOT_ready <- FLY383_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
 # Merge LOG Date and Time 
 LOG_0080_R <- transform(LOG_0080, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
 # Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
 LOG_0080_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0080_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 # Join FLY and LOG columns by GPS.dateTimeStamp
 FLY383_REC_MOT_LD <- left_join(FLY383_REC_MOT_ready, LOG_0080_R, by = "GPS.dateTimeStamp")
 # Laser data from cm to m
 FLY383_REC_MOT_LD_1 <- FLY383_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
 FLY383_REC_MOT_LD_1.2 <- FLY383_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
 # Plot 
 plot_383 <- ggplot(FLY383_REC_MOT_LD_1.2) +
   geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-10-18 10:04:40 (FLY_383)", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_383


 # --------------------------------------------- FLY_384 + LOG_0081 --------------------------------------------- #
 # -------------------------------------------- 2022-10-19 10:07:31 -------------------------------------------- #
 
 # Import csv
 FLY384_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY384_REC_MOT.csv")
 # Import LOG 
 LOG_0081 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0081.CSV", 
                         skip = 2)
 # Clean repetitions in `GPS:dateTimeStamp`
 FLY384_REC_MOT_clean <- distinct(FLY384_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
 # Reneme FLY "`GPS:dateTimeStamp`" name 
 FLY384_REC_MOT_ready <- FLY384_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
 # Merge LOG Date and Time 
 LOG_0081_R <- transform(LOG_0081, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
 # Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
 LOG_0081_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0081_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 # Join FLY and LOG columns by GPS.dateTimeStamp
 FLY384_REC_MOT_LD <- left_join(FLY384_REC_MOT_ready, LOG_0081_R, by = "GPS.dateTimeStamp")
 # Laser data from cm to m
 FLY384_REC_MOT_LD_1 <- FLY384_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
 FLY384_REC_MOT_LD_1.2 <- FLY384_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
 # Plot 
 plot_384 <- ggplot(FLY384_REC_MOT_LD_1.2) +
   geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-10-19 10:07:31 (FLY_384)", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_384

 
 # --------------------------------------------- FLY_387 + LOG_0084 --------------------------------------------- #
 # -------------------------------------------- 2022-10-19 11:10:10 -------------------------------------------- #
 
 # Import csv
 FLY387_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY387_REC_MOT.csv")
 # Import LOG 
 LOG_0084 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0084.CSV", 
                         skip = 2)
 # Clean repetitions in `GPS:dateTimeStamp`
 FLY387_REC_MOT_clean <- distinct(FLY387_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
 # Reneme FLY "`GPS:dateTimeStamp`" name 
 FLY387_REC_MOT_ready <- FLY387_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
 # Merge LOG Date and Time 
 LOG_0084_R <- transform(LOG_0084, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
 # Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
 LOG_0084_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0084_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 # Join FLY and LOG columns by GPS.dateTimeStamp
 FLY387_REC_MOT_LD <- left_join(FLY387_REC_MOT_ready, LOG_0084_R, by = "GPS.dateTimeStamp")
 # Laser data from cm to m
 FLY387_REC_MOT_LD_1 <- FLY387_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
 # Rename to meters
 FLY387_REC_MOT_LD_1.2 <- FLY387_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
 # Plot 
 plot_387 <- ggplot(FLY387_REC_MOT_LD_1.2) +
   geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
   geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
   labs(title = "2022-10-19 11:10:10 (FLY_387)", x = "Time", y = "Height (m)") +
   theme_minimal() +
   theme(legend.position = c(0.9, 0.9),
         legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
         legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
         axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
   scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
   theme(plot.title = element_text(size = 10, face = "bold"))
 plot_387
 
# --------------------------------------------- FLY_388 + LOG_0085 --------------------------------------------- #
# -------------------------------------------- 2022-10-19 11:25:31 -------------------------------------------- #

# Import csv
FLY388_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY388_REC_MOT.csv")
# Import LOG 
LOG_0085 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0085.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY388_REC_MOT_clean <- distinct(FLY388_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY388_REC_MOT_ready <- FLY388_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0085_R <- transform(LOG_0085, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0085_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0085_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY388_REC_MOT_LD <- left_join(FLY388_REC_MOT_ready, LOG_0085_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY388_REC_MOT_LD_1 <- FLY388_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY388_REC_MOT_LD_1.2 <- FLY388_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_388 <- ggplot(FLY388_REC_MOT_LD_1.2) +
 geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
 geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
 labs(title = "2022-10-19 11:25:31 (FLY_388)", x = "Time", y = "Height (m)") +
 theme_minimal() +
 theme(legend.position = c(0.9, 0.9),
       legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
       legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
       axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
 scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
 theme(plot.title = element_text(size = 10, face = "bold")) 
plot_388

# --------------------------------------------- FLY_389 + LOG_0086 --------------------------------------------- #
# -------------------------------------------- 2022-10-19 11:36:11 -------------------------------------------- #

# Import csv
FLY389_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY389_REC_MOT.csv")
# Import LOG 
LOG_0086 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0086.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY389_REC_MOT_clean2 <- distinct(FLY389_REC_MOT_clean1, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY389_REC_MOT_ready <- FLY389_REC_MOT_clean2 %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0086_R <- transform(LOG_0086, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0086_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0086_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY389_REC_MOT_LD <- left_join(FLY389_REC_MOT_ready, LOG_0086_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY389_REC_MOT_LD_1 <- FLY389_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY389_REC_MOT_LD_1.2 <- FLY389_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY389_REC_MOT_LD_1.2_clean <- subset(FLY389_REC_MOT_LD_1.2, !grepl("2015-10-21", GPS.dateTimeStamp))
# Plot 
plot_389 <- ggplot(FLY389_REC_MOT_LD_1.2_clean) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-10-19 11:36:11 (FLY_389)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_389

# --------------------------------------------- FLY_393 + LOG_0088 --------------------------------------------- #
# -------------------------------------------- 2022-10-26 16:21:30 -------------------------------------------- #

# Import csv
FLY393_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY393_REC_MOT.csv")
# Import LOG 
LOG_0088 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0088.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY393_REC_MOT_clean <- distinct(FLY393_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY393_REC_MOT_ready <- FLY393_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0088_R <- transform(LOG_0088, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0088_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0088_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY393_REC_MOT_LD <- left_join(FLY393_REC_MOT_ready, LOG_0088_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY393_REC_MOT_LD_1 <- FLY393_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY393_REC_MOT_LD_1.2 <- FLY393_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_393 <- ggplot(FLY393_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2022-10-26 16:21:30 (FLY_393)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_393

# --------------------------------------------- FLY_396 + LOG_0090 --------------------------------------------- #
# -------------------------------------------- 2023-02-22 08:45:32 -------------------------------------------- #

# Import csv
FLY396_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY396_REC_MOT.csv")
# Import LOG 
LOG_0090 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0090.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY396_REC_MOT_clean <- distinct(FLY396_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY396_REC_MOT_ready <- FLY396_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0090_R <- transform(LOG_0090, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0090_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0090_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY396_REC_MOT_LD <- left_join(FLY396_REC_MOT_ready, LOG_0090_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY396_REC_MOT_LD_1 <- FLY396_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY396_REC_MOT_LD_1.2 <- FLY396_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY396_REC_MOT_LD_1.2_clean <- subset(FLY396_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Plot 
plot_396 <- ggplot(FLY396_REC_MOT_LD_1.2_clean) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-02-22 08:45:32  (FLY_396)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_396


# --------------------------------------------- FLY_400 + LOG_0093 --------------------------------------------- #
# -------------------------------------------- 2023-02-22 09:47:57 -------------------------------------------- #

# Import csv
FLY400_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY400_REC_MOT.csv")
# Import LOG 
LOG_0093 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0093.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY400_REC_MOT_clean <- distinct(FLY400_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY400_REC_MOT_ready <- FLY400_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0093_R <- transform(LOG_0093, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0093_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0093_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY400_REC_MOT_LD <- left_join(FLY400_REC_MOT_ready, LOG_0093_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY400_REC_MOT_LD_1 <- FLY400_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY400_REC_MOT_LD_1.2 <- FLY400_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_400 <- ggplot(FLY400_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-02-22 09:47:57 (FLY_400)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_400

# --------------------------------------------- FLY_401 + LOG_0094 --------------------------------------------- #
# -------------------------------------------- 2023-02-22 09:57:52 -------------------------------------------- #

# Import csv
FLY401_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY401_REC_MOT.csv")
# Import LOG 
LOG_0094 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0094.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY401_REC_MOT_clean <- distinct(FLY401_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY401_REC_MOT_ready <- FLY401_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0094_R <- transform(LOG_0094, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0094_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0094_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY401_REC_MOT_LD <- left_join(FLY401_REC_MOT_ready, LOG_0094_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY401_REC_MOT_LD_1 <- FLY401_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY401_REC_MOT_LD_1.2 <- FLY401_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_401 <- ggplot(FLY401_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-02-22 09:47:57 (FLY_401)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_401

# --------------------------------------------- FLY_402 + LOG_0095 --------------------------------------------- #
# -------------------------------------------- 2023-02-22 12:49:01 -------------------------------------------- #

# Import csv
FLY402_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY402_REC_MOT.csv")
# Import LOG 
LOG_0095 <- read_table2("Desktop/Matched 1/LIDAR DATA copia/LOG_0095.CSV",  
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY402_REC_MOT_clean <- distinct(FLY402_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY402_REC_MOT_ready <- FLY402_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0095_R <- transform(LOG_0095, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0095_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0095_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY402_REC_MOT_LD <- left_join(FLY402_REC_MOT_ready, LOG_0095_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY402_REC_MOT_LD_1 <- FLY402_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY402_REC_MOT_LD_1.2 <- FLY402_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_402 <- ggplot(FLY402_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-02-22 12:49:01 (FLY_402)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_402


# --------------------------------------------- FLY_406 + NOLOG --------------------------------------------- #
# -------------------------------------------- 2023-02-22 13:55:31 -------------------------------------------- #

# Import csv
FLY406_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY406_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY406_REC_MOT_clean <- distinct(FLY406_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY406_REC_MOT_ready <- FLY406_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_406 <- ggplot(FLY406_REC_MOT_ready) +
# geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-02-22 13:55:31 (FLY_406)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_406

#########.........................................Second Folder..............................................##########
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# --------------------------------------------- FLY_411 + LOG99 --------------------------------------------- #
# ------------------------------------------- 2023-05-06 09:16:31 -------------------------------------------- #

# Import csv
FLY411_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY411_REC_MOT.csv")
# Import LOG 
LOG_0099 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0099.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY411_REC_MOT_clean <- distinct(FLY411_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY411_REC_MOT_ready <- FLY411_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0099_R <- transform(LOG_0099, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0099_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0099_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY411_REC_MOT_LD <- left_join(FLY411_REC_MOT_ready, LOG_0099_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY411_REC_MOT_LD_1 <- FLY411_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY411_REC_MOT_LD_1.2 <- FLY411_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_411 <- ggplot(FLY411_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-05-06 09:16:31 (FLY_411)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_411

# --------------------------------------------- FLY_412 + LOG100 --------------------------------------------- #
# ------------------------------------------- 2023-05-06 09:29:32 -------------------------------------------- #

# Import csv
FLY412_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY412_REC_MOT.csv")
# Import LOG 
LOG_0100 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0100.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY412_REC_MOT_clean <- distinct(FLY412_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY412_REC_MOT_ready <- FLY412_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0100_R <- transform(LOG_0100, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0100_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0100_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY412_REC_MOT_LD <- left_join(FLY412_REC_MOT_ready, LOG_0100_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY412_REC_MOT_LD_1 <- FLY412_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY412_REC_MOT_LD_1.2 <- FLY412_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30"
FLY412_REC_MOT_LD_1.2_clean <- subset(FLY368_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Plot 
plot_412 <- ggplot(FLY412_REC_MOT_LD_1.2_clean) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-05-06 09:29:32 (FLY_412)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_412

# --------------------------------------------- FLY_414 + LOG103 --------------------------------------------- #
# ------------------------------------------- 2023-05-06 16:18:31 -------------------------------------------- #

# Import csv
FLY414_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY414_REC_MOT.csv")
# Import LOG 
LOG_0103 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0103.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY414_REC_MOT_clean <- distinct(FLY414_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY414_REC_MOT_ready <- FLY414_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0103_R <- transform(LOG_0103, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0103_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0103_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY414_REC_MOT_LD <- left_join(FLY414_REC_MOT_ready, LOG_0103_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY414_REC_MOT_LD_1 <- FLY414_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY414_REC_MOT_LD_1.2 <- FLY414_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_414 <- ggplot(FLY414_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-05-06 16:18:31 (FLY_414)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_414

# --------------------------------------------- FLY_417 + LOG106 --------------------------------------------- #
# ------------------------------------------- 2023-05-24 10:29:31 -------------------------------------------- #

# Import csv
FLY417_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY417_REC_MOT.csv")
# Import LOG 
LOG_0106 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0106.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY417_REC_MOT_clean <- distinct(FLY417_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY417_REC_MOT_ready <- FLY417_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0106_R <- transform(LOG_0106, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0106_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0106_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY417_REC_MOT_LD <- left_join(FLY417_REC_MOT_ready, LOG_0106_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY417_REC_MOT_LD_1 <- FLY417_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY417_REC_MOT_LD_1.2 <- FLY417_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Plot 
plot_417 <- ggplot(FLY417_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-05-24 10:29:31  (FLY_417)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_417

# --------------------------------------------- FLY_420 + LOG108 --------------------------------------------- #
# ------------------------------------------- 2023-06-17 09:19:13 -------------------------------------------- #

# Import csv
FLY420_REC_MOT <- read_csv("Desktop/Matched 2/BAR/FLY420_REC_MOT.csv")
# Import LOG 
LOG_0108 <- read_table2("Desktop/Matched 2/LIDAR al 11_2023 copia/LOG_0108.CSV", 
                        skip = 2)
# Clean repetitions in `GPS:dateTimeStamp`
FLY420_REC_MOT_clean <- distinct(FLY420_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Rename FLY "`GPS:dateTimeStamp`" name 
FLY420_REC_MOT_ready <- FLY420_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Merge LOG Date and Time 
LOG_0108_R <- transform(LOG_0108, `GPS:dateTimeStamp` = paste(`#gmt_date`, gmt_time, sep = " "))
# Transform GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0108_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0108_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Join FLY and LOG columns by GPS.dateTimeStamp
FLY420_REC_MOT_LD <- left_join(FLY420_REC_MOT_ready, LOG_0108_R, by = "GPS.dateTimeStamp")
# Laser data from cm to m
FLY420_REC_MOT_LD_1 <- FLY420_REC_MOT_LD %>% mutate(laser_altitude_cm = laser_altitude_cm / 100)
# Rename to meters
FLY420_REC_MOT_LD_1.2 <- FLY420_REC_MOT_LD_1 %>% rename(laser_altitude_m = laser_altitude_cm)
# Clean "0002-11-30 00:00:00 UTC" value
FLY420_REC_MOT_LD_1.2_clean <- subset(FLY420_REC_MOT_LD_1.2, !grepl("0002-11-30", GPS.dateTimeStamp))
# Plot 
plot_420 <- ggplot(FLY420_REC_MOT_LD_1.2_clean) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "2023-06-17 09:19:13 (FLY_420)", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue")) +
  theme(plot.title = element_text(size = 10, face = "bold")) 
plot_420

