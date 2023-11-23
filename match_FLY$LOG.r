library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
install.packages("hms")
library(hms)

# PROCESS 

# Import CSV
FLY285_REC_MOT <- read_csv("Desktop/Matched 1/Original/FLY285_REC_MOT.csv")

# Import LOG With frist 2 columns deleted and new names 
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
# Plot 
plot_285 <- ggplot(FLY285_REC_MOT_LD_1.2) +
  geom_line(aes(x = GPS.dateTimeStamp, y = laser_altitude_m, color = "LIDAR"), size = 0.7, linetype = "solid") +
  geom_line(aes(x = GPS.dateTimeStamp, y = `osd_data:relativeHeight[meters]`, color = "BAR"), size = 0.7, linetype = "solid") +
  labs(title = "FLY_285", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue"))
plot_285


# Upload Series 

# --------------------------------------------- FLY_304 + LOG_0030 ----------------------------------------------- #
# -------------------------------------------- 2022-06-30 08:15:31 ----------------------------------------------- #

# Import CSV 
FLY304_REC_MOT <- read_csv("Desktop/VID/PC_220630_biopsy22_2_photogrammetry/FLY304_REC_MOT.csv")
# Import LOG 
LOG_0030 <- read_delim("Desktop/VID/PC_220630_biopsy22_2_photogrammetry/LOG_0030.csv", 
                       +     delim = ";", escape_double = FALSE, trim_ws = TRUE)
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
  labs(title = "FLY_304", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue"))
plot_304

# ---------------------------------------------- FLY_307 + LOG_0032--------------------------------------------- #
# -------------------------------------------- 2022-06-30 15:14:31 --------------------------------------------- #

# Import csv
FLY307_REC_MOT <- read_csv("Desktop/VID/PC_220630_biopsy22_3_photogrammetry/FLY307_REC_MOT.csv")
# Import log
LOG_0032 <- read_table2("Desktop/VID/PC_220630_biopsy22_3_photogrammetry/LOG_0032.CSV", 
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
  labs(title = "FLY_307", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9), legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue"))
plot_307


# ---------------------------------------------- FLY_317 + LOG_0040--------------------------------------------- #
# --------------------------------------------- 2022-07-13 18:04:20 -------------------------------------------- #

# Import csv
FLY317_REC_MOT <- read_csv("Desktop/VID/PC_220713_biopsy6_22_photogrammetry/Prove_exp/FLY317_REC_MOT.csv")
# Import log
LOG_0040 <- read_table2("Desktop/VID/PC_220713_biopsy6_22_photogrammetry/LOG_0040.CSV", 
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
  labs(title = "FLY_317", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
        legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue"))
plot_317



# ---------------------------------------------- FLY_315 + LOG_0038--------------------------------------------- #
# --------------------------------------------- 2022-07-13 16:38:25 -------------------------------------------- #

# Import csv
FLY315_REC_MOT <- read_csv("Desktop/VID/PC_220713_photogrammetry/FLY315_REC_MOT.csv")
# Import log
LOG_0038 <- read_delim("Desktop/VID/PC_220713_photogrammetry/LOG_0038 2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                         skip = 1)
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
  labs(title = "FLY_315", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
    legend.box = "horizontal", legend.background = element_rect(color = "gray", fill = "white"),
    legend.margin = margin(t = -6, r = 6, b = -1.5, l = 6), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), legend.text = element_text(size = 7)) +
  scale_colour_manual(name = NULL, values = c("LIDAR" = "darkorange", "BAR" = "blue"))
plot_315


# ---------------------------------------------- FLY_263 + NOLOG--------------------------------------------- #
# -------------------------------------------- 2020-09-29 14:10:01 ------------------------------------------ #

# Import csv
FLY263_REC_MOT <- read_csv("Desktop/Matched 1/BAR/FLY263_REC_MOT.csv")
# Clean repetitions in `GPS:dateTimeStamp`
FLY263_REC_MOT_clean <- distinct(FLY263_REC_MOT, `GPS:dateTimeStamp`, .keep_all = TRUE)
# Reneme FLY "`GPS:dateTimeStamp`" name 
FLY263_REC_MOT_ready <- FLY263_REC_MOT_clean %>% rename(GPS.dateTimeStamp = `GPS:dateTimeStamp`)
# Plot 
plot_263 <- ggplot() +
  #geom_line(aes(x = FLY315_REC_MOT_LD_1.2$GPS.dateTimeStamp, y = FLY315_REC_MOT_LD_1.2$laser_altitude_m), color = "darkorange", size = 0.7, linetype = "solid") +
  geom_line(aes(x = FLY315_REC_MOT_LD_1.2$GPS.dateTimeStamp, y = FLY315_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`), color = "blue", size = 0.7, linetype = "solid") +
  labs(title = "FLY_263", x = "Time", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 8), axis.text = element_text(size = 6), axis.title = element_text(size = 8)) +
  scale_colour_manual(name = "Legend", values = c("darkorange", "blue"), labels = c("LIDAR", "BAR")) +
  guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid", shape = NA), ncol = 2))
plot_263

