library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

# 317
FLY317_REC_REC_2_LD <- read_delim("Desktop/VID/PC_220713_biopsy6_22_photogrammetry/CVS/FLY317_REC_REC_2_LD.csv", 
                                    +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

LOG_0040_clean <- read_delim("Desktop/VID/PC_220713_biopsy6_22_photogrammetry/LOG_0040_clean.csv", 
                             +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(FLY317_REC_REC_2_LD)
View(LOG_0040_clean)

Time_numeric <- as.numeric(FLY317_REC_REC_2_LD[[962]])
H_numeric <- as.numeric(FLY317_REC_REC_2_LD[[268]])

H <- ggplot(FLY317_REC_REC_2_LD, aes(x = H_numeric, y = Time_numeric)) +
  geom_line() + 
  labs(title = "Grafico altezza",yre
       x = "Orario",
       y = "Altezza")

print(H)

###############

AIRDATA <- read_csv("Downloads/Jul-13th-2022-08-04PM-Flight-Airdata (1).csv")

H_AIRDATA <- ggplot(AIRDATA, aes(x = `height_above_takeoff(meters)`, y = `time(millisecond)`)) +
  geom_line() + 
  labs(title = "Grafico altezza",
       x = "Orario",
       y = "Altezza")

print(H_AIRDATA)

plot_airdata <- plot(AIRDATA$`time(millisecond)`, AIRDATA$`height_above_takeoff(meters)`, type = "l", col = "blue", lwd = 2, main = "Grafico Altezza Airdata")

plot_original <- plot(FLY317_REC_REC_2_LD$`GPS:Time`, FLY317_REC_REC_2_LD$`osd_data:relativeHeight[meters]`, type = "l", col = "blue", lwd = 2, main = "Grafico Altezza Original")
plot_laser <- plot(FLY317_REC_REC_2_LD$LIDARgmt_time, FLY317_REC_REC_2_LD$`LIDAR laser_altitude_cm`, type = "l", col = "blue", lwd = 2, main = "Grafico Altezza Original")


### Double plot ###
blind_friendly <- viridis(2)

LASER_M_317 <- FLY317_REC_REC_2_LD$`LIDAR laser_altitude_cm` / 100

matched_317 <- plot(FLY317_REC_REC_2_LD$`GPS:Time`, LASER_m, type = "l", col = "darkorange", lwd = 2, main = "BAR-LiDAR") +
  lines(FLY317_REC_REC_2_LD$`GPS:Time`, FLY317_REC_REC_2_LD$`osd_data:relativeHeight[meters]`, col = "blue", lwd = 2)


##### 304 
FLY304_REC_REC_LD <- read_delim("Desktop/VID/PC_220630_biopsy22_2_photogrammetry/FLY304_REC_REC_LD.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

LASER_M_304<- FLY304_REC_REC_LD$laser_altitude_cm / 100
#LASER_M_304_C <- na.omit(LASER_M_304)
#LASER_M_304_C <- LASER_M_304_C[1:length(FLY304_REC_REC_LD$`GPS:Time`)]

matched_304 <- plot(FLY304_REC_REC_LD$`GPS:Time`, LASER_M_304, type = "l", col = "darkorange", lwd = 2, main = "BAR-LiDAR", xlab = "Time (HH:MM:SS)", ylab = "Height (m)") +
  lines(FLY304_REC_REC_LD$`GPS:Time`, FLY304_REC_REC_LD$`osd_data:relativeHeight[meters]`, col = "blue", lwd = 2)
  legend("topright", legend = c("LIDAR", "BAR"), col = c("darkorange", "blue"), lwd = 2, cex = 0.8, x.intersp = 0.2, y.intersp = 0.9)
  
# 307
FLY307_REC_REC_LD <- read_delim("Desktop/VID/PC_220630_biopsy22_3_photogrammetry/FLY307_REC_REC_LD.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
LASER_M_307 <- FLY307_REC_REC_LD$laser_altitude_cm / 100
matched_307 <- plot(FLY307_REC_REC_LD$`GPS:Time`, LASER_M_307, type = "l", col = "darkorange", lwd = 2, main = "BAR-LiDAR", xlab = "Time (HH:MM:SS)", ylab = "Height (m)") +
  lines(FLY307_REC_REC_LD$`GPS:Time`, FLY307_REC_REC_LD$`osd_data:relativeHeight[meters]`, col = "blue", lwd = 2)
legend("topright", legend = c("LIDAR", "BAR"), col = c("darkorange", "blue"), lwd = 2, cex = 0.8, x.intersp = 0.2, y.intersp = 0.9)

#315

FLY315_REC_REC_LD <- read_delim("Desktop/VID/PC_220713_photogrammetry/FLY315_REC_REC_LD.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

LASER_M_315 <- FLY315_REC_REC_LD$laser_altitude_cm / 100
matched_315 <- plot(FLY315_REC_REC_LD$`GPS:Time`, LASER_M_315, type = "l", col = "darkorange", lwd = 2, main = "BAR-LiDAR", xlab = "Time (HH:MM:SS)", ylab = "Height (m)") +
  lines(FLY315_REC_REC_LD$`GPS:Time`, FLY315_REC_REC_LD$`osd_data:relativeHeight[meters]`, col = "blue", lwd = 2)
legend("topright", legend = c("LIDAR", "BAR"), col = c("darkorange", "blue"), lwd = 2, cex = 0.8, x.intersp = 0.2, y.intersp = 0.9)

#################################################

# GGPLOT 315
gg_matched_315 <- ggplot() +
  geom_line(aes(x = FLY315_REC_REC_LD$`GPS:Time`, y = LASER_M_315), color = "darkorange", size = 0.7, linetype = "solid") +
  geom_line(aes(x = FLY315_REC_REC_LD$`GPS:Time`, y = FLY315_REC_REC_LD$`osd_data:relativeHeight[meters]`), color = "blue", size = 0.7, linetype = "solid") +
  labs(title = "FLY_315", x = "Time (HH:MM:SS)", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 8), axis.text = element_text(size = 6), axis.title = element_text(size = 8)) +
  scale_colour_manual(name = "Legend", values = c("darkorange", "blue"), labels = c("LIDAR", "BAR")) +
  guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid", shape = NA), ncol = 2))

# GGPLOT 307
gg_matched_307 <- ggplot() +
  geom_line(aes(x = FLY307_REC_REC_LD$`GPS:Time`, y = LASER_M_307), color = "darkorange", size = 0.7, linetype = "solid") +
  geom_line(aes(x = FLY307_REC_REC_LD$`GPS:Time`, y = FLY307_REC_REC_LD$`osd_data:relativeHeight[meters]`), color = "blue", size = 0.7, linetype = "solid") +
  labs(title = "FLY_307", x = "Time (HH:MM:SS)", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 8), axis.text = element_text(size = 6), axis.title = element_text(size = 8)) +
  scale_colour_manual(name = "Legend", values = c("darkorange", "blue"), labels = c("LIDAR", "BAR")) +
  guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid", shape = NA), ncol = 2))

# GGPLOT 304
gg_matched_304 <- ggplot() +
  geom_line(aes(x = FLY304_REC_REC_LD$`GPS:Time`, y = LASER_M_304), color = "darkorange", size = 0.7, linetype = "solid") +
  geom_line(aes(x = FLY304_REC_REC_LD$`GPS:Time`, y = FLY304_REC_REC_LD$`osd_data:relativeHeight[meters]`), color = "blue", size = 0.7, linetype = "solid") +
  labs(title = "FLY_304", x = "Time (HH:MM:SS)", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 8), axis.text = element_text(size = 6), axis.title = element_text(size = 8)) +
  scale_colour_manual(name = "Legend", values = c("darkorange", "blue"), labels = c("LIDAR", "BAR")) +
  guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid", shape = NA), ncol = 2))

# GGPLOT 317
gg_matched_317 <- ggplot() +
  geom_line(aes(x = FLY317_REC_REC_2_LD$`GPS:Time`, y = LASER_M_317), color = "darkorange", size = 0.7, linetype = "solid") +
  geom_line(aes(x = FLY317_REC_REC_2_LD$`GPS:Time`, y = FLY317_REC_REC_2_LD$`osd_data:relativeHeight[meters]`), color = "blue", size = 0.7, linetype = "solid") +
  labs(title = "FLY_317", x = "Time (HH:MM:SS)", y = "Height (m)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(size = 8), axis.text = element_text(size = 6), axis.title = element_text(size = 8)) +
  scale_colour_manual(name = "Legend", values = c("darkorange", "blue"), labels = c("LIDAR", "BAR")) +
  guides(color = guide_legend(override.aes = list(size = 2, linetype = "solid", shape = NA), ncol = 2))


gg_matched_307 + gg_matched_315 + gg_matched_304 + gg_matched_317
