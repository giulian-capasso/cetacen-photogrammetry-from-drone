#### VOLI CON MISURAZIONI ####

#### FLY_365 + LOG_0071 ####

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
# offset BAR to match with Lidar, max 3 rows meaning 
FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0,0, head(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -2))
# Azzera accensione motori
FLY365_REC_MOT_LD_1.2 <- azzera_dataset(FLY365_REC_MOT_LD_1.2)
#Add Flight_ID column for future could plot
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1.2 %>% mutate(Flight_ID = "FLY365")
# Add seconds column
FLY365_REC_MOT_LD_1.2 <- FLY365_REC_MOT_LD_1.2 %>% mutate(seconds = row_number())

# Pulizia spikes 
FLY365_REC_MOT_LD_1.2 <- spike_cleaning(FLY365_REC_MOT_LD_1.2, column_name = "laser_altitude_m", new_column_name = "laser_altitude_m_cleaned", 
                                        lower_limit = 0, upper_limit = 60, omit_first_n = 0, omit_last_n = 0)

# check results by plotting
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = GPS.dateTimeStamp)) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_cleaned, color = "Cleaned Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold"))

# Correggo tilt 
FLY365_REC_MOT_LD_1.2$tilt_corrected <- FLY365_REC_MOT_LD_1.2$tilt_deg - 5
FLY365_REC_MOT_LD_1.2$tilt_corrected <- ifelse(
  is.na(FLY365_REC_MOT_LD_1.2$tilt_corrected),
  NA,
  ifelse(FLY365_REC_MOT_LD_1.2$tilt_corrected < 0, -FLY365_REC_MOT_LD_1.2$tilt_corrected, FLY365_REC_MOT_LD_1.2$tilt_corrected)
)

# Applicazione correzione tilt
FLY365_REC_MOT_LD_1.2 <- correct_altitude(FLY365_REC_MOT_LD_1.2)

# check results by plotting
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = laser_altitude_m, color = "Raw Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = laser_altitude_m_corrected, color = "Corrected Lidar"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `osd_data:relativeHeight[meters]`, color = "BAR"), linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(34,361, 518,604), linetype = "dashed", color = "black") +
  geom_vline(xintercept = c(79), linetype = "solid", color = "orange") +
  geom_vline(xintercept = c(558), linetype = "solid", color = "purple") +
  
  labs(title = "2022-08-12 08:06:40 (FLY_365 LOG_0071)",
       y = "Height",
       x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkorange", "darkgray")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "top", plot.title = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(1, nrow(FLY365_REC_MOT_LD_1.2), by = 10))

# heignt frame whale
mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[77:81])
sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[77:81])
mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[77:81])
sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[77:81])

#height frame pole
mean(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[556:560])
sd(FLY365_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`[556:560])
mean(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[556:560])
sd(FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned[556:560])


# Calcola Gap 
FLY365_REC_MOT_LD_1.2 <- GAP3(FLY365_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", list(c(61, 361), c(531,581), c(611,631)))
FLY365_REC_MOT_LD_1.2$diff_col

# Normalizza la differenza punto per punto, volo per volo
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = 1:length(diff_col), y = diff_col)) +
  geom_line(color = "#FF934F", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "solid", color = "#2D3142") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) - sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  geom_hline(yintercept = mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE) + sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE),
             linetype = "dashed", color = "#058ED9") +
  theme_minimal()

cv_365<- sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)/mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
sd_365<- sd(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)

# differenza relativa
FLY365_REC_MOT_LD_1.2$relative_diff <- FLY365_REC_MOT_LD_1.2$diff_col / mean(FLY365_REC_MOT_LD_1.2$diff_col, na.rm = TRUE)
ggplot(FLY365_REC_MOT_LD_1.2, aes(x = 1:length(relative_diff), y = relative_diff)) +
  geom_line(color = "#058ED9", size = 1) +
  labs(x = "Tempo", y = "Differenza (m)") +
  ylim(0, 2) +
  theme_minimal()

#### 
