library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
install.packages("hms")
library(hms)

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

# Transfor GPS.dateTimeStamp column from character to "POSIXct" "POSIXt" 
LOG_0021_R$GPS.dateTimeStamp <- as.POSIXct(LOG_0021_R$GPS.dateTimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Join FLY and LOG columns 
FLY285_REC_MOT_LD <- left_join(FLY285_REC_MOT_ready, LOG_0021_R, by = "GPS.dateTimeStamp")




