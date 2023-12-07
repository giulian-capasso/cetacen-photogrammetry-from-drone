# Tilt Laser analysis 
plot_tilt_285 <- ggplot(FLY285_REC_MOT_LD_1.2, aes(x = seq_along(GPS.dateTimeStamp))) +
  geom_line(aes(y = FLY285_REC_MOT_LD_1.2$laser_altitude_m, color = "Variable 1"), size = 0.7) +
  geom_line(aes(y = FLY285_REC_MOT_LD_1.2$tilt_deg, color = "Variable 2"), size = 0.7) +
  scale_color_manual(values = c("Variable 1" = "blue", "Variable 2" = "darkorange")) +
  labs(x = "Time", y = "Altezza Laser / Tilt Laser") +
  theme_minimal()

plot_tilt_285 + scale_y_continuous(
  sec.axis = sec_axis(~./5, name = "Variable 2", breaks = seq(-5, 5, 1)))
