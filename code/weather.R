#Load in packages
library(tidyverse) 
library(lubridate)
library(patchwork)


weather<- read_csv("data/weather/aos_weather.csv")


air_temp<-ggplot(dplyr::filter(weather, month(datetime) == 8))+
  geom_line(aes(x = datetime, y = air_temp))+
  geom_vline(xintercept = as.POSIXct("2022-08-07 00:00:00"), linetype = "dashed", alpha = 0.2)+
  xlab("Air Temperature (C°)")+
  ylab("")+
  theme_bw(base_size = 8)
wind<-ggplot(dplyr::filter(weather, month(datetime) == 8))+
  geom_col(aes(x = datetime, y = wind_speed))+
  geom_vline(xintercept = as.POSIXct("2022-08-07 00:00:00"), linetype = "dashed", alpha = 0.2)+
  xlab("Wind speed (m/s)")+
  ylab("")+
  theme_bw(base_size = 8)
precip<-ggplot(dplyr::filter(weather, month(datetime) == 8))+
  geom_hist(aes(x = datetime, y = accum_precip))+
  geom_vline(xintercept = as.POSIXct("2022-08-07 00:00:00"), linetype = "dashed", alpha = 0.2)+
  xlab("Accumulated Precipitation (mm)")+
  ylab("")+
  theme_bw(base_size = 8)

air_temp/wind/precip

