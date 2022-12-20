library(tidyverse)
library(lubridate)
library(patchwork)

shallow_temp<- read_csv("data/temperature/shallow/temp_shallow.csv")
shallow_temp<- shallow_temp%>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%y %H:%M"))

mid_temp<- read_csv("data/temperature/mid/temp_mid.csv")
mid_temp<- mid_temp%>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%y %H:%M"))

deep_temp<- read_csv("data/temperature/deep/temp_deep.csv")
deep_temp<- deep_temp%>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%y %H:%M"))

shallow<-ggplot(shallow_temp)+
  geom_line(aes(x = datetime, y = Temp_C, color = as.factor(depth)))+
  ylab('Shallow Temp (°C)') +
  labs(fill = ((expression("Temperature " ( degree*C)))))+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())
mid<-ggplot((mid_temp))+
  geom_line(aes(x = datetime, y = Temp_C, color = as.factor(depth)))+
  ylab('Mid Temp (°C)') +
  labs(fill = ((expression("Temperature " ( degree*C)))))+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())

deep<-ggplot()+
  geom_line(data = dplyr::filter(deep_temp, depth !=1.5), aes(x = datetime, y = Temp_C, color = as.factor(depth)))+
  geom_line(data = dplyr::filter(deep_temp, depth ==1.5 & (datetime < "2022-09-16 00:00:00")), aes(x = datetime, y = Temp_C, color = as.factor(depth)))+
  geom_line(data = dplyr::filter(deep_temp, depth ==1.5 & (datetime > "2022-10-31 00:00:00")), aes(x = datetime, y = Temp_C, color = as.factor(depth)))+
  ylab('Deep Hole Temp (°C)') +
  labs(fill = ((expression("Temperature " ( degree*C)))))+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())

shallow/mid/deep
#ggsave('figures/temp.png', height = 10, width = 8, dpi = 500, units = 'in')

ggview(units = "in", height = 10, width = 8, dpi = 500)
