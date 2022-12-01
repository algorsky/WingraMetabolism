library(tidyverse)
library(lubridate)

do<- read_csv("data/DO/buoy_DO.csv")
do_deep<- read_csv('data/DO/do_top_15.csv')

do<- do%>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%y %H:%M"))
do <- rbind(do, do_deep)
       
ggplot(do)+
  geom_line(aes(x = datetime, y = DO_mgL, color = buoy), alpha = 0.7)+
  ylab(expression(paste(O[2], " (mg " , L^-1,")")))+
  xlab("")+
  theme_bw(base_size = 16)+
  theme(legend.title=element_blank())
               

