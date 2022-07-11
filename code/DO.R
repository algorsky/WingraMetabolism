library(tidyverse)
library(lubridate)


do<- read_csv("data/buoy_DO.csv")


do<- do%>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%y %H:%M"))
               
               
ggplot(do)+
  geom_line(aes(x = datetime, y = DO_mgL, color = buoy))+
  ylab(expression(paste(O[2], " (mg " , L^-1,")")))+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())

  