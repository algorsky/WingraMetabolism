## remove everything from workspace
rm(list = ls())

# set wd to current dir of script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## libraries
library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)
library(glmtools)
library(scales)

## read hypsography
eg_nml <- read_nml(nml_file = '../data/bathymetry/nhdhr_143249705.nml')

H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
A <- eg_nml$morphometry$A

deep <- read_csv('../data/temperature/deep/temp_deep.csv')
mid <- read_csv('../data/temperature/mid/temp_mid.csv')
shallow <- read_csv('../data/temperature/shallow/temp_shallow.csv')

deep_df <- deep %>%
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  select(datetime, depth, Temp_C) %>%
  spread(key = depth, value = Temp_C)
colnames(deep_df) <- c('datetime', paste0('wtr_',unique(deep$depth)))

mid_df <- mid %>%
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  select(datetime, depth, Temp_C) %>%
  filter(!row_number() %in% c(13990, 13991, 13992)) %>%
  spread(key = depth, value = Temp_C)
colnames(mid_df) <- c('datetime', paste0('wtr_',unique(deep$depth)))

shallow_df <- shallow %>%
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  select(datetime, depth, Temp_C) %>%
  spread(key = depth, value = Temp_C)
colnames(shallow_df) <- c('datetime', paste0('wtr_',unique(deep$depth)))

deep_bf <- ts.buoyancy.freq(wtr = deep_df)
mid_bf <- ts.buoyancy.freq(wtr = mid_df)
shallow_bf <- ts.buoyancy.freq(wtr = shallow_df)

ggplot() +
  geom_line(data = deep_bf, aes(datetime, n2, col = 'deep')) + 
  geom_point(data = deep_bf, aes(datetime, n2, col = 'deep')) +
  geom_line(data = mid_bf, aes(datetime, n2, col = 'middle')) + 
  geom_point(data = mid_bf, aes(datetime, n2, col = 'middle')) +
  geom_line(data = shallow_bf, aes(datetime, n2, col = 'shallow')) + 
  geom_point(data = shallow_bf, aes(datetime, n2, col = 'shallow')) +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  theme_minimal()

g.top <- ggplot() +
  geom_line(data = deep_df, aes(datetime, wtr_0, col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, wtr_0, col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, wtr_0, col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, wtr_0, col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, wtr_0, col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, wtr_0, col = 'shallow')) +
  theme_minimal()

g.bottom <- ggplot() +
  geom_line(data = deep_df, aes(datetime, wtr_2, col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, wtr_2, col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, wtr_2, col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, wtr_2, col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, wtr_0.5, col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, wtr_0.5, col = 'shallow')) +
  theme_minimal()


g.dens <- ggplot() +
  geom_line(data = deep_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, water.density(wtr_0.5) - water.density(wtr_0), col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, water.density(wtr_0.5) - water.density(wtr_0),col = 'shallow')) +
  geom_hline(yintercept = 0.1,  linetype='dashed') +
  xlab('') + ylab('density difference') +
  theme_minimal(); g.dens


deep_df2 <- deep %>%
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  select(datetime, depth, Temp_C) #%>%
#  filter(depth != 0.5) # 0.5 m depth seems to be faulty

m.df <- reshape2::melt(deep_df2, "datetime")

g1 <- ggplot(deep_df2, aes((datetime), depth)) +
  geom_raster(aes(fill = as.numeric(Temp_C)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(20,30),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'Temp [degC]')+
  scale_y_reverse() ; g1

mid_df2 <- mid %>%
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  select(datetime, depth, Temp_C) 


g1 <- ggplot(mid_df2, aes((datetime), depth)) +
  geom_raster(aes(fill = as.numeric(Temp_C)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(20,30),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'Temp [degC]')+
  scale_y_reverse() ; g1

do <- read_csv('../data/buoy_DO.csv')

g.do <- ggplot(do %>% 
               mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')), 
             aes((datetime), as.numeric(DO_mgL), col = buoy)) +
  geom_line() + geom_point() +
  theme_minimal()  +xlab('Time') +
  ylab('DO [mg/L]'); g.do

g.top / g.bottom / g.dens / g.do

do.df = do %>% 
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  filter(buoy == 'shallow')
idx <- match(do.df$datetime, shallow_df$datetime)

shallow_df = shallow_df %>%
  mutate('dens' = water.density(wtr_0.5) - water.density(wtr_0))

do.df$dens = shallow_df$dens[(idx)]

ggplot(do.df %>%
         mutate(day = factor(as.Date(datetime)),
                hour = lubridate::hour(datetime)), aes(DO_mgL, dens, group= day, col = hour)) +
  geom_point() +
  facet_wrap(~ day) +
  xlab('DO [mg/L]') + ylab('Density diff. [g/m3]') + 
  ggtitle('Shallow') +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_minimal()



do.df = do %>% 
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  filter(buoy == 'deep')
idx <- match(do.df$datetime, deep_df$datetime)

deep_df = deep_df %>%
  mutate('dens' = water.density(wtr_2) - water.density(wtr_0))

do.df$dens = deep_df$dens[(idx)]

ggplot(do.df %>%
         mutate(day = factor(as.Date(datetime)),
                hour = lubridate::hour(datetime)), aes(DO_mgL, dens, group= day, col = hour)) +
  geom_point() +
  facet_wrap(~ day) +
  xlab('DO [mg/L]') + ylab('Density diff. [g/m3]') + 
  ggtitle('Deep') +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_minimal()


do.df = do %>% 
  mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %H:%M')) %>%
  filter(buoy == 'mid')
idx <- match(do.df$datetime, mid_df$datetime)

mid_df = mid_df %>%
  mutate('dens' = water.density(wtr_2) - water.density(wtr_0))

do.df$dens = mid_df$dens[(idx)]

ggplot(do.df %>%
         mutate(day = factor(as.Date(datetime)),
                hour = lubridate::hour(datetime)), aes(DO_mgL, dens, group= day, col = hour)) +
  geom_point() +
  facet_wrap(~ day) +
  xlab('DO [mg/L]') + ylab('Density diff. [g/m3]') + 
  ggtitle('Mid') +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_minimal()
