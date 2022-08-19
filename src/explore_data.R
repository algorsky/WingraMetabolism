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

ggplot() +
  geom_line(data = deep_df, aes(datetime, wtr_0, col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, wtr_0, col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, wtr_0, col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, wtr_0, col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, wtr_0, col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, wtr_0, col = 'shallow')) +
  theme_minimal()

ggplot() +
  geom_line(data = deep_df, aes(datetime, wtr_2, col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, wtr_2, col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, wtr_2, col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, wtr_2, col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, wtr_0.5, col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, wtr_0.5, col = 'shallow')) +
  theme_minimal()


ggplot() +
  geom_line(data = deep_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'deep')) + 
  geom_point(data = deep_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'deep')) +
  geom_line(data = mid_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'middle')) + 
  geom_point(data = mid_df, aes(datetime, water.density(wtr_2) - water.density(wtr_0), col = 'middle')) +
  geom_line(data = shallow_df, aes(datetime, water.density(wtr_0.5) - water.density(wtr_0), col = 'shallow')) + 
  geom_point(data = shallow_df, aes(datetime, water.density(wtr_0.5) - water.density(wtr_0),col = 'shallow')) +
  geom_hline(yintercept = 0.1,  linetype='dashed') +
  xlab('') + ylab('density difference') +
  theme_minimal()
