library(tidyverse)
library(RcppRoll)

# ---- for reading in coded data ----
codedDataFolder <- './coded data/'
codedDataFiles <- dir(codedDataFolder)

# time in seconds
prestim.period.to.keep <- 1.0 
baseline.period <- 0.2 
stimulus.period.to.keep <- 1.0
inspection.window <- 0.05

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)
flag.threshold <- 3 # multiple of SD

n <- 1
coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
coded.femg.data <- read_csv(coded.data.file)


inspection.window.samples <- inspection.window/diff(coded.femg.data$Time.sec[1:2])


pp.femg.data <- coded.femg.data %>% 
  mutate(Zyg.z = scale(Zyg.mV),
         Cor.z = scale(Cor.mV),
         Zyg.z.range = roll_range(Zyg.z, inspection.window.samples, fill = NA),
         Cor.z.range = roll_range(Cor.z, inspection.window.samples, fill = NA),
         flag.Zyg = abs(Zyg.z.range) > flag.threshold,
         flag.Cor = abs(Cor.z.range) > flag.threshold) %>% 
  filter(trialNo > 0 & 
           stimTime.sec >= -prestim.period.to.keep &
           stimTime.sec < stimulus.period.to.keep) 

pp.femg.data %>% 
  filter(trialNo > 0 & trialNo <= 3) %>% 
  ggplot(aes(x = stimTime.sec)) +
  facet_grid(trialNo ~ .) +
  geom_line(aes(y = Zyg.z), colour = 'blue') +
  geom_line(aes(y = Cor.z), colour = 'red')

pp.femg.data %>% 
  filter(flag.Zyg | flag.Cor) %>% 
  distinct(trialNo)