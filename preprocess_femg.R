library(tidyverse)
source('preprocess_femg_functions.R')


# ---- for reading in coded data ----
codedDataFolder <- './coded data/'
codedDataFiles <- dir(codedDataFolder)


# time in seconds
prestim.sec <- 1.0 
baseline.sec <- 0.2 
stimulus.sec <- 1.0
win.sec <- 0.05

flag.threshold <- 3 # multiple of SD
prefixes <- c('Zyg', 'Cor')

# ---- MAIN LOOP ---- 

n <- 1
coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
coded.femg.data <- read_csv(coded.data.file)

win.samples <- win.sec/diff(coded.femg.data$Time.sec[1:2])

# z-score data and flag extreme values
pp.femg.data <- coded.femg.data %>% 
  scale_and_flag(prefixes, win.samples, flag.threshold) %>% 
  filter(trialNo > 0 & 
           stimTime.sec >= -prestim.sec &
           stimTime.sec < stimulus.sec) 

# how many flagged trials
pp.femg.data %>% 
  filter(stimTime.sec >= -baseline.sec) %>% 
  group_by(trialNo) %>% 
  summarise(flagged = any(flag.Zyg) ) %>% 
  group_by(flagged) %>% 
  tally() %>% 
  mutate(pc = 100*n/sum(n)) -> Zyg.n.flagged

# look at flagged zyg trials
if (Zyg.n.flagged %>% filter(flagged) %>% pull(n) > 0) {
  
  pp.femg.data %>% 
    filter(stimTime.sec >= -baseline.sec & flag.Zyg) %>% 
    distinct(trialNo) %>% 
    pull(trialNo) -> flagged.trials
  
  windows(20,20)
  pp.femg.data %>% 
    filter(stimTime.sec >= -baseline.sec & trialNo %in% flagged.trials) %>%
    plot_flagged_femg_trials(Zyg.mV, flag.Zyg, win.sec) +
    scale_x_continuous(breaks = function(x) seq(-baseline.sec, stimulus.sec, by = win.sec*4),
                       minor_breaks = function(x) seq(-baseline.sec, stimulus.sec, by = win.sec))
  ggsave(paste0('./FOR INSPECTION/',codedDataFiles[n],'_flagged_trials.png'))
  dev.off()
  tibble(code = flagged.trials, from = codedDataFiles[n]) %>% 
    write_csv(paste0('./FOR INSPECTION/flagged_trials.csv'), append = TRUE)
}

# try to find better baseline periods
if (Zyg.n.flagged %>% filter(flagged) %>% pull(pc) > 20) print(pc)

pp.femg.data %>% 
  filter(stimTime.sec >= 0 & flag.Zyg) %>% 
  distinct(trialNo) %>% 
  pull(trialNo) %>% 
  setdiff(x = flagged.trials) -> baseline.only

if (length(baseline.only > 0)) {
  pp.femg.data %>% 
    filter(trialNo == baseline.only[1] & stimTime.sec < 0) %>% 
    mutate( new.bl = roll_maxr(flag.Zyg, 200, fill = NA) == 0) %>% 
    filter(new.bl) %>% pull(stimTime.sec) %>% max() -> new.bl.end
  
  pp.femg.data %>% 
    filter(stimTime.sec >= -prestim.sec & trialNo %in% baseline.only) %>%
    plot_flagged_femg_trials(Zyg.mV, flag.Zyg, win.sec) +
    scale_x_continuous(breaks = function(x) seq(-prestim.sec, stimulus.sec, by = win.sec*4),
                       minor_breaks = function(x) seq(-prestim.sec, stimulus.sec, by = win.sec)) +
    geom_vline(xintercept = c(new.bl.end - baseline.sec, new.bl.end), colour = '#41ab5d', size = 1) 
}

# look at flagged cor trials
pp.femg.data %>% 
  filter(stimTime.sec >= -baseline.sec) %>%
  plot_flagged_femg_trials(Cor.z, flag.Cor)


flagged.baseline.trials <- pp.femg.data %>% 
  filter (flag.Zyg | flag.Cor)