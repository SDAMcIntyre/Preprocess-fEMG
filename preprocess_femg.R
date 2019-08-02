library(tidyverse)
source('preprocess_femg_functions.R')


# ---- for reading in coded data ----
codedDataFolder <- './coded data/'
codedDataFiles <- dir(codedDataFolder)

IDformat <- '(s|p)[0-9]{3}'

# ---- for saving pre-processed data ----
outputFolder <- './preprocessed data/'

# time in seconds
prestim.sec <- 1.0 
baseline.sec <- 0.2 
stimulus.sec <- 1.0
win.sec <- 0.05

flag.threshold <- 3 # multiple of SD
prefixes <- c('Zyg', 'Cor')
rawVariables <- c('Zyg.mV', 'Cor.mV')

# ---- MAIN LOOP ---- 

n <- 3
coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
coded.femg.data <- read_csv(coded.data.file)

# z-score data and flag extreme transitions
pp.femg.data <- coded.femg.data %>% 
  scale_and_flag(prefixes, win.sec, flag.threshold) %>% 
  filter(trialNo > 0 & 
           stimTime.sec >= -prestim.sec &
           stimTime.sec < stimulus.sec) 

# summary of trials with flags and find alternate baselines
trials <- summarise_flagged_trials(pp.femg.data, prefixes, baseline.sec)
glimpse(trials)

# save the flagged trials for inspection
ID <- str_extract(codedDataFiles[n], IDformat)
for (varPF in prefixes) {
  if (trials[[varPF]]$nFlaggedTrials > 0) {
    
    for (thisTrial in trials[[varPF]]$allFlaggedTrials) {
      thisBaseline <- trials[[varPF]]$alt.baselines %>% 
        filter(trialNo == thisTrial) %>% select(2:3) %>% unlist() %>% na.omit()
      if (length(thisBaseline) > 0) { note <- 'flagged_bl'} else {
        note <- 'flagged'
      }
      
      windows(15,2)
      pp.femg.data %>% 
        filter(stimTime.sec >= -prestim.sec & 
                 trialNo %in% thisTrial) %>%
        plot_flagged_femg_trials(y = str_subset(rawVariables,varPF), 
                                 flag = paste(varPF, 'flagged', sep = '.'), win.sec,
                                 baseline = thisBaseline) +
        labs(title = paste(ID, varPF), x = 'Time (seconds)', y = str_subset(rawVariables,varPF))
      ggsave(paste0('./FOR INSPECTION/',ID,'_',thisTrial,'_',varPF,'_',note,'.png')) 
      dev.off()
      tibble(code = thisTrial, variable = varPF, note = note, from = codedDataFiles[n]) %>% 
        write_csv(paste0('./FOR INSPECTION/flagged_trials.csv'), append = TRUE)
    }
  }
}


