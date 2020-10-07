source('preprocess_femg_functions.R')
library(plotly)

# Automatic artifact rejection is applied according to a procedure developed and validated in [1]. A trial will be rejected if the range of the data within a 50 ms long window exceeds 3 x SD of the data from each participant’s full set of trials for each muscle.
# To reduce exclusions, the trials are checked in which the exclusion was due to extreme values occurring within the 200ms baseline period but not during the stimulus period. For these selected trials, an alternate baseline period is used if it does not include extreme values, and these trials will be included. The alternate baseline period will be the nearest 200ms window to the stimulus beginning no more than 1000ms before stimulus onset, examined in 50ms bins.

# [1] Künecke, J. et al. (2014) ‘Facial EMG responses to emotional expressions are related to
# emotion perception ability’, PLoS ONE, 9(1). doi: 10.1371/journal.pone.0084053.

# read in the file we created in part 1
labelled.data.file <- 'sub_005_f_labelled.csv' 
labelled.femg.data <- read_csv(labelled.data.file, col_types = cols()) %>% 
  rename('Zyg.mV' = `Zyg Processed`,
         'Cor.mV' = `Corr Processed`,
         'Lev.mV' = `Lev Processed`,
         'StimCode' = 'StimCode.filled')

prefixes <- c('Zyg', 'Cor', 'Lev')
rawVariables <- c('Zyg.mV', 'Cor.mV', 'Lev.mV')

#### parameters for automatic artifact rejection ####

# Enter the time periods that you are interested in:
# (time in seconds)

# we will try to use the 200 ms immediately before the stimulus
baseline.sec <- 0.2 

# if the baseline includes artifacts, look for nearby baselines within this window
prestim.sec <- 1.0 

# how much of the stimulus period do you want to look at?
stimulus.sec <- 1.0

# the window in which to look for range > 3xSD
win.sec <- 0.05

# multiple of SD to use as the threshold
flag.threshold <- 3 

# z-score data and flag extreme transitions
flagged.femg.data.all <- labelled.femg.data %>% 
  scale_and_flag(prefixes, win.sec, flag.threshold) 
  
# plot the distributions for each muscle
# and see where 1, 2, and 3 x SD cut-offs are
flagged.femg.data.all %>% 
  plot_histograms(prefixes) %>% 
  ggplotly()


# select only the data we're interested in
flagged.femg.data <- flagged.femg.data.all %>% 
  filter(trialNo > 0 & 
           stimTime.sec >= -prestim.sec &
           stimTime.sec < stimulus.sec) 


# summarise trials with flags and find alternate baselines
trials <- summarise_flagged_trials(flagged.femg.data, prefixes, baseline.sec)
glimpse(trials)

# save updated baseline periods based on "trials" info above
flagged.femg.data <- flagged.femg.data %>% 
  update_baseline_periods(prefixes, trials, baseline.sec)

# all the trial numbers that have been flagged on any muscle
(trials$flaggedOnAnyMuscle)

# plot one of them
flagged.femg.data %>% 
  plot_flagged_trial(flaggedTrial = 98, prefixes, win.sec, prestim.sec)

# it looks like the new baseline includes artifact, but this is because
# the shaded area flagged as artifact highlights any data point where
# the window centred on it exceeds the 3xSD threshold, while the
# right-most value in the updated baseline is the right-most value
# of a window that does not exceed the threshold

# separate data by muscle, fix the "phase" variable based on 
# the new baselines, and replace data from excluded trials with NA
pp.femg.data <- flagged.femg.data %>% 
  preprocess_data(prefixes)

# save the preprocessed data, each row is one sample, but only  
# baseline and stimulus phases are kept
for (muscle in prefixes) {
  pp.femg.data[[muscle]] %>% 
    write_csv(paste0('sub_005_f_',muscle,'_preprocessed.csv'))
}
  
# calculate means and differences (stim - baseline)
summarised.femg.data <- pp.femg.data %>% 
  means_and_diffs(prefixes)

# save the summarised data, each row is one trial
for (muscle in prefixes) {
  summarised.femg.data[[muscle]] %>% 
  write_csv(paste0('sub_005_f_',muscle,'_means.csv'))
}

