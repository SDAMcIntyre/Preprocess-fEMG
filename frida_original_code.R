#### PART 1 ####

source('label_femg_functions.R')
library(plotly)

#### read in the raw data ####

# For this demo, we are looking at a recording from a single session
raw.femg.file <- 'example_experiment/1 raw data/sub_005_f.txt'

# Provide the channels in the raw data file that we are interested in
femg.ChannelNames <- c('Corr Processed',
                       'Zyg Processed',
                       'Lev Processed')
stim.ChannelName <- 'Marker'

# read the data file
raw.femg.data <- read_acq_text(fileName = raw.femg.file, 
                               delim = ',',
                               keepChannels = c(stim.ChannelName, femg.ChannelNames))

# Look at the data, it has only the channels we told it to keep, 
# and a new Time.sec channel based on the sampling rate reported 
# in the raw data file:
glimpse(raw.femg.data)

##### clean up the stimulus codes ####

# voltages on the stimulus/marker channel that indicate 
# what the stimulus was, chosen by the experimenter:

femg.stimCodes <- c(111:118, 121:128, 131:138, 141:148, 151:158, 
                    161:168, 171:178, 11:18, 21:28, 31:38, 41:48, 
                    51:58,  61:68,  71:78, 224)

# Look for errors in the marker channel and try to fix them

labelled.femg.data <- clean_acq_stim_codes(femgData = raw.femg.data,
                                           stimChannel = stim.ChannelName,
                                           usedStimCodes = femg.stimCodes) 

# Look at the labelled data. Now we have some additional variables

glimpse(labelled.femg.data)

# Stim.flag.noise = was the voltage on the Marker channel flagged
#                   by the function as possible noise? TRUE/FALSE
# StimCode.corrected = the corrected stimulus codes
# unexpected = was the corrected stim code still something unexpected,
#               i.e. not one of the values in femg.stimCodes? TRUE/FALSE

# now check for unexpected stim codes

labelled.femg.data %>% 
  filter(unexpected) %>% 
  group_by(StimCode.corrected) %>% 
  tally()

# An uexpected voltage of 1 appears  896 times and the function
# didn't catch it. Plot the channel so we can see what's going on:

labelled.femg.data %>% 
  plot_stim_code_sequence('StimCode.corrected') %>% 
  ggplotly()

# Try zooming in on the red dots. It looks like it was produced by 
# some kind of electrical artifact so it should be safe
# to set these to 0. 

# run the clean-up again, this time telling it to set 1s to 0s 
# by adding the parameter knownNoiseCodes = c(1)
# you can add as many as you like if you find more, e.g. c(1,4,254)

labelled.femg.data <- clean_acq_stim_codes(femgData = raw.femg.data,
                                           stimChannel = stim.ChannelName,
                                           usedStimCodes = femg.stimCodes,
                                           knownNoiseCodes = c(1)) 

# check again for unexpected stim codes

labelled.femg.data %>% 
  filter(unexpected) %>% 
  group_by(StimCode.corrected) %>% 
  tally()

# now there are no unexpected values on the marker channel

labelled.femg.data %>% 
  plot_stim_code_sequence('StimCode.corrected') %>% ggplotly()


##### filling the stimulus period ####

# in this recording the stimulus markers were brief pulses
# at the stimulus onset, but we want the labels to apply
# the whole time the stimulus is switched on. We know that the 
# stimulus lasts 6 seconds so we "fill" the stimCode.corrected
# channel for the full duration of the stimulus


labelled.femg.data <- labelled.femg.data %>% 
  fill_stim_codes('StimCode.corrected', stimDuration = 6)

labelled.femg.data %>% 
  plot_stim_code_sequence('StimCode.filled') %>% ggplotly()


##### check against the expected stimulus sequence ####

# next, if we have a log file from the stimulus presentation 
# software, we can check that the sequences of stimuli match
# we have a logfile from Presentation

stim.File <- 'example_experiment/0 stim sequences/sub_f_005-emoji.log'

# We use a function which is specific to this experiment, and needs to be 
# adapted for different experiments.

# here we fill the stim codes based on the log file using fillStimCodes = TRUE
comparison <- compare_stim_face_emoji_expt(femgData = labelled.femg.data, 
                                           stimChannel = 'StimCode.corrected', 
                                           stimFile = stim.File,
                                           fillStimCodes = TRUE) 

# alternate call using our pre-filled codes
# comparison <- compare_stim_face_emoji_expt(femgData = labelled.femg.data,
#                                            stimChannel = 'StimCode.filled',
#                                            stimFile = stim.File)

# this produces a list of several objects that help with comparing the 
# expected and recorded stimulus sequence of the session

# are they synced
comparison$synced

# plot the two sequences
ggplotly(comparison$comparisonPlot)

# the timing is never exact, this shows that there were only small 
# differences between the timing of the expected and recorded times
# when the stimuli started and ended
print('Stimulus start offsets (seconds):'); print(summary(comparison$startOffsets))
print('Stimulus end offsets (seconds):'); print(summary(comparison$endOffsets))

# grab the filled data based on the stimulus log
labelled.femg.data <- comparison$stimFilledData

labelled.femg.data %>% 
  plot_stim_code_sequence('StimCode.filled') %>% ggplotly()

##### add session variables ####
# add trial numbers within the session
# add phase info (pre-stim/stim)
# add time relative to stimulus onset
labelled.femg.data <- labelled.femg.data %>% 
  add_session_variables('StimCode.filled') 

glimpse(labelled.femg.data)

# just choose the variables we want to keep
out.femg.data <- labelled.femg.data %>% 
  select(c('Time.sec',
           'stimTime.sec',
           'StimCode.filled',
           'trialNo',
           'phase',
           all_of(femg.ChannelNames))) 

out.femg.data %>% glimpse()

# save the data file
out.femg.data %>% 
  write_csv('sub_005_f_labelled.csv')



#### PART 2 ####

source('preprocess_femg_functions.R')

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

##### parameters for automatic artifact rejection ####

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

##### z-score data and flag extreme transitions ####

flagged.femg.data.all <- labelled.femg.data %>% 
  scale_and_flag(prefixes, win.sec, flag.threshold) 

##### plot the distributions for each muscle ####

# and see where 1, 2, and 3 x SD cut-offs are
flagged.femg.data.all %>% 
  plot_histograms(prefixes) %>% 
  ggplotly()


##### select only the data we're interested in ####

flagged.femg.data <- flagged.femg.data.all %>% 
  filter(trialNo > 0 & 
           stimTime.sec >= -prestim.sec &
           stimTime.sec < stimulus.sec) 

##### new baseline periods ####

# summarise trials with flags and find alternate baselines
trials <- summarise_flagged_trials(flagged.femg.data, prefixes, baseline.sec)
glimpse(trials)

# record updated baseline periods based on "trials" info above
# in new variables Zyg.bl.start, Zyg.bl.end etc.
flagged.femg.data <- flagged.femg.data %>% 
  update_baseline_periods(prefixes, trials, baseline.sec)


##### plot data from a single trial ####

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

# save all the flagged trials (you could also do this for every trial)
# this takes some time, so be patient
for (t in trials$flaggedOnAnyMuscle) { 
 thisplot <- flagged.femg.data %>%
   plot_flagged_trial(flaggedTrial = t, prefixes, win.sec, prestim.sec) %>%
   ggplotly()
 filename <- paste0('sub_005_f_',t,'.html')
 saveWidgetFix(thisplot, filename) 
}

##### finalise and save the data ####

# separate data by muscle, fix the "phase" variable based on 
# the new baselines, and replace data from excluded trials with NA
pp.femg.data <- flagged.femg.data %>% 
  finalise_data(prefixes)

# save the preprocessed data, one file for each muscle, 
# each row is one sample, but only baseline and stimulus 
# phases are kept
for (muscle in prefixes) {
  pp.femg.data[[muscle]] %>% 
    write_csv(paste0('sub_005_f_',muscle,'_preprocessed.csv'))
}

# calculate means and differences (stim - baseline)
summarised.femg.data <- pp.femg.data %>% 
  means_and_diffs(prefixes)

# save the summarised data, one file for each muscle,
# each row is one trial, excluded trials show NaN
for (muscle in prefixes) {
  summarised.femg.data[[muscle]] %>% 
    write_csv(paste0('sub_005_f_',muscle,'_means.csv'))
}

