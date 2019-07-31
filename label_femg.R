library(tidyverse)
library(readxl)
source('label_femg_functions.R')

# ---- for reading in raw data ----
rawDataFolder <- './raw data/'
rawDataFiles <- dir(rawDataFolder)
femg.ChannelNames <- c('A-ZYG Processed',
                       'A-COR Processed')
stim.ChannelName <- 'Stimulus'

# ---- for cleaning up stimulus codes ----
stim.OffCode <- 0
# read all stimulus codes used in the experiment from a file, add 0 to the list (for no stim)
femg.stimCodes <- read_excel('trigger stimulus coding.xlsx')$code %>% c(stim.OffCode)
# alternatively you can just type the list of codes into a vector e.g. stimCodes <- c(0,1,2,3)

# known noise codes to set to stim off code
femg.noiseCodes <- c(4)

# ---- for checking the stim sequence against a file from Presentation ----
stimSequenceFolder <- './stim sequences/'
stimSequenceFiles <- dir(stimSequenceFolder)
# regex format of the participant/session/file ID from the femg filename to match to the Presentation filename
IDformat <- '(s|p)[0-9]{3}'

# ---- for saving cleaned data ----
stimColName <- 'StimCode'
femgColNames <- c('Zyg.mV','Cor.mV')
outputFolder <- './coded data/'


# ---- MAIN LOOP ----
for (n in 1:length(rawDataFiles)) {
  # current file
  raw.femg.file <- paste0(rawDataFolder,rawDataFiles[n])
  # print progress
  print( paste( n, 'of', length(rawDataFiles), ':', rawDataFiles[n]))
  
  # read in the raw text file from acqKnowledge
  raw.femg.data <- read_acq_text(fileName = raw.femg.file, 
                                 keepChannels = c(stim.ChannelName, femg.ChannelNames))
  
  # clean up the codes in the data file, add transition labels, and find any remaining unexpected codes
  # add trial numbers and phase (prestim or stimulus) and stimulus relative time
  coded.femg.data <- clean_acq_stim_codes(data = raw.femg.data, 
                                        stimChannel = stim.ChannelName,
                                        usedStimCodes = femg.stimCodes,
                                        knownNoiseCodes = femg.noiseCodes,
                                        offCode = stim.OffCode)

  # check for unexpected stim codes
  raw.unexpectedCodes <- unique(filter(coded.femg.data,unexpected)$StimCode.corrected)
  if (length(raw.unexpectedCodes) > 0) {
    # plot the stimulus sequence
    windows(20,3)
    coded.femg.data %>% 
      #filter(Time.sec > 887.6 & Time.sec < 887.7) %>% 
      filter(transition.start | transition.end) %>% 
      plot_stim_code_sequence(stim.ChannelName)
    ggsave(paste0('./FOR INSPECTION/',ID,'_unexpected_codes.png'))
    dev.off()
    tibble(code = raw.unexpectedCodes, from = rawDataFiles[n]) %>% 
      write_csv(paste0('./FOR INSPECTION/unexpected_codes.csv'), append = TRUE)
  }

  # check against Presentation stim sequence
  # get ID to match acqKnowledge and Presentation files
  ID <- str_extract(raw.femg.file, IDformat)
  # corresponding stimulus sequence file
  stim.File <- paste0(stimSequenceFolder,
                      stimSequenceFiles[str_detect(stimSequenceFiles, ID)])
  
  # this function is experiment specific
  comparison <- compare_stim_face_emoji_expt(femgData = coded.femg.data,
                                             stimFile = stim.File)

  if (comparison$synced == FALSE) {
    # plot the two sequences
    windows(20,3)
    comparison$comparisonPlot # + coord_cartesian(xlim = c(630,800))
    ggsave(paste0('./FOR INSPECTION/',ID,'_unsynced.png'))
    dev.off()
  }
  
  # save the data
  if (length(raw.unexpectedCodes) == 0 & comparison$synced) {
    out.femg.data <- coded.femg.data %>% 
      select(c('Time.sec',
               'stimTime.sec',
               'StimCode.corrected',
               'trialNo',
               'phase',
               femg.ChannelNames))
    names(out.femg.data)[
      str_which(names(out.femg.data),
                paste(c('StimCode\\.corrected', femg.ChannelNames), collapse = '|')
                )] <- c(stimColName, femgColNames)
    out.femg.data %>% 
      write_csv(paste0(outputFolder,ID,'_coded.csv'))
  }
}
