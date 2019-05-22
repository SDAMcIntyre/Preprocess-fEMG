library(tidyverse)
library(readxl)

possibleStimCodes <- read_excel('trigger stimulus coding.xlsx')$code %>% c(0)
noiseCodes <- c(1,4,8)
stimSequenceFolder <- './stim sequences/'
stimSequenceFiles <- dir(stimSequenceFolder)
stim.CodeColumn <- 5

rawDataFolder <- './raw data/'
rawDataFiles <- dir(rawDataFolder)
femg.ChannelNames <- c('A-ZYG Processed',
                       'A-COR Processed')
stim.ChannelName <- 'Stimulus'
baseline.time.sec <- 0.5

IDformat <- '(s|p)[0-9]{3}'

# main loop
for (n in 1:length(rawDataFiles)) {
  # current file
  raw.File <- paste0(rawDataFolder,rawDataFiles[n])
  # print progress
  print( paste( n, 'of', length(rawDataFiles), ':', rawDataFiles[n]))
  
  # get the participant/session/file ID from the filename
  ID <- str_extract(raw.File, IDformat)
  
  # matching stimulus sequence file
  stim.File <- paste0(stimSequenceFolder,
    stimSequenceFiles[str_detect(stimSequenceFiles, ID)])
  
  # read in stim file
  stim.Seq <- read_tsv(stim.File, skip = 5, col_names = FALSE)[,stim.CodeColumn]
  names(stim.Seq) <- 'X'
  stim.Seq <- stim.Seq$X %>% 
    as.numeric() %>% 
    na.omit()
  
  # extract numbers from 2nd row to get sample duration in ms and convert to sampling rate
  sampRate.Hz <- read_lines(raw.File, skip = 1, n_max = 1) %>% 
    str_extract('[0-9]+') %>% 
    as.numeric() %>% 
    ( function(x) (1 / (x / 1000)) )
   
  # extract numbers from 3rd row to get number of channels in file
  raw.nChannels <- read_lines(raw.File, skip = 2, n_max = 1) %>% 
    str_extract('[0-9]+') %>% 
    as.numeric()
  
  # read channel names 
  # just get the odd ones for the names (even rows are measurement units)
  raw.ChannelNames <- read_lines(raw.File, skip = 3, n_max = 2*raw.nChannels
                               )[seq(1,raw.nChannels*2, 2)]
  
  # get the column numbers for the femg and stimulus channels by matching the names
  keepColumns <- c(str_which(raw.ChannelNames, stim.ChannelName),
                   str_which(raw.ChannelNames, paste(femg.ChannelNames, collapse = '|')) )
  
  # read in the data, keep only the stim and femg channels
  femg.data <- read_csv(raw.File, skip = raw.nChannels*2+5, col_names = FALSE
                        )[,keepColumns]
  # rename the columns
  names(femg.data) <- c(stim.ChannelName, femg.ChannelNames)
  
  # add time and stim transition variables
  femg.data <- femg.data %>% 
    mutate(time.sec = seq(0,n()-1)/sampRate.Hz,
           transition = c(1,diff(Stimulus)) != 0 | c(diff(Stimulus),1) != 0,
           badStim = Stimulus %in% possibleStimCodes == FALSE
           )

  raw.badStimCodes <- unique(filter(femg.data,badStim)$Stimulus)
  
  if (length(raw.badStimCodes) > 0) {
    # plot the stimulus sequence
    windows(20,3)
    femg.data %>% 
      filter(transition) %>% 
      ggplot(aes(time.sec, Stimulus)) +
      geom_path() +
      geom_point(aes(colour = badStim)) +
      scale_colour_manual(values = c('grey','red')) +
      labs(title = paste(raw.badStimCodes, collapse = ', '))
    ggsave(paste0('./bad stim codes/',rawDataFiles[n],'.png'))
    dev.off()
    tibble(code = raw.badStimCodes, from = rawDataFiles[n]) %>% 
      write_csv(paste0('./bad stim codes/bad_codes.csv'), append = TRUE)
  }
}
