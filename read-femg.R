library(tidyverse)

rawDataFolder <- './rawdata/'
rawDataFiles <- dir(rawDataFolder)
femg.ChannelNames <- c('A-ZYG Processed',
                       'A-COR Processed')
stim.ChannelName <- 'Stimulus'
baseline.time.sec <- 0.5

raw.File <- paste0(rawDataFolder,rawDataFiles[1])

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
femg.data <- read_csv(raw.File, skip = raw.nChannels*2+3
                      )[,keepColumns]
# rename the columns
names(femg.data) <- c(stim.ChannelName, femg.ChannelNames)

# add a time variable
femg.data <- femg.data %>% 
  mutate(time.sec = seq(0,n()-1)/sampRate.Hz)

femg.data %>% 
  group_by(Stimulus) %>% 
  tally() %>% 
  mutate(sec = n/sampRate.Hz) %>% View()

windows()
femg.data[-1,] %>% 
  ggplot(aes(time.sec, Stimulus)) +
  geom_path()

# # old code
#   x$transition <- c(0,diff(as.logical(x$stim.trigger)))
#   x$trial <- 0
#   x$trial[x$transition > 0] <- seq_along(x$stim.trigger[x$transition > 0])
#   cued <- stimNames[x$stim.trigger[x$transition > 0]]
#   
#  # otherwise use the auto trigger
#       t.start <- x$time.sec[x$trial == trialNum & x$transition == 1]
#       t.next <- x$time.sec[x$trial == trialNum +1 & x$transition == 1]
#       if (length(t.next) == 0) t.next <- max(x$time.sec)
#       t.end <- x$time.sec[x$time.sec > t.start & x$time.sec < t.next & x$transition == -1]
# 
#     
#     x$trial[x$time.sec >= t.start -1 & x$time.sec < t.end] <- trialNum
#     x$phase[x$time.sec >= t.start & x$time.sec < t.end] <- 'stimulus'
#     x$phase[x$time.sec >= t.start -1 & x$time.sec < t.start] <- 'baseline'
#     x$cued[x$trial == trialNum] <- cued[trialNum]
#   
#   x <- x[is.na(x$phase)==FALSE,]
#   x <- x %>%
#     group_by(trial) %>%
#     mutate(time.sec = time.sec - min(time.sec[phase!='baseline']))
#   femgData.raw <- rbind(femgData.raw,x)
# }
# femgData.raw <- femgData.raw[,c(1:4,6:7,9:11)]
# femgData.raw <- merge(femgData.raw, commForfemg, all.x = TRUE)
# 
