library(tidyverse)
library(readxl)

read_acq_text <- function(fileName, keepChannels) {

  # extract numbers from 2nd row to get sample duration in ms and convert to sampling rate
  sampRate.Hz <- fileName %>% 
    read_lines(skip = 1, n_max = 1) %>% 
    str_extract('[0-9]+') %>% 
    as.numeric() %>% 
    ( function(x) (1 / (x / 1000)) )
  
  # extract numbers from 3rd row to get number of channels in file
  raw.nChannels <- read_lines(fileName,
                              skip = 2, n_max = 1) %>%
    str_extract('[0-9]+') %>% 
    as.numeric()
  
  # read channel names 
  # just get the odd ones for the names (even rows are measurement units)
  raw.ChannelNames <- read_lines(fileName,
                                 skip = 3, n_max = 2*raw.nChannels)[seq(1,raw.nChannels*2, 2)]
  
  # get the column numbers for the data and stimulus channels by matching the names
  keepColumns <- raw.ChannelNames %>% 
    str_which(paste(keepChannels, collapse = '|'))
  
  # read in the data, keep only the wanted channels
  rawAcqData <- read_csv(fileName,
                           skip = raw.nChannels*2+5, col_names = FALSE)[,keepColumns]
  # put the names back
  names(rawAcqData) <- c(keepChannels)
  
  # add time variable
  rawAcqData <- rawAcqData %>% 
    mutate(Time.sec = seq(0,n()-1)/sampRate.Hz)
  
  return(rawAcqData)
}


library(RcppRoll)

denoise <- function(x, nsamples) {
  flagged <- rep_along(x,FALSE)
  corrected <- x
  # look forwards
  last.window.median <- median(x[(length(x)-nsamples):length(x)])
  next.n.med <- roll_medianl(x, nsamples, fill = last.window.median)
  # shift it one back so it doesn't include itself
  next.n.med <- c(next.n.med[-1], next.n.med[length(next.n.med)])
  
  # look backwards
  first.window.median <- median(x[1:nsamples])
  last.n.med <- roll_medianr(x, nsamples, fill = first.window.median)
  #shoft it one forwards so it doesn't include itself
  last.n.med <- c(last.n.med[1], last.n.med[-length(last.n.med)])
  
  # check if it's both different from the median of the previous n samples and 
  #from the median of the next n samples
  flagged <- x != last.n.med & x != next.n.med

  corrected[flagged] <- next.n.med[flagged]
  
  return(list('flagged' = flagged, 'corrected' = corrected))
  
}

clean_acq_stim_codes <- function(data, stimChannel, usedStimCodes, knownNoiseCodes) {
  diff_from_prev <- function(x) c(TRUE, diff(x) != 0)
  diff_from_next <- function(x) c(diff(x) != 0, TRUE)
  
  stimCodedData <- data %>% 
    mutate(
      # try to flag and correct unknown noise/debounce errors:
      Stim.flag.noise = denoise(.data[[stimChannel]], 100)$flagged,
      StimCode.corrected = denoise(.data[[stimChannel]], 100)$corrected,
      #set known noise codes to 0
      StimCode.corrected = replace(StimCode.corrected, StimCode.corrected %in% knownNoiseCodes, 0),
      # is the code different to the previous one? 
      transition.start = diff_from_prev(StimCode.corrected),
      transition.end = diff_from_next(StimCode.corrected),
      # is the stim code an unexpected one?
      unexpected = StimCode.corrected %in% usedStimCodes == FALSE
    )
  return(stimCodedData)
}

plot_stim_code_sequence <- function(data, stimVar) {
  unexpectedCodes <- unique(filter(data,unexpected)$StimCode.corrected)
  data %>% 
    ggplot(aes(Time.sec, .data[[stimVar]])) +
    geom_path() +
    geom_point(aes(colour = unexpected)) +
    scale_colour_manual(values = c('grey','red')) +
    labs(title = paste(unexpectedCodes, collapse = ', '), 
         y = 'Stimulus Code',
         x = 'Time (sec)') 
}

compare_stim_face_emoji_expt <- function(femgData, stimFile) {
  
  # read in stim file
  stimData <- read_tsv(stimFile, skip = 3) %>%
    mutate(StimCode = as.numeric(`Pic(num)`),
           Time.sec = Time/10000) %>% 
    filter(`Event Type` == 'Picture' & is.na(StimCode) == FALSE) %>% 
    select(StimCode, Time.sec)
  
  # time offset
  stim.Start <- stimData %>% 
    filter(StimCode > 0) %>% 
    summarise(start = min(Time.sec))
  
  femg.Start <- femgData %>% 
    filter(transition.start & StimCode.corrected > 0) %>% 
    summarise(start = min(Time.sec))
  
  offset <- femg.Start$start - stim.Start$start
  
  # stim sequence in femg file
  femg.Seq <- filter(femgData,transition.start)$StimCode.corrected
  
  # is the sequence in the femg data file the same as in the stim data file?
  synced <- sum(abs(stimData$StimCode - femg.Seq[-length(femg.Seq)])) == 0
  
  comparisonPlot <- ggplot() +
    geom_path(data = filter(femgData, transition.start | transition.end),
              aes(x = Time.sec, y = StimCode.corrected)) +
    geom_text(data = filter(femgData, transition.start & StimCode.corrected > 0),
              aes(x = Time.sec, y = StimCode.corrected + 40, label = StimCode.corrected),
              colour = 'black') +
    geom_point(data = stimData,
               aes(x = Time.sec + offset, y = StimCode),
               colour = 'blue', shape = 3) +
    geom_text(data = filter(stimData, StimCode > 0),
              aes(x = Time.sec + offset, y = StimCode + 20, label = StimCode),
              colour = 'blue')
  
  return(list('stimSeqData' = stimData, 'offset' = offset, 'synced' = synced, 'comparisonPlot' = comparisonPlot))
  
}

