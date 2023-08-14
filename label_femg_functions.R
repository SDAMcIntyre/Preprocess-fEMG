library(tidyverse)
library(readxl)
library(RcppRoll)
library(htmlwidgets)

read_acq_text <- function(fileName, delim, keepChannels) {
  print(paste('Reading file', fileName))
  # extract numbers from 4th row to get sample duration in ms and convert to sampling rate
  sampRate.Hz <- fileName %>% 
    read_lines(skip = 3, n_max = 1) %>% 
    str_extract('[0-9]+') %>% 
    as.numeric() %>% 
    ( function(x) (1 / (x / 1000)) )
  
  # extract numbers from 5th row to get number of channels in file
  raw.nChannels <- read_lines(fileName,
                              skip = 4, n_max = 1) %>%
    str_extract('[0-9]+') %>% 
    as.numeric()
  
  # read channel names 
  # just get the odd ones for the names (even rows are measurement units)
  raw.ChannelNames <- read_lines(fileName,
                                 skip = 5, 
                                 n_max = 2*raw.nChannels)[seq(1,raw.nChannels*2, 2)]
  
  # get the column numbers for the data and stimulus channels by matching the names
  keepColumns <- raw.ChannelNames %>% 
    str_which(paste(keepChannels, collapse = '|'))
  
  # read in the data, keep only the wanted channels
  rawAcqData <- read_delim(fileName,
                           delim = delim,
                         skip = raw.nChannels*2+7, 
                         col_names = FALSE,
                         col_types = cols())[,keepColumns]
  # put the names back
  names(rawAcqData) <- raw.ChannelNames[keepColumns]
  
  # add time variable
  rawAcqData <- rawAcqData %>% 
    mutate(Time.sec = seq(0,n()-1)/sampRate.Hz)
  
  return(rawAcqData)
}

diff_from_prev <- function(x) { c(TRUE, diff(x) != 0) }
diff_from_next <- function(x) { c(diff(x) != 0, TRUE) }

add_transitions <- function(femgData, stimChannel) {
  femgData %>%
    ungroup() %>% 
    mutate(
      transition.start = diff_from_prev(.[[stimChannel]]),
      transition.end = diff_from_next(.[[stimChannel]])
    )
}

denoise <- function(x, nsamples = 'auto', ignoreValues = c(0)) {
  if (nsamples == 'auto') {
    # look at the data to find out how long the stimulus signal is
    runs <- rle(x)
    raw.stim.runs <- tibble(lengths = runs$lengths,
                            stimcode = runs$values)
    stimcode.nsamples <- raw.stim.runs %>% 
      filter(stimcode %in% ignoreValues == FALSE) %>% 
      pull(lengths) %>% median()
    nsamples = ceiling(stimcode.nsamples/2)
  }
  
  # look forwards
  #get the median of the last window in the dataset
  last.window.median <- median(x[(length(x)-nsamples):length(x)])
  
  # get the rolling median of the data, left-aligned, pad it with the 
  # median of the last window so that it is the same length as the data
  next.n.med <- roll_medianl(x, nsamples, fill = last.window.median)
  
  # shift the medians back by one position so that each value in the 
  # dataset aligns with the median of the window of data ahead of it ("looking ahead")
  next.n.med <- c(next.n.med[-1], next.n.med[length(next.n.med)])
  
  # look backwards (do the same as above but right aligned medians)
  first.window.median <- median(x[1:nsamples])
  prev.n.med <- roll_medianr(x, nsamples, fill = first.window.median)
  #shift it one forwards so it doesn't include itself
  prev.n.med <- c(prev.n.med[1], prev.n.med[-length(prev.n.med)])
  
  # check if it's both different from the median of the previous n samples and 
  #from the median of the next n samples
  flagged <- x != prev.n.med & x != next.n.med

  corrected <- x
  corrected[flagged] <- next.n.med[flagged]
  
  return(list('flagged' = flagged, 'corrected' = corrected))
  
}

clean_acq_stim_codes <- function(femgData, stimChannel, usedStimCodes, knownNoiseCodes = c(), offCode = 0, nsamples = 'auto') {

  denoised <- denoise(femgData[[stimChannel]], nsamples = nsamples, 
                      ignoreValues = c(offCode, knownNoiseCodes))
  stimCodedData <- femgData %>% 
    mutate(
      # try to flag and correct unknown noise/debounce errors:
      Stim.flag.noise = denoised$flagged,
      StimCode.corrected = denoised$corrected,
      #set known noise codes to 0
      StimCode.corrected = replace(StimCode.corrected, # values to replace
                                   StimCode.corrected %in% knownNoiseCodes, # condition
                                   offCode), # replace with
      # is the stim code an unexpected one?
      unexpected = StimCode.corrected %in%  c(offCode, usedStimCodes) == FALSE
    )
  return(stimCodedData)
}  

plot_stim_code_sequence <- function(femgData, stimChannel) {
  unexpectedCodes <- femgData %>% filter(unexpected) %>% 
    .[[stimChannel]] %>% unique()
  femgData %>%
    add_transitions(stimChannel) %>%
    filter(transition.start | transition.end) %>% 
    ggplot(aes(Time.sec, .data[[stimChannel]])) +
    geom_path() +
    geom_point(aes(colour = unexpected)) +
    scale_colour_manual(values = c('grey','red')) +
    labs(title = paste(unexpectedCodes, collapse = ', '), 
         y = 'Stimulus Code',
         x = 'Time (sec)') 
}


fill_stim_codes <- function(femgData, stimChannel, stimDuration) {
  
  starts <- femgData %>% 
    add_transitions(stimChannel) %>% 
    filter(transition.start & .[[stimChannel]] != 0) %>% 
    select(c('Time.sec', all_of(stimChannel)))
  
  femgData <- femgData %>% 
    mutate(StimCode.filled = .[[stimChannel]])
  
  for (n in seq_along(starts$Time.sec)) {
    tofill <- which(
      femgData$Time.sec > starts$Time.sec[n] & 
        femgData$Time.sec <= starts$Time.sec[n] + stimDuration
    )
    femgData[tofill,'StimCode.filled'] <- starts[n, stimChannel] %>% pull()
  }
  
  femgData
  
}

compare_stim_face_emoji_expt <- function(femgData, stimChannel, offCode = 0, stimFile, fillStimCodes = FALSE) {
  
  # read in stim file
 #this would probably work if you had normal decimals, but we don't.
  #stimData <- suppressWarnings(
   # read_tsv(stimFile, skip = 3, col_types = '__c_n__d__d_____')) %>%
    #rename('StimCode' = `Pic(num)`) %>% 
    #mutate(Start.sec = Time/10000,
     #      Duration.sec = Duration/10000,
      #     End.sec = Start.sec + Duration.sec ) %>% 
    #filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 

  #Instead we'll use the following to read in the stim file. This transfers the pic(num) values from hexadecimals to decimals when creating the StimCode variable.
  stimData <- suppressWarnings(
    read_tsv(stimFile, skip = 2)) %>% 
    mutate('StimCode' = strtoi(`Pic(num)`, base=16),
           Start.sec = Time/10000,
           Duration.sec = Duration/10000,
           End.sec = Start.sec + Duration.sec ) %>% 
    filter(`Event Type` == 'Picture' & !is.na(StimCode) & StimCode != offCode) 
  
  # get time of first stimulus in stimulus file
  stim.StartTime <- stimData %>% filter(StimCode != offCode) %>% 
    summarise(start = min(Start.sec)) %>% pull(start)
  
  # get time of first stimulus in femg file
  femgData <- femgData %>% 
    add_transitions(stimChannel)
  
  femg.StartTime <- femgData %>% 
    filter(transition.start & .[[stimChannel]] != offCode) %>% 
    summarise(start = min(Time.sec)) %>% pull(start)
  
  # time offset between femg and stim files
  offset <- femg.StartTime - stim.StartTime
  
  # align stimulus sequence to first stim in femg file
  stimAligned <- stimData %>% 
    select(StimCode, Start.sec, Duration.sec, End.sec) %>% 
    mutate(Start.sec = Start.sec + offset,
           End.sec = End.sec + offset)
  
  # stim sequence in femg file
  femg.Seq <- femgData %>% 
    filter(transition.start & .[[stimChannel]] != offCode) %>% 
    .[[stimChannel]]
  
  # is the sequence in the femg data file the same as in the stim data file?
  synced <- length(stimData$StimCode) == length(femg.Seq)
  if (synced) synced <- sum(abs(stimData$StimCode - femg.Seq)) == 0
  
  output <- list('stimSeqData' = stimAligned, 
                 'synced' = synced)
  
  # onsets for all stimuli in femg file
  femg.Start <- femgData %>% 
    filter(transition.start & .[[stimChannel]] != offCode) %>% 
    pull(Time.sec)

  # offsets for all stimuli in femg file
  femg.End <- femgData %>% 
    filter(transition.end & .[[stimChannel]] != offCode) %>% 
    pull(Time.sec) 
  
  if (synced) {
    # does the timing of the start of the stimulus match?
    startOffsets <- stimAligned$Start.sec - femg.Start
    output$startOffsets <- startOffsets
      
    # does the timing of the end of the stimulus match?
    endOffsets <- stimAligned$End.sec - femg.End 
    output$endOffsets <- endOffsets
  }
  
  comparisonPlot <- ggplot() +
    geom_path(data = filter(femgData, transition.start | transition.end),
              aes(x = Time.sec, y = .data[[stimChannel]])) +
    geom_text(data = filter(femgData, transition.start & .data[[stimChannel]] != offCode),
              aes(x = Time.sec, y = .data[[stimChannel]] + 40, label = .data[[stimChannel]]),
              colour = 'black') +
    geom_point(data = stimAligned,
               aes(x = Start.sec, y = StimCode),
               colour = 'blue', shape = 3) +
    geom_text(data = stimAligned,
              aes(x = Start.sec, y = StimCode + 20, label = StimCode),
              colour = 'blue') +
    geom_point(data = stimAligned,
               aes(x = End.sec, y = offCode),
               colour = 'blue', shape = 4) +
    labs(title = 'Black: stim codes in femg file; Blue: stim codes in stim file', y = stimChannel)
  
  output$comparisonPlot <- comparisonPlot

  if (fillStimCodes) {
    stimFilledData <- femgData %>% mutate(StimCode.filled = .[[stimChannel]])
    for (n in seq_along(stimAligned$StimCode)) {
      tofill <- which(
        femgData$Time.sec > femg.Start[n] & 
        femgData$Time.sec <= femg.Start[n] + stimAligned$Duration.sec[n]
        )
      stimFilledData[tofill,'StimCode.filled'] <- stimAligned$StimCode[n]
    }

      stimFilledData <- stimFilledData %>% 
      add_transitions('StimCode.filled')
    
    filledPlot <- ggplot() +
      geom_path(data = filter(stimFilledData, transition.start | transition.end),
                aes(x = Time.sec, y = StimCode.filled), colour = 'darkgreen') +
      geom_text(data = filter(stimFilledData, transition.start & StimCode.filled != offCode),
                aes(x = Time.sec, y = StimCode.filled + 40, label = StimCode.filled),
                colour = 'darkgreen') +
      geom_point(data = stimAligned,
                 aes(x = Start.sec, y = StimCode),
                 colour = 'blue', shape = 3) +
      geom_text(data = stimAligned,
                aes(x = Start.sec, y = StimCode + 20, label = StimCode),
                colour = 'blue') +
      geom_point(data = stimAligned,
                 aes(x = End.sec, y = offCode),
                 colour = 'blue', shape = 4) +
      labs(title = 'Green: stim codes in femg file (duration filled from stim file); Blue: stim codes in stim file', y = stimChannel) 
    
    output$stimFilledData <- stimFilledData
    output$filledPlot <- filledPlot
    
    } else {
      startmed <- median(startOffsets)
      endmed <- median(endOffsets)
      if (endmed > 3*startmed) {
        warning(paste('compare_stim_face_emoji_expt()\n',
                      'offsets for the end of the stimulus are between', 
                      prettyNum(min(endOffsets)), 'and', prettyNum(max(endOffsets)), 
                      'seconds (median =', prettyNum(endmed), 
                      'seconds). \nDo you want \'fillStimCodes = TRUE\'?'))
      }
    }
  
  return(output)
  
}

saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  saveWidget(widget,file=file,...)
}

add_session_variables <- function(femgData, stimChannel, offCode = 0) {
  
  # is the code different to the previous one? 
  femgData <- femgData %>% 
    add_transitions(stimChannel)
  # get trial numbers from indices of transition points
  newTrials <- c(0, which(femgData$transition.end &
                            femgData[[stimChannel]] != offCode))
  
  # fill in all trial numbers 
  femgData <- femgData %>% mutate(trialNo = 0)
  for (n in seq_along(newTrials)[-length(newTrials)]) {
    femgData <- femgData %>% 
      mutate(trialNo = replace(trialNo, 
                               (newTrials[n]+1):newTrials[n+1], 
                               values = n))
  }
  
  # add phase info (pre-stim/stim)
  femgData <- femgData %>% 
    mutate(phase = ifelse(femgData[[stimChannel]] != offCode, 
                          'stimulus', 'prestim'),
           phase = replace(phase, trialNo == 0, NA))
  
  # time relative to stimulus onset
  femgData <- femgData %>%
    group_by(trialNo) %>%
    mutate(stimTime.sec = Time.sec - min(Time.sec[phase!='prestim']))
  
  return(femgData %>% ungroup)
}