source('label_femg_functions.R')
library(plotly)

# ---- for reading in raw data ----
rawDataFolder <- './raw data emojis 2/'
rawDataFiles <- dir(rawDataFolder)
femg.ChannelNames <- c('Corr Processed',
                       'Zyg Processed',
                       'Lev Processed')
stim.ChannelName <- 'Marker'

stim.OffCode <- 0
# read all stimulus codes used in the experiment from a file, add 0 to the list (for no stim)
femg.stimCodes <- read_excel('trigger stimulus coding.xlsx')$code %>% c(stim.OffCode)
# known noise codes to set to stim off code
femg.noiseCodes <- c(1,4)

stimSequenceFolder <- './stim sequences emojis 2/'
stimSequenceFiles <- dir(stimSequenceFolder)
# regex format of the participant/session/file ID from the femg filename to match to the Presentation filename
IDformat <- '[0-9]{3}'

# ---- for saving cleaned data ----
stimColName <- 'StimCode'
femgColNames <- c('Cor.mV','Zyg.mV','Lev.mV')
outputFolder <- './coded data emojis 2/'

# ---- MAIN LOOP ----
for (n in 1:length(rawDataFiles)) {
  # current file
  raw.femg.file <- paste0(rawDataFolder,rawDataFiles[n])
  # print progress
  print( paste( n, 'of', length(rawDataFiles), ':', rawDataFiles[n]))
  
  # does this file need the stimcodes to be filled (IDs with numbers higher than 3)?
  fillStimCodes <- str_extract(raw.femg.file, IDformat) %>% parse_number() > 3
  
  # read in the raw text file from acqKnowledge
  raw.femg.data <- read_acq_text(fileName = raw.femg.file, 
                                 keepChannels = c(stim.ChannelName, femg.ChannelNames))

  # clean up the codes in the data file, add transition labels, and find any remaining unexpected codes
  coded.femg.data <- clean_acq_stim_codes(femgData = raw.femg.data, 
                                          stimChannel = stim.ChannelName,
                                          usedStimCodes = femg.stimCodes,
                                          knownNoiseCodes = femg.noiseCodes,
                                          offCode = stim.OffCode,
                                          nsamples = 'auto') 

  # check for unexpected stim codes
  corrected.unexpectedCodes <- coded.femg.data %>% 
    filter(unexpected) %>% pull(StimCode.corrected) %>% unique()
  
  corrected.expectedCodes <- coded.femg.data %>% 
    filter(!unexpected) %>% pull(StimCode.corrected) %>% unique()
  
  if (length(corrected.unexpectedCodes) > 0 | length(corrected.expectedCodes) < 2) {
    # plot the stimulus sequence
    codely <- coded.femg.data %>% 
      plot_stim_code_sequence('StimCode.corrected')
    inspectplot <- paste0('./FOR INSPECTION/',ID,'_unexpected_codes.html')
    htmlwidgets::saveWidget(codely, inspectplot)
    inspectfile <- paste0('./FOR INSPECTION/unexpected_codes.csv')
    tibble(code = corrected.unexpectedCodes, from = raw.femg.file) %>% 
      write_csv(inspectfile, append = TRUE)
    warning(paste(
      'Unexpected stimulus codes found in', raw.femg.file, '.\n',
      'See', inspectplot, 'and', inspectfile, 'for more details.'
    ))
  }
  
  # check against Presentation stim sequence
  # get ID to match acqKnowledge and Presentation files
  ID <- str_extract(raw.femg.file, IDformat)
  # corresponding stimulus sequence file
  stim.File <- paste0(stimSequenceFolder,
                      stimSequenceFiles[str_detect(stimSequenceFiles, ID)])
  
  # this function is experiment specific
  comparison <- compare_stim_face_emoji_expt(femgData = coded.femg.data, 
                                 stimChannel = 'StimCode.corrected', 
                                 offCode = stim.OffCode,
                                 stimFile = stim.File,
                                 fillStimCodes = fillStimCodes)
  
  if (comparison$synced == FALSE) {
    # plot the two sequences
    comply <- ggplotly(comparison$comparisonPlot)
    inspect <- paste0('./FOR INSPECTION/',ID,'_unsynced.html')
    htmlwidgets::saveWidget(comply, inspect)
    warning(paste(
      'Stimulus codes in', raw.femg.file, 'and', stim.File, 'are not synced.\n',
      'See', inspect, 'for more details.'
    ))
  }
  
  print('Stimulus start offsets (seconds):'); print(summary(comparison$startOffsets))
  print('Stimulus end offsets (seconds):'); print(summary(comparison$endOffsets))
  
  if (fillStimCodes) {
    coded.data <- comparison$stimFilledData
    coded.stim.channel <- 'StimCode.filled'
  } else {
    coded.data <- coded.femg.data
    coded.stim.channel <- 'StimCode.corrected'
  }

  # add trial numbers and phase (prestim or stimulus) and stimulus relative time
  labelled.femg.data <- label_femg_data(coded.data, coded.stim.channel, stim.OffCode)

  # save the data
  if (length(corrected.unexpectedCodes) == 0 &
      length(corrected.expectedCodes) > 1 & 
      comparison$synced) {
  
      out.femg.data <- labelled.femg.data %>% 
      select(c('Time.sec',
               'stimTime.sec',
               coded.stim.channel,
               'trialNo',
               'phase',
               femg.ChannelNames))
      
      toreplace <- str_which(names(out.femg.data),
        paste(c('StimCode', femg.ChannelNames), collapse = '|') )
      
      print(paste(
        'Renaming', names(out.femg.data)[toreplace],
        'to', c(stimColName, femgColNames)
        ))
      
      names(out.femg.data)[toreplace] <- c(stimColName, femgColNames)
          
    out.femg.data %>% 
      write_csv(paste0(outputFolder,ID,'_labelled.csv'))
    print(paste0('Saving data: ', outputFolder,ID,'_labelled.csv'))
  }
}

