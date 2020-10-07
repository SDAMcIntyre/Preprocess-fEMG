library(tidyverse)
library(plotly)
source('preprocess_femg_functions.R')

# ---- for reading in coded data ----
codedDataFolder <- './coded data emojis 2/'
codedDataFiles <- dir(codedDataFolder)

IDformat <- '[0-9]{3}'

# ---- for saving pre-processed data ----
outputFolder <- './preprocessed data emojis 2/'

# time in seconds
prestim.sec <- 1.0 
baseline.sec <- 0.2 
stimulus.sec <- 1.0
win.sec <- 0.05

flag.threshold <- 3 # multiple of SD
prefixes <- c('Zyg', 'Cor', 'Lev')
rawVariables <- c('Zyg.mV', 'Cor.mV', 'Lev.mV')

dataset.summary <- list()
for (varPF in prefixes) {
  dataset.summary[[varPF]] <- tibble(sourceFile = codedDataFiles,
                                     nExcludedTrials = rep_along(codedDataFiles,0),
                                     pcExcludedTrials = rep_along(codedDataFiles,0))
}

# ---- MAIN LOOP ---- 

for (n in seq_along(codedDataFiles)) {
  coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
  coded.femg.data <- read_csv(coded.data.file, col_types = cols())
  ID <- str_extract(codedDataFiles[n], IDformat)
  
  # z-score data and flag extreme transitions
  flagged.femg.data.all <- coded.femg.data %>% 
    scale_and_flag(prefixes, win.sec, flag.threshold) 
  
  histData <- flagged.femg.data.all %>%
    pivot_longer( cols = matches( to_regex(prefixes) ),
                  names_to = c('muscle','.value'), 
                  names_pattern = '(.{3})\\.(.*)') %>% 
    mutate(muscle = factor(muscle, levels = prefixes),
           zstep = cut(abs(z), breaks = c(0:3,Inf), labels = c('>0','>1','>2','>3'), right = FALSE))
  
  h <- histData %>% 
    ggplot(aes(x = mV)) +
    facet_wrap(muscle~., scales = 'free', ncol = 1) +
    geom_histogram(aes(fill = zstep), binwidth = 0.05, colour = 'grey') +
    scale_x_continuous(trans = 'log1p') +
    scale_fill_brewer(direction = -1) +
    theme_bw() +
    labs(title = ID, fill = 'z-score')
  ggsave(paste0('./histograms/',ID,'.png'), h, width = 7, height = 6)
  
  flagged.femg.data <- flagged.femg.data.all %>% 
    filter(trialNo > 0 & 
             stimTime.sec >= -prestim.sec &
             stimTime.sec < stimulus.sec) 
  
  # summary of trials with flags and find alternate baselines
  trials <- summarise_flagged_trials(flagged.femg.data, prefixes, baseline.sec)
  #glimpse(trials)
  all.flagged.trials <- c()
  for (varPF in prefixes) {
    all.flagged.trials <- all.flagged.trials %>% 
      c(trials[[varPF]]$allFlaggedTrials)
  }
  all.flagged.trials <- sort_unique(all.flagged.trials)
  
  
  # apply baseline periods
  secondsPerSample <- diff(flagged.femg.data$Time.sec[1:2])
  default.bl <- c(-baseline.sec, 0 - secondsPerSample) # 1 sample before 0
  flagged.femg.data <- flagged.femg.data %>% 
    label_baseline_periods(prefixes, trials, default.bl)
  
  
  # save the flagged trials for inspection
  for (flaggedTrial in all.flagged.trials) {
    
    trialData <- flagged.femg.data %>% 
      filter(trialNo == flaggedTrial) %>% 
      pivot_longer( cols = matches( to_regex(prefixes) ),
                    names_to = c('muscle','.value'), 
                    names_pattern = '(.{3})\\.(.*)') %>% 
      select(-c(Time.sec,StimCode,trialNo,phase,z,zrange)) 
    
    facetData <- trialData %>% 
      group_by(muscle) %>% 
      summarise(bl.start = bl.start[1], bl.end = bl.end[1],
                top.y = max(mV),
                bottom.y = min(mV),
                middle.y = max(mV) - diff(range(mV))/2,
                height.y = diff(range(mV))
                )
    
    x.start <- min(trialData$stimTime.sec)
    x.stop <- max(trialData$stimTime.sec)
    
    get_block_runs <- function(x,secondsPerSample,win.sec,prestim.sec) {
      runs <- rle(x)
      blockruns <- tibble(flagged = runs$values,
                          end = cumsum((runs$lengths)*secondsPerSample) - prestim.sec,
                          start = c(- prestim.sec,end + secondsPerSample)[-(length(end)+1)]
      )
      blockruns %>% 
        mutate(end = end + win.sec/2, start = start - win.sec/2) %>% 
        filter(flagged)
    }
    
    flagData <- trialData %>% 
      select(muscle, flagged) %>% 
      nest(flagdata = c(flagged)) %>% 
      group_by(muscle) %>% 
      mutate(flagruns = map(flagdata, ~get_block_runs(.x$flagged,secondsPerSample,win.sec,prestim.sec) )) %>% 
      unnest(flagruns, keep_empty = TRUE) %>% 
      full_join(facetData, by = 'muscle') %>%
      ungroup() %>% 
      mutate(muscle = factor(muscle, levels = prefixes))
    
    note = paste0(ID,' trial ',flaggedTrial,'.')
    for (varPF in prefixes) {
      note <- paste(note,varPF)
      add <- case_when( flaggedTrial %in% trials[[varPF]]$excludeTrials ~ 'excluded.',
                        flaggedTrial %in% trials[[varPF]]$baselineRescuedTrials ~ 'alternative baseline.',
                        TRUE ~ 'included.')
      note <- paste(note, add)
    }
    
    
    trialplot <- trialData %>%
      mutate(muscle = factor(muscle, levels = prefixes)) %>% 
      ggplot() +
      facet_grid(muscle~., scales = 'free_y') +
      geom_vline(xintercept = 0) +
      geom_rect(data = flagData,
        aes(fill = flagged, xmin = start, xmax = end, 
            ymin = bottom.y, ymax = top.y )) +
      scale_fill_manual(values = '#fcae91') +
      geom_tile(data = flagData,
                aes(y = middle.y, height = height.y*1.1,
                    x = bl.start + baseline.sec/2,
                    colour = muscle, fill = NA),
                width = baseline.sec) +
      geom_point(aes(x = stimTime.sec,y = mV, colour = muscle), size = 1) + 
      scale_color_brewer(palette = 'Set1') +
      theme_bw() + theme(legend.position='none') +
      scale_x_continuous(
       breaks = function(x) seq(x.start, x.stop, by = win.sec*4),
       minor_breaks = function(x) seq(x.start, x.stop, by = win.sec)) +
      labs(title = paste(note,
                         '\nSquare outline = baseline period. Shaded areas = flagged as artefact.'),
           x = 'Time relative to stimulus onset (seconds)', y = 'mV')
    
    trialplotly <- ggplotly(trialplot) 
    
    #plot001 <- combineWidgets(ncol = 1, trialplotly, trialplotly2)

    inspectplot <- paste0('./FOR INSPECTION/',ID,'_',flaggedTrial,'.html')
    saveWidgetFix(trialplotly, inspectplot)
    warning(paste(
      'Trial flagged for exclusion.\n',
      'See', inspectplot, 'for more details.'
    ))
    
  }
  
  
  for (varPF in prefixes) {
    if (trials[[varPF]]$nFlaggedTrials > 0) {

      for (thisTrial in trials[[varPF]]$allFlaggedTrials) {
        thisBaseline <- trials[[varPF]]$alt.baselines %>%
          filter(trialNo == thisTrial) %>% select(2:3) %>% unlist() %>% na.omit()
        if (length(thisBaseline) > 0) { note <- 'flagged_bl'} else {
          note <- 'flagged'
        }

        inspectfile <- paste0('./FOR INSPECTION/',ID,'_flagged_trials.csv')
        tibble(code = thisTrial, variable = varPF, note = note, from = codedDataFiles[n]) %>%
          write_csv(inspectfile, append = TRUE)
        
        warning(paste(
          'Trial flagged for exclusion.\n',
          'See', inspectfile, 'for more details.'
        ))
      }
    }
  }

  # save the data
  summaryFolder <- './results/'
  pp.femg.data <- list()
  
   for (varPF in prefixes) {
     var.cols <- str_which(names(flagged.femg.data),varPF)
     name.z <- paste0(varPF,'.z')
     name.bl.start <- paste0(varPF,'.bl.start')
     name.bl.end <- paste0(varPF,'.bl.end')
     pp.femg.data[[varPF]] <- flagged.femg.data %>% 
       select(c(1:5, var.cols)) %>% 
       group_by(trialNo) %>% 
       mutate(phase = replace(phase, 
                              stimTime.sec >= .data[[name.bl.start]] &
                                stimTime.sec <= .data[[name.bl.end]], # boundaries off by one sample?
                              'baseline'),
              !!name.z := replace(.data[[name.z]],
                              trialNo %in% trials[[varPF]]$excludeTrials,
                              NA)) %>% 
       filter( phase != 'prestim')
     
     pp.femg.data[[varPF]] %>% 
       select(-c(name.bl.start,name.bl.end)) %>% 
       write_csv(paste0(outputFolder,ID,'_',varPF,'_preprocessed.csv'))
  }
  
  summary.femg.data <- list()
  for (varPF in prefixes) {
    name.z <- paste0(varPF,'.z')
    name.z.mean <- paste0(varPF,'.z.mean')
    name.z.mean.baseline <- paste0(varPF,'.z.mean.baseline')
    name.z.mean.stimulus <- paste0(varPF,'.z.mean.stimulus')
    name.z.mean.difference <- paste0(varPF,'.z.mean.difference')
    
    summary.femg.data[[varPF]] <- pp.femg.data[[varPF]] %>% 
      group_by(trialNo, StimCode, phase) %>% 
      summarise(!!name.z.mean := mean(.data[[name.z]], na.rm = TRUE)) %>% 
      group_by(trialNo) %>% 
      summarise(StimCode = StimCode[2],
                !!name.z.mean.baseline := .data[[name.z.mean]][1],
                !!name.z.mean.stimulus := .data[[name.z.mean]][2]) %>% 
                # name.z.mean.difference = diff(name.z.mean))
      mutate(!!name.z.mean.difference := .data[[name.z.mean.stimulus]] - .data[[name.z.mean.baseline]])

    summary.femg.data[[varPF]] %>% 
      write_tsv(paste0(summaryFolder,ID,'_',varPF,'_mean.txt'))
    
    dataset.summary[[varPF]]$nExcludedTrials[n] = trials[[varPF]]$nExcludedTrials
    dataset.summary[[varPF]]$pcExcludedTrials[n] = trials[[varPF]]$pcExcludedTrials
    
  }
  
}

for (varPF in prefixes) {
  dataset.summary[[varPF]] %>% 
    write_csv(paste0(varPF,'_excluded_summary.csv'))
}
View(dataset.summary$Zyg)
View(dataset.summary$Cor)
print(mean(dataset.summary$Zyg$pcExcludedTrials))
print(mean(dataset.summary$Cor$pcExcludedTrials))
