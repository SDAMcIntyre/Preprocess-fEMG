library(tidyverse)
library(RcppRoll)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

scale_and_flag <- function(data, prefixes, win.sec, flag.threshold) {
  sample.duration <- diff(data$stimTime.sec[1:2])
  nSamples <- win.sec/sample.duration
  for (varPF in prefixes) {
    rawVar <- names(data) %>% str_subset(varPF)
    name.z <- paste0(varPF,'.z')
    name.z.range <- paste0(name.z,'.range')
    name.flagged <- paste0(varPF, '.flagged')
    data <- data %>% 
      mutate(!!name.z := scale(.data[[rawVar]]),
             !!name.z.range := roll_range(.data[[name.z]], n = nSamples, fill = NA),
             !!name.flagged := abs(.data[[name.z.range]]) > flag.threshold)
  }
  return(data)
}

find_alternate_baseline <- function(data, flagVar, baseline.sec) {
  # find an alternate baseline from a single trial
  sample.duration <- diff(data$Time.sec[1:2])
  nSamples <- baseline.sec/sample.duration
  data %>% 
    mutate(possible.new.bl = roll_maxr(.data[[flagVar]], nSamples, fill = NA) == 0) %>% 
    filter(possible.new.bl) %>% 
    pull(stimTime.sec) %>% 
    max() -> new.bl.end
  if (length(new.bl.end) == 0) return(new.bl.end) else
    return(c(new.bl.end - baseline.sec, new.bl.end))
}

summarise_flagged_trials <- function(data, prefixes, baseline.sec) {
  output <- list()
  for (varPF in prefixes) {
    
    name.flagged <- paste0(varPF, '.flagged')
    
    allFlaggedTrials <- data %>% 
      filter(stimTime.sec >= -baseline.sec & .data[[name.flagged]]) %>% 
      distinct(trialNo) %>% 
      pull(trialNo) 

    baselineOnlyFlaggedTrials <- data %>% 
      filter(stimTime.sec >= 0 & .data[[name.flagged]]) %>% # get stim flagged trials
      distinct(trialNo) %>% 
      pull(trialNo) %>% 
      setdiff(x = allFlaggedTrials) # compare to all flagged trials
    
    nTrials <- n_distinct(data$trialNo)
    nFlaggedTrials <- length(allFlaggedTrials)
    
    alt.baselines = tibble(trialNo = baselineOnlyFlaggedTrials,
                             alt.bl.start = NA,
                             alt.bl.end = NA)
    
    output[[varPF]] <- list('allFlaggedTrials' = allFlaggedTrials,
                            'baselineOnlyFlaggedTrials' = baselineOnlyFlaggedTrials,
                            'nTrials' = nTrials,
                            'nFlaggedTrials' = nFlaggedTrials,
                            'pcFlaggedTrials' = 100*nFlaggedTrials/nTrials)

    # if there are any trials with flags only in the baseline and there is more pre-stim data
    if ( length(baselineOnlyFlaggedTrials) > 0 & min(data$stimTime.sec < -2*baseline.sec) ) {
      # try to find a better baseline for each of those
      for (baselineTrial in baselineOnlyFlaggedTrials) {
        new.bl <-data %>%
          filter(trialNo == baselineTrial & stimTime.sec < 0) %>%
          find_alternate_baseline( flagVar = paste(varPF,'flagged', sep = '.'), baseline.sec )
        # save if it found some
        if (length(new.bl) > 0) {
          alt.baselines <- alt.baselines %>%
            mutate(alt.bl.start = replace(alt.bl.start, trialNo == baselineTrial, values = new.bl[1]),
                   alt.bl.end = replace(alt.bl.end, trialNo == baselineTrial, values = new.bl[2])) }
      } 
    }
    
    baselineRescuedTrials <- na.omit(alt.baselines$trialNo) %>% unlist()
    nRescuedTrials <- length(baselineRescuedTrials)
    excludeTrials <- setdiff(allFlaggedTrials, baselineRescuedTrials)
    nExcludedTrials <- length(excludeTrials)
    pcExcludedTrials <- 100*nExcludedTrials/nTrials
    output[[varPF]]<- output[[varPF]] %>% 
      c(list('alt.baselines' = alt.baselines,
             'baselineRescuedTrials' = baselineRescuedTrials,
             'nRescuedTrials' = nRescuedTrials,
             'excludeTrials' = excludeTrials,
             'nExcludedTrials' = nExcludedTrials,
             'pcExcludedTrials' = pcExcludedTrials))
  }
  return(output)
}

plot_flagged_femg_trials <- function(data, y, flag, flag.win, baseline) {
  x.start <- min(data$stimTime.sec)
  x.stop <- max(data$stimTime.sec)
  data %>% 
    ggplot(aes(x = stimTime.sec,y = .data[[y]])) +
    geom_vline(xintercept = 0) +
    geom_tile(data = filter(data, .data[[flag]]), fill = '#fed976', alpha = 0.1,
              aes(x = stimTime.sec, y = .data[[y]], width = flag.win, height = Inf ) ) +
    facet_grid(trialNo ~ ., scales = 'free_y') +
    geom_vline(xintercept = baseline, colour = '#41ab5d', size = 1) +
    geom_line(colour = '#2b8cbe', size = 1) +
    scale_x_continuous(breaks = function(x) seq(x.start, x.stop, by = win.sec*4),
                       minor_breaks = function(x) seq(x.start, x.stop, by = win.sec))
}

label_baseline_periods <- function(data, prefixes, trials, default.bl) {
  for (varPF in prefixes) {
    name.bl.start <- paste(varPF,'bl.start', sep = '.')
    name.bl.end <- paste(varPF,'bl.end', sep = '.')
    data <- data %>% mutate(!!name.bl.start:= default.bl[1],
                    !!name.bl.end:= default.bl[2])
    for (b in trials[[varPF]]$baselineRescuedTrials) {
      alt.bl <- trials[[varPF]]$alt.baselines %>% filter(trialNo == b) %>% unlist()
      data <- data %>% 
        mutate(!!name.bl.start:= replace(.data[[name.bl.start]], b == trialNo, alt.bl[2]),
               !!name.bl.end:= replace(.data[[name.bl.end]], b == trialNo, alt.bl[3]))
    }
  }
  data
}
