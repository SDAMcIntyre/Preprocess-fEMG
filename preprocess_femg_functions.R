library(tidyverse)
library(RcppRoll)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

scale_and_flag <- function(data, prefixes, win.sec, flag.threshold) {
  sample.duration <- diff(data$Time.sec[1:2])
  nSamples <- win.sec/sample.duration
  for (varPF in prefixes) {
    rawVar <- names(data) %>% str_subset(varPF)
    name.z <- paste(varPF,'z',sep = '.')
    name.z.range <- paste(name.z,'range', sep = '.')
    name.flagged <- paste(varPF, 'flagged', sep = '.')
    data <- data %>% 
      mutate(!!name.z := scale(.data[[rawVar]]),
             !!name.z.range := roll_range(.data[[name.z]], nSamples, fill = NA),
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
    
    name.flagged <- paste(varPF, 'flagged', sep = '.')
    
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
    
    output[[varPF]] <- list('allFlaggedTrials' = allFlaggedTrials,
                            'baselineOnlyFlaggedTrials' = baselineOnlyFlaggedTrials,
                            'nTrials' = nTrials,
                            'nFlaggedTrials' = nFlaggedTrials,
                            'pcFlaggedTrials' = 100*nFlaggedTrials/nTrials,
                            'alt.baselines' = tibble(trialNo = baselineOnlyFlaggedTrials,
                                                  new.bl.start = NA,
                                                  new.bl.end = NA))

    # if there are any trials with flags only in the baseline
    if ( length(baselineOnlyFlaggedTrials) > 0 ) {
     # try to find a better baseline for each of those
      for (baselineTrial in baselineOnlyFlaggedTrials) {
        new.bl <- pp.femg.data %>% 
          filter(trialNo == baselineTrial & stimTime.sec < 0) %>% 
          find_alternate_baseline( flagVar = paste(varPF,'flagged', sep = '.'), baseline.sec )
        # save if it found some
        if (length(new.bl) > 0) {
          output[[varPF]]$alt.baselines %>% 
            mutate(new.bl.start = replace(new.bl.start, trialNo == baselineTrial, values = new.bl[1]),
                   new.bl.end = replace(new.bl.end, trialNo == baselineTrial, values = new.bl[2])) }
      } }
  
  }
  return(output)
}

plot_flagged_femg_trials <- function(data, y, flag, flag.win) {
  data %>% 
    ggplot(aes(x = stimTime.sec,y = {{y}})) +
    geom_vline(xintercept = 0) +
    geom_tile(data = filter(data, {{flag}}), fill = '#fed976', alpha = 0.1,
              aes(x = stimTime.sec, y = {{y}}, width = flag.win, height = Inf ) ) +
    facet_grid(trialNo ~ ., scales = 'free_y') +
    geom_line(colour = '#2b8cbe', size = 1) 
}

