library(tidyverse)
library(RcppRoll)
library(htmlwidgets)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

scale_and_flag <- function(df, prefixes, win.sec, flag.threshold) {
  sample.duration <- diff(df$stimTime.sec[1:2])
  nSamples <- win.sec/sample.duration
  for (varPF in prefixes) {
    rawVar <- names(df) %>% str_subset(varPF)
    name.z <- paste0(varPF,'.z')
    name.zrange <- paste0(name.z,'range')
    name.flagged <- paste0(varPF, '.flagged')
    df <- df %>% 
      mutate(!!name.z := scale(.[[rawVar]])[,1]) %>% 
      mutate(!!name.zrange := roll_range(.[[name.z]], n = nSamples, fill = NA)) %>% 
      mutate(!!name.flagged := abs(.[[name.zrange]]) > flag.threshold)
  }
  return(df)
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

find_alternate_baseline <- function(df, flagVar, baseline.sec) {
  # find an alternate baseline from a single trial
  sample.duration <- diff(df$Time.sec[1:2])
  nSamples <- baseline.sec/sample.duration
  df %>% 
    mutate(possible.new.bl = roll_maxr(.[[flagVar]], nSamples, fill = NA) == 0) %>% 
    filter(possible.new.bl) %>% 
    pull(stimTime.sec) %>% 
    max() -> new.bl.end
  if (length(new.bl.end) == 0) return(new.bl.end) else
    return(c(new.bl.end - baseline.sec, new.bl.end))
}

summarise_flagged_trials <- function(df, prefixes, baseline.sec) {
  output <- list()
  for (varPF in prefixes) {
    
    name.flagged <- paste0(varPF, '.flagged')
    
    allFlaggedTrials <- df %>% 
      filter(stimTime.sec >= -baseline.sec & .[[name.flagged]]) %>% 
      distinct(trialNo) %>% 
      pull(trialNo) 

    baselineOnlyFlaggedTrials <- df %>% 
      filter(stimTime.sec >= 0 & .[[name.flagged]]) %>% # get stim flagged trials
      distinct(trialNo) %>% 
      pull(trialNo) %>% 
      setdiff(x = allFlaggedTrials) # compare to all flagged trials
    
    nTrials <- n_distinct(df$trialNo)
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
    if ( length(baselineOnlyFlaggedTrials) > 0 & (min(df$stimTime.sec) < -2*baseline.sec) ) {
      # try to find a better baseline for each of those
      for (baselineTrial in baselineOnlyFlaggedTrials) {
        new.bl <-df %>%
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

sort_unique <- function(x) {
  y <- unique(x)
  y[order(y)] }

to_regex <- function(x) {
  return(paste0('(', paste(x, collapse = ')|('), ')'))
}


label_baseline_periods <- function(df, prefixes, trials, default.bl) {
  for (varPF in prefixes) {
    name.bl.start <- paste(varPF,'bl.start', sep = '.')
    name.bl.end <- paste(varPF,'bl.end', sep = '.')
    df <- df %>% mutate(!!name.bl.start:= default.bl[1],
                    !!name.bl.end:= default.bl[2])
    for (b in trials[[varPF]]$baselineRescuedTrials) {
      alt.bl <- trials[[varPF]]$alt.baselines %>% filter(trialNo == b) %>% unlist()
      df <- df %>% 
        mutate(!!name.bl.start:= replace(.[[name.bl.start]], b == trialNo, alt.bl[2]),
               !!name.bl.end:= replace(.[[name.bl.end]], b == trialNo, alt.bl[3]))
    }
  }
  df
}
