library(tidyverse)
library(RcppRoll)
library(htmlwidgets)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

scale_and_flag <- function(df, prefixes, win.sec, flag.threshold) {
  sample.duration <- diff(df$stimTime.sec[1:2])
  nSamples <- win.sec/sample.duration
  for (muscleName in prefixes) {
    rawVar <- names(df) %>% str_subset(muscleName)
    name.z <- paste0(muscleName,'.z')
    name.zrange <- paste0(name.z,'range')
    name.flagged <- paste0(muscleName, '.flagged')
    name.rawfixed <- paste0(muscleName, '.fixed')
    name.zfixed <- paste0(muscleName, '.zfixed')
    df <- df %>% 
      mutate(!!name.z := scale(.[[rawVar]])[,1]) %>% 
      mutate(!!name.zrange := roll_range(.[[name.z]], n = nSamples, fill = NA)) %>% 
      mutate(!!name.flagged := abs(.[[name.zrange]]) > flag.threshold)
      mutate(!!name.flagged := replace_na(.[[name.flagged]], FALSE)) %>% 
        
        mutate(!!name.rawfixed := if_else(.[[name.flagged]], 
                                          as.numeric(NA), 
                                          .[[rawVar]])) %>% 
        
        mutate(!!name.zfixed := if_else(.[[name.flagged]], 
                                        as.numeric(NA), 
                                        .[[name.z]]))
  }
  return(df)
}

clean_bins <- function(df, prefixes, pfkeep = 0.5) {
  for (muscleName in prefixes) {
    name.flagged <- paste0(muscleName, '.flagged')
    name.rawfixed <- paste0(muscleName, '.fixed')
    name.zfixed <- paste0(muscleName, '.zfixed')
    df <- df %>% 
      mutate(!!name.rawfixed := if_else(.[[name.flagged]] > pfkeep, 
                                        as.numeric(NA), 
                                        .[[name.rawfixed]])) %>% 
      mutate(!!name.zfixed := if_else(.[[name.flagged]] > pfkeep, 
                                      as.numeric(NA), 
                                      .[[name.zfixed]]))
  }
  return(df)
}

apply_bins <- function(df, prefixes, bin.sec = 0.1, pfkeep = 0) {
  df %>% 
    mutate(
      bin.n = cut(stimTime.sec, 
                  breaks = seq(min(stimTime.sec), max(stimTime.sec), by = bin.sec), 
                  labels = FALSE),
      binTime.sec = bin.n * bin.sec + min(stimTime.sec) 
    ) %>% 
    group_by(StimCode,trial,binTime.sec) %>% 
    summarise(
      across(.cols = starts_with(prefixes), 
             .fns = ~mean(., na.rm = TRUE))
    ) %>% 
    ungroup() %>% 
    do(clean_bins(., prefixes, pfkeep)) 
}

myboot <- function(x) { mean_cl_boot(x, B=10000) }

time_plot <- function(df, muscle) {
  
  df %>% 
    ggplot(aes(x = binTime.sec, y = {{muscle}}, 
               colour = cued, fill = cued, linetype = cued, shape = cued)) +
    geom_vline(xintercept = 0) +
    stat_summary(fun.data = 'myboot', geom = 'ribbon', alpha = 0.3) +
    stat_summary(fun = 'mean', geom = 'line') +
    stat_summary(fun = 'mean', geom = 'point') +
    scale_x_continuous(breaks = seq(0,max(df$binTime.sec),1)) +
    theme_bw() +
    labs(x = 'Time (seconds)', y = 'Muscle Activity (z)')
}


plot_histograms <-function(flaggedData, prefixes) {
  histData <- flaggedData %>%
    pivot_longer( cols = matches( to_regex(prefixes) ),
                  names_to = c('muscle','.value'), 
                  names_pattern = '(.{3})\\.(.*)') %>% 
    mutate(muscle = factor(muscle, levels = prefixes),
           zstep = cut(abs(z), breaks = c(0:3,Inf), labels = c('>0','>1','>2','>3'), right = FALSE))
  
  histData %>% 
    ggplot(aes(x = mV)) +
    facet_wrap(muscle~., scales = 'free', ncol = 1) +
    geom_histogram(aes(fill = zstep), binwidth = 0.05, colour = 'grey') +
    scale_x_continuous(trans = 'log1p') +
    scale_fill_brewer(direction = -1) +
    theme_bw() +
    labs(fill = 'z-score')
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

sort_unique <- function(x) {
  y <- unique(x)
  y[order(y)] }

to_regex <- function(x) {
  return(paste0('(', paste(x, collapse = ')|('), ')'))
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
  output[['flaggedOnAnyMuscle']] <- c()
  for (muscleName in prefixes) {
    
    name.flagged <- paste0(muscleName, '.flagged')
    
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
    
    output[[muscleName]] <- list('allFlaggedTrials' = allFlaggedTrials,
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
          find_alternate_baseline( flagVar = paste(muscleName,'flagged', sep = '.'), baseline.sec )
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
    output[[muscleName]]<- output[[muscleName]] %>% 
      c(list('alt.baselines' = alt.baselines,
             'baselineRescuedTrials' = baselineRescuedTrials,
             'nRescuedTrials' = nRescuedTrials,
             'excludeTrials' = excludeTrials,
             'nExcludedTrials' = nExcludedTrials,
             'pcExcludedTrials' = pcExcludedTrials))
  
    output[['flaggedOnAnyMuscle']] <- output[['flaggedOnAnyMuscle']] %>% 
      c(allFlaggedTrials)
  }
  output[['flaggedOnAnyMuscle']] <- output[['flaggedOnAnyMuscle']] %>% sort_unique()
  return(output)
}

update_baseline_periods <- function(df, prefixes, trials, baseline.sec) {
  secondsPerSample <- diff(df$Time.sec[1:2])
  default.bl <- c(-baseline.sec, 0 - secondsPerSample) # 1 sample before 0
  for (muscleName in prefixes) {
    name.bl.start <- paste(muscleName,'bl.start', sep = '.')
    name.bl.end <- paste(muscleName,'bl.end', sep = '.')
    name.z <- paste0(muscleName,'.z')
    df <- df %>% mutate(!!name.bl.start:= default.bl[1],
                    !!name.bl.end:= default.bl[2])
    
    for (b in trials[[muscleName]]$baselineRescuedTrials) {
      alt.bl <- trials[[muscleName]]$alt.baselines %>% filter(trialNo == b) %>% unlist()
      df <- df %>% 
        mutate(
          !!name.bl.start:= replace(.[[name.bl.start]], b == trialNo, alt.bl[2]),
          !!name.bl.end:= replace(.[[name.bl.end]], b == trialNo, alt.bl[3])
          )
        
    } 
  }
  df
}

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

plot_flagged_trial <- function(flaggedData, flaggedTrial, prefixes,win.sec,prestim.sec) {
  trialData <- flaggedData %>% 
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
  
  secondsPerSample <- diff(flaggedData$Time.sec[1:2])
  
  flagData <- trialData %>% 
    select(muscle, flagged) %>% 
    nest(flagdata = c(flagged)) %>% 
    group_by(muscle) %>% 
    mutate(flagruns = map(flagdata, 
                          ~get_block_runs(.x$flagged,secondsPerSample,win.sec,prestim.sec) 
                          )
           ) %>% 
    unnest(flagruns, keep_empty = TRUE) %>% 
    full_join(facetData, by = 'muscle') %>%
    ungroup() %>% 
    mutate(muscle = factor(muscle, levels = prefixes))
  
  note = paste0(' trial ',flaggedTrial,'.')
  for (muscleName in prefixes) {
    note <- paste(note,muscleName)
    add <- case_when( 
      flaggedTrial %in% trials[[muscleName]]$excludeTrials ~ 'excluded.',
      flaggedTrial %in% trials[[muscleName]]$baselineRescuedTrials ~ 'alternative baseline.',
      TRUE ~ 'included.'
      )
    note <- paste(note, add)
  }

  trialData %>%
    mutate(muscle = factor(muscle, levels = prefixes)) %>% 
    ggplot() +
    facet_grid(muscle~., scales = 'free_y') +
    # vertical line at stimulus onset
    geom_vline(xintercept = 0) +
    # shaded area to show flagged as artifact
    geom_rect(data = flagData,
              aes(fill = flagged, xmin = start, xmax = end, 
                  ymin = bottom.y, ymax = top.y )) +
    scale_fill_manual(values = '#fcae91') +
    # box outline to show baseline
    geom_tile(data = flagData,
              aes(y = middle.y, height = height.y*1.1,
                  x = bl.start + baseline.sec/2,
                  colour = muscle, fill = NA),
              width = baseline.sec) +
    # actual data
    geom_point(aes(x = stimTime.sec,y = mV, colour = muscle), size = 1) + 
    scale_color_brewer(palette = 'Set1') +
    # general appearance
    theme_bw() + theme(legend.position='none') +
    scale_x_continuous(
      breaks = function(x) seq(x.start, x.stop, by = win.sec*4),
      minor_breaks = function(x) seq(x.start, x.stop, by = win.sec)) +
    labs(title = paste(note,
                       '\nSquare outline = baseline period. Shaded areas = flagged as artifact.'),
         x = 'Time relative to stimulus onset (seconds)', y = 'mV')
}

finalise_data <- function(flaggedData,prefixes) {
  pp.femg.data <- list()
  
  for (muscleName in prefixes) {
    
    # names of variables depending on the muscle
    var.cols <- str_which(names(flaggedData),muscleName)
    name.mV <- paste0(muscleName,'.mV')
    name.z <- paste0(muscleName,'.z')
    name.bl.start <- paste0(muscleName,'.bl.start')
    name.bl.end <- paste0(muscleName,'.bl.end')
    
    pp.femg.data[[muscleName]] <- flaggedData %>% 
      # keep only ID variables and outcome variables
      select(c(1:5, var.cols)) %>% 
      group_by(trialNo) %>% 
      mutate(
        # fix baseline
        phase = replace(
          phase,
          stimTime.sec >= .data[[name.bl.start]] &
            stimTime.sec <= .data[[name.bl.end]],
          'baseline'
          ),
        # replace mV data from excluded trials with NA
        !!name.mV := replace(
          .data[[name.mV]],
          trialNo %in% trials[[muscleName]]$excludeTrials,
          NA
        ),
        # replace z data from excluded trials with NA
        !!name.z := replace(
          .data[[name.z]],
          trialNo %in% trials[[muscleName]]$excludeTrials,
          NA
        )
      ) %>% 
      # only keep data from baseline and stimulus phases
      filter( phase != 'prestim')
    
    pp.femg.data[[muscleName]] %>% 
      select(-c(name.bl.start,name.bl.end))
  }
  return(pp.femg.data)
}

means_and_diffs <- function(preprocessedData, prefixes) {
  summaryData <- list()
  for (muscleName in prefixes) {
    name.mV <- paste0(muscleName,'.mV')
    name.z <- paste0(muscleName,'.z')
    name.mV.mean <- paste0(muscleName,'.mV.mean')
    name.z.mean <- paste0(muscleName,'.z.mean')
    name.mV.mean.baseline <- paste0(muscleName,'.mV.mean.baseline')
    name.z.mean.baseline <- paste0(muscleName,'.z.mean.baseline')
    name.mV.mean.stimulus <- paste0(muscleName,'.mV.mean.stimulus')
    name.z.mean.stimulus <- paste0(muscleName,'.z.mean.stimulus')
    name.mV.mean.difference <- paste0(muscleName,'.mV.mean.difference')
    name.z.mean.difference <- paste0(muscleName,'.z.mean.difference')
    
    summaryData[[muscleName]] <- preprocessedData[[muscleName]] %>% 
      group_by(trialNo, StimCode, phase) %>% 
      summarise(
        # calculate means
        !!name.mV.mean := mean(.data[[name.mV]], na.rm = TRUE),
        !!name.z.mean := mean(.data[[name.z]], na.rm = TRUE)
      ) %>% 
      group_by(trialNo) %>% 
      summarise(
        # convert to wide format
        StimCode = StimCode[2],
        !!name.mV.mean.baseline := .data[[name.mV.mean]][1],
        !!name.z.mean.baseline := .data[[name.z.mean]][1],
        !!name.mV.mean.stimulus := .data[[name.mV.mean]][2],
        !!name.z.mean.stimulus := .data[[name.z.mean]][2]
        ) %>% 
  mutate(
    # calculate differences
    !!name.mV.mean.difference := .data[[name.mV.mean.stimulus]] - .data[[name.mV.mean.baseline]],
    !!name.z.mean.difference := .data[[name.z.mean.stimulus]] - .data[[name.z.mean.baseline]]
    )
  }
  summaryData
}