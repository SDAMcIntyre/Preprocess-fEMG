library(tidyverse)
source('preprocess_femg_functions.R')


# ---- for reading in coded data ----
codedDataFolder <- './coded data/'
codedDataFiles <- dir(codedDataFolder)

IDformat <- '(s|p)[0-9]{3}'

# ---- for saving pre-processed data ----
outputFolder <- './preprocessed data/'

# time in seconds
prestim.sec <- 1.0 
baseline.sec <- 0.2 
stimulus.sec <- 1.0
win.sec <- 0.05

flag.threshold <- 3 # multiple of SD
prefixes <- c('Zyg', 'Cor')
rawVariables <- c('Zyg.mV', 'Cor.mV')

dataset.summary <- list()
for (varPF in prefixes) {
  dataset.summary[[varPF]] <- tibble(sourceFile = codedDataFiles,
                                     nExcludedTrials = rep_along(codedDataFiles,0),
                                     pcExcludedTrials = rep_along(codedDataFiles,0))
}

# ---- MAIN LOOP ---- 

for (n in seq_along(codedDataFiles)) {
  coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
  coded.femg.data <- read_csv(coded.data.file)
  ID <- str_extract(codedDataFiles[n], IDformat)
  
  # z-score data and flag extreme transitions
  flagged.femg.data <- coded.femg.data %>% 
    scale_and_flag(prefixes, win.sec, flag.threshold) %>% 
    filter(trialNo > 0 & 
             stimTime.sec >= -prestim.sec &
             stimTime.sec < stimulus.sec) 
  
  # summary of trials with flags and find alternate baselines
  trials <- summarise_flagged_trials(flagged.femg.data, prefixes, baseline.sec)
  glimpse(trials)
  
  # save the flagged trials for inspection
  for (varPF in prefixes) {
    if (trials[[varPF]]$nFlaggedTrials > 0) {

      for (thisTrial in trials[[varPF]]$allFlaggedTrials) {
        thisBaseline <- trials[[varPF]]$alt.baselines %>%
          filter(trialNo == thisTrial) %>% select(2:3) %>% unlist() %>% na.omit()
        if (length(thisBaseline) > 0) { note <- 'flagged_bl'} else {
          note <- 'flagged'
        }

        windows(15,2)
        flagged.femg.data %>%
          filter(stimTime.sec >= -prestim.sec &
                   trialNo %in% thisTrial) %>%
          plot_flagged_femg_trials(y = str_subset(rawVariables,varPF),
                                   flag = paste(varPF, 'flagged', sep = '.'), win.sec,
                                   baseline = thisBaseline) +
          labs(title = paste(ID, varPF), x = 'Time (seconds)', y = str_subset(rawVariables,varPF))
        ggsave(paste0('./FOR INSPECTION/',ID,'_',thisTrial,'_',varPF,'_',note,'.png'))
        dev.off()
        tibble(code = thisTrial, variable = varPF, note = note, from = codedDataFiles[n]) %>%
          write_csv(paste0('./FOR INSPECTION/flagged_trials.csv'), append = TRUE)
      }
    }
  }

  # select baseline period and save
  default.bl <- c(-baseline.sec, 0 - diff(flagged.femg.data$Time.sec[1:2])) # 1 sample before 0
  flagged.femg.data <- flagged.femg.data %>% 
    label_baseline_periods(prefixes, trials, default.bl)
  
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
      summarise(name.z.mean = mean(name.z)) %>% 
      group_by(trialNo) %>% 
      summarise(StimCode = StimCode[2],
                name.z.mean.baseline = name.z.mean[1],
                name.z.mean.stimulus = name.z.mean[2],
                # name.z.mean.difference = diff(name.z.mean))
                name.z.mean.difference = name.z.mean.stimulus - name.z.mean.baseline)

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
