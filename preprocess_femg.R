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

# ---- MAIN LOOP ---- 

for (n in seq_along(codedDataFiles)) {
  coded.data.file <- paste0(codedDataFolder,codedDataFiles[n])
  coded.femg.data <- read_csv(coded.data.file)
  
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
  ID <- str_extract(codedDataFiles[n], IDformat)
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
                name.z.mean.difference = diff(name.z.mean))
    
    summary.femg.data[[varPF]] %>% 
      write_tsv(paste0(summaryFolder,ID,'_',varPF,'_mean.txt'))
    
  }
  
  
  Zyg.summary <- Zyg %>% 
    group_by(trialNo, StimCode, phase) %>% 
    summarise(Zyg.z.mean = mean(Zyg.z)) %>% 
    group_by(trialNo) %>% 
    summarise(StimCode = StimCode[2],
              Zyg.z.mean.baseline = Zyg.z.mean[1],
              Zyg.z.mean.stimulus = Zyg.z.mean[2],
              Zyg.z.mean.difference = diff(Zyg.z.mean))
  
  write_tsv(Zyg.summary, paste0(summaryFolder,ID,'_Zyg_mean.txt'))
  
  Cor.cols <- str_which(names(pp.femg.data),'Cor')
  Cor <- pp.femg.data %>% 
    select(c(1:5, Cor.cols)) %>% 
    group_by(trialNo) %>% 
    mutate(phase = replace(phase, 
                           stimTime.sec >= Cor.bl.start & stimTime.sec <= Cor.bl.end, # boundaries wrong
                           'baseline'),
           Cor.z = replace(Cor.z,
                           trialNo %in% trials$Cor$excludeTrials,
                           NA)) %>% 
    filter( phase != 'prestim') 
  write_csv(Cor, paste0(outputFolder,ID,'_Cor_preprocessed.csv'), append = TRUE)
  
  
  Cor.summary <- Cor %>% 
    group_by(trialNo, StimCode, phase) %>% 
    summarise(Cor.z.mean = mean(Cor.z)) %>% 
    group_by(trialNo) %>% 
    summarise(StimCode = StimCode[2],
              Cor.z.mean.baseline = Cor.z.mean[1],
              Cor.z.mean.stimulus = Cor.z.mean[2],
              Cor.z.mean.difference = diff(Cor.z.mean))
  
  write_tsv(Cor.summary, paste0(summaryFolder,ID,'_Cor_mean.txt'))
}
