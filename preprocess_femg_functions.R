library(tidyverse)
library(RcppRoll)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

scale_and_flag <- function(data, prefixes, win.samples, flag.threshold) {
  for (v in seq_along(prefixes)) {
    rawVar <- names(data) %>% str_subset(prefixes[v])
    name.z <- paste(prefixes[v],'z',sep = '.')
    name.z.range <- paste(name.z,'range', sep = '.')
    name.flagged <- paste(prefixes[v], 'flagged', sep = '.')
    data <- data %>% 
      mutate(!!name.z := scale(.data[[rawVar]]),
             !!name.z.range := roll_range(.data[[name.z]], win.samples, fill = NA),
             !!name.flagged := abs(.data[[name.z.range]]) > flag.threshold)
  }
  return(data)
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

