library(tidyverse)
library(RcppRoll)

roll_range <- function(x, ...) roll_max(x, ...) - roll_min(x, ...)

plot_flagged_femg_trials <- function(data, y, flag, flag.win) {
  data %>% 
    ggplot(aes(x = stimTime.sec,y = {{y}})) +
    geom_vline(xintercept = 0) +
    geom_tile(data = filter(data, {{flag}}), fill = '#fed976', alpha = 0.1,
              aes(x = stimTime.sec, y = {{y}}, width = flag.win, height = Inf ) ) +
    facet_grid(trialNo ~ ., scales = 'free_y') +
    geom_line(colour = '#2b8cbe', size = 1) 
}

