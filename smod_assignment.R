rm(list = ls())
library(tidyverse)

# smods <- c("Pip", "Susanne", "Lucia", "Rolf")

smod_assignment <- function(smodList = c("Pip", "Rolf", "Lucia", "Susanne")){

srtlers <- c("Alyssa", "Amelia", "Andee", "Carl", "Jill", "Katie", "Kym", 
                  "Lonneke", "Lucia", "Matt", "Neil", "Pip", "Rolf", "Sibel", 
                  "Susanne", "Tim", "Yannik")

  
  # subtract 1 so we don't double-count the smods themselves
  minAssign <- floor(length(srtlers) / length(smodList)) - 1  
  extra <- length(srtlers) %% length(smodList)
  
  SmallGroups <- 
    tibble(srtlers) %>%
    filter(!srtlers %in% smodList) %>%
    mutate(smod = c(rep(smodList, minAssign), 
                    sample(smodList, extra)), 
           srtlers = sample(srtlers)) %>%
    arrange(smod) 
  return(SmallGroups)
}

