#source("0 General.R")
library(tidyverse)

breaks.theory = length(breakPositions.theory)
maxDistBlock = max(itiEnd)/1000 * sample.rate * 1.1 #max trial time (in seconds) * sampling rate * 10% buffer

physiology.trials.missing = tibble()
# physiology.markers = c() #additional check on missing triggers with little insight
# physiology.markers.length = tibble() #additional check on missing triggers with little insight
for (file in files.physio) {
  #file = files.physio %>% sample(1) #for testing
  data = file %>% 
    read_delim(delim="\t", na="", skip=9, progress=F, show_col_types=F) %>% suppressMessages() %>% 
    rename(EDA = "CH1", ECG = "CH2", Trigger = "CH28") %>%
    filter(Trigger <= 2^8) %>% select(Trigger)
  mst = data %>% pull(Trigger) %>% diff() %>% {. > 0} %>% which() %>% {. + 1}
  
  # mlength = data %>% pull(Trigger) %>% diff() %>% {. < 0} %>% which() %>% {. + 1} %>% {. - mst} #additional check on missing triggers with little insight
  # physiology.markers = physiology.markers %>% c(mlength) #additional check on missing triggers with little insight
  # physiology.markers.length = physiology.markers.length %>% bind_rows(tibble(subject = file %>% pathToCode(), min = min(mlength), max = max(mlength), mean = mean(mlength), sd = sd(mlength))) #additional check on missing triggers with little insight
  
  endFlags = c()
  if (exclusions.phys.trials[[file %>% pathToCode()]] %>% is.null() == F) {
    endFlags = "(after manual trial exclusion)" %>% c(endFlags, .)
    mst = mst[-exclusions.phys.trials[[file %>% pathToCode()]]]
  }
  
  markerDist = mst %>% diff()
  breakPositions.detected = which(markerDist > maxDistBlock)
  breaks.detected = length(breakPositions.detected)
  
  problem = length(mst)!=trials.n || breaks.detected!=breaks.theory || any(breakPositions.detected!=breakPositions.theory); problem.corrected = problem
  
  if (problem) {
    # if (breaks.detected - (trials.n - length(mst)) == breaks.theory)
    #   endFlags = "(some breaks are missing trials)" %>% c(endFlags, .)
    
    cat(paste0(file %>% pathToCode(), ": ", length(mst), " trials, ", 
               breaks.detected, " break(s) after ", paste(breakPositions.detected, collapse=", "), paste0(" ", endFlags, collapse = "; ")), "\n")
    
    #markerDist %>% sort(decreasing = T) %>% head(breaks.detected+2)
    gaps = tibble(dist.samples = markerDist) %>% mutate(trial = 1:n(), dist.trials = dist.samples %/% maxDistBlock) %>% relocate(trial) %>% arrange(desc(dist.samples))
    trials.missing = gaps %>% tail(-breaks.theory) %>% #assume longest breaks are correct ones
      arrange(trial) %>% mutate(trials.sum = cumsum(dist.trials)) %>% 
      filter(trials.sum <= trials.n - length(mst), dist.trials > 0)
    
    if (trials.missing %>% nrow() == 0) { #not too many gaps between trials => assume first trials are missing
      trials.corrected = trials.n #implied by assuming trials.missing == trials.n - length(mst)
      breakPositions.detected.corrected = breakPositions.detected + trials.n - length(mst)
      
      problem.corrected = trials.corrected!=trials.n || length(breakPositions.detected.corrected)!=breaks.theory || any(breakPositions.detected.corrected!=breakPositions.theory)
      if (problem.corrected==F) {
        physiology.trials.missing.s = tibble(subject = file %>% pathToCode(),
                                             trial = 1, trials.missing = trials.n - length(mst)) %>% 
          rowwise() %>% mutate(trials.missing.seq = seq.int(trial, trial + trials.missing - 1) %>% paste(collapse = ", ")) %>% ungroup()
        
        physiology.trials.missing = physiology.trials.missing %>% bind_rows(physiology.trials.missing.s)
        
        cat(paste0("successfully corrected ", file %>% pathToCode(), ": ", trials.corrected, " trials, ", 
                   length(breakPositions.detected.corrected), " break(s) after ", 
                   paste(breakPositions.detected.corrected, collapse=", "),
                   "; missing trials: ", paste(physiology.trials.missing.s %>% pull(trials.missing.seq), collapse=", ")), "\n")
      }
      
    } else { #too many gaps between trials => assume missing triggers
      breakPositions.detected.corrected = breakPositions.detected
      for (t in trials.missing %>% pull(trial)) {
        breakPositions.detected.corrected[breakPositions.detected > t & breakPositions.detected %in% {trials.missing %>% pull(trial)} == F] = 
          breakPositions.detected.corrected[breakPositions.detected > t & breakPositions.detected %in% {trials.missing %>% pull(trial)} == F] + 
          trials.missing %>% filter(trial==t) %>% pull(dist.trials)
      }
      trials.corrected = length(mst) + trials.missing %>% pull(trials.sum) %>% max()
      breakPositions.detected.corrected = breakPositions.detected.corrected %>% setdiff(trials.missing %>% pull(trial))
      
      problem.corrected = trials.corrected!=trials.n || length(breakPositions.detected.corrected)!=breaks.theory || any(breakPositions.detected.corrected!=breakPositions.theory)
      if (problem.corrected==F) { #problem has been corrected
        trials.missing = trials.missing %>% mutate(helper = 1, helper.sum = cumsum(helper)) %>% 
          transmute(trial = trial + helper.sum, trials.missing = dist.trials)
        
        physiology.trials.missing.s = trials.missing %>% mutate(subject = file %>% pathToCode()) %>% relocate(subject) %>% 
          rowwise() %>% mutate(trials.missing.seq = seq.int(trial, trial + trials.missing - 1) %>% paste(collapse = ", ")) %>% ungroup()
        
        physiology.trials.missing = physiology.trials.missing %>% bind_rows(physiology.trials.missing.s)
        
        cat(paste0("successfully corrected ", file %>% pathToCode(), ": ", trials.corrected, " trials, ", 
                   length(breakPositions.detected.corrected), " break(s) after ", 
                   paste(breakPositions.detected.corrected, collapse=", "),
                   "; missing trials: ", paste(physiology.trials.missing.s %>% pull(trials.missing.seq), collapse=", ")), "\n")
      }
    }
  }
  
  if (problem.corrected) { #previous assumptions not correct => assume last trial(s) are missing
    nextTrial = length(mst) + {trials.missing %>% pull(dist.trials) %>% sum()} + 1
    physiology.trials.missing = physiology.trials.missing %>% bind_rows(
      tibble(subject = file %>% pathToCode(), 
             trial = nextTrial,
             trials.missing = trials.n - nextTrial) %>% 
        rowwise() %>% mutate(trials.missing.seq = seq.int(trial, trial + trials.missing - 1) %>% paste(collapse = ", ")) %>% ungroup())
  }
  
  # if (problem) 
  #   warning(file)
}

# physiology.markers %>% Filter(\(x) (x < 9), .) %>% hist() #additional check on missing triggers with little insight
# physiology.markers.length #additional check on missing triggers with little insight

physiology.trials.missing.compact = physiology.trials.missing
physiology.trials.missing.compact %>% print(n = nrow(.))
physiology.trials.missing = physiology.trials.missing %>% select(subject, trials.missing.seq) %>% separate_longer_delim(trials.missing.seq, ", ") %>% rename(trial = trials.missing.seq)

##when merging conditions, you can account for missing trials using anti_join:
# conditions = physiology.trials.missing %>% select(subject) %>% unique() %>% 
#     crossing(tibble(trial = 1:max(physiology.trials.missing %>% pull(trial)))) %>% 
#   mutate(condition = sample.int(4, size=n(), replace=T))
# 
## physiology.trials.missing %>% left_join(conditions) #check missing conditions
# conditions.reduced = conditions %>% anti_join(physiology.trials.missing)
# tibble(conditions.n = conditions %>% nrow(),
#        conditions.reduced.n = conditions.reduced %>% nrow(),
#        missings.n = physiology.trials.missing %>% nrow()) %>% mutate(check = conditions.n == conditions.reduced.n + missings.n)

physiology.trials.missing %>% write_rds("physiology.trials.missing.rds" %>% paste0(path.rds, .))
