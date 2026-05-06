#source("0 General.R")
library(tidyverse)

outlier.z = 2 * c(-1, 1) #z score(s) that an RT must exceed (within participant) in order to be classified as an outlier

## don't the the following! filter by block instead!
# files.behavior.checker = tibble(filename = files.behavior) %>% 
#   mutate(filename = filename %>% gsub(".*/", "", .)) %>% #get rid of path
#   separate(filename, into = c("subject", NA, NA, "date", "time", NA), sep = "_")
# 
# #files.behavior.checker %>% count(subject) %>% arrange(desc(n)) #more than 1 training session possible
# 
# files.behavior.checker = files.behavior.checker %>% 
#   mutate(.by = subject, 
#          n = 1:n(),
#          block = n - max(n) + 4) %>% 
#   mutate(analysis = block > 0)
# 
# #files.behavior.checker %>% pull(analysis) %>% sum() %>% {. %% 4 == 0} #check that number of files is multiple of 4
# 
# files.behavior.analysis = files.behavior[files.behavior.checker %>% pull(analysis)]

behavior = files.behavior %>% 
  map(read_csv, na = c("", "NA", "None"), show_col_types = F) %>% #rt can be "None"
  map(function(x) {if (x %>% ncol() != 0) 
    x %>% mutate(participant = as.character(participant)) %>% 
      mutate(trial = as.integer(trial)) %>% 
      mutate(anticipation = as.double(anticipation)) %>% 
      mutate(face_pos = as.double(face_pos)) %>% 
      rename(target_pos = taget_pos) %>% 
      mutate(target_pos = as.double(target_pos)) %>% 
      mutate(keyAssignment = as.integer(keyAssignment)) %>% 
      mutate(correct = as.logical(correct)) %>% 
      mutate(rt = as.double(rt))}) %>% 
  bind_rows()

behavior = behavior %>% 
  rename(subject = participant) %>% 
  select(subject, block, everything()) %>% #no "block" column in training
  filter(block %>% is.na() == F) #discard training blocks

#behavior %>% pull(face) %>% unique() %>% grepl("_n_", .) %>% mean() #works!
behavior = behavior %>% mutate(emotion = if_else(face %>% grepl("_n_", .), "neutral", "angry"))

behavior %>% count(subject) %>% filter(n != trials.n)
blocks.missing = behavior %>% select(subject, block) %>% unique() %>% complete(subject, block) %>% 
  anti_join(behavior %>% select(subject, block) %>% unique()) %>% rename(block.missing = block)
#subject 21: 3rd block missing
#subject 36: blocks 1-3 missing
#subject 46: block 4 missing
exclusions = blocks.missing %>% count(subject) %>% filter(n > 2) %>% pull(subject) %>% c(exclusions) %>% unique() %>% sort()

behavior = behavior %>% 
  filter(subject %in% exclusions == F) %>% 
  mutate(.by = subject,
         #outlier correction
         outlier = abs((rt - mean(rt, na.rm=T))/sd(rt, na.rm=T)) > outlier.z,
         rt = Winsorize.z(rt, outlier.z),
         outlier = if_else(outlier %>% is.na(), F, outlier) #mark NAs as FALSE
  )

behavior %>% write_rds("behavior.rds" %>% paste0(path.rds, .))
