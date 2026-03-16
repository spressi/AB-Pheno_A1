#source("0 General.R")
library(tidyverse)

# Read .edf data into EyeLink DataViewer (multiple import without subfolders)
# Create summary reports: Fixations.txt & Messages.txt

exclusions.eye = c() %>% c(exclusions) %>% unique() %>% sort()

#eye baseline validation
baseline = c(-300, 0) #Baseline in ms relative to expoID onset; min(baseline) = start; max(baseline) = end
expoID = "fixCrossEnd" #marker for end of baseline (1st message of each trial)
expoID2 = "Anticipation" #marker for beginning of anticipation phase (2nd message of each trial)
expoID3 = "kongruent" #marker for beginning of stimuli/cues (3rd message of each trial)
#useAllBaselines = list("83" = 2, "84" = 3) #manually allow all baselines of vp83 phase 2 (Gen1) & vp84 phase 3 (Gen2)
saveBaselinePlots = F
driftPlots = T #c("vp30", "vp33")
maxDeviation_rel = 3 #max abs value of z-score
outlierLimit.eye = .5 #maximum percentage of invalid baselines per subject
maxSpread = 150 #maximum spread of valid baselines (diameter / edge length)
usePointDistance = T #use point (vector) distance instead of coordinates independently?
source("eye_helpers.R")

#ROI analysis
validFixTime.trial = .5 #percentage of valid fixation time within trial in order to analyze trial
validFixTime.subj = .5 #percentage of trials with sufficient valid fixation time in order to analyze subject


# Load Data ---------------------------------------------------------------
eye.messages = "Messages.txt" %>% paste0(path.eye, .) %>% 
  read_delim(delim="\t", locale=locale(decimal_mark=","), na=".", show_col_types=F) %>% 
  rename(subject_full = RECORDING_SESSION_LABEL,
         trial = TRIAL_LABEL,
         time = CURRENT_MSG_TIME,
         message = CURRENT_MSG_TEXT) %>% 
  filter(message %>% grepl("!MODE", .) == F) %>% 
  mutate(subject = subject_full %>% gsub("_.*", "", .),
         trial = trial %>% sub("Trial: ","", .) %>% as.integer())

eye.subjects = eye.messages %>% 
  filter(message %>% grepl("Anticipation", .) == F) %>% 
  filter(message %>% grepl("training", .) == F) %>% 
  pull(subject_full) %>% unique()

eye.messages = eye.messages %>% 
  filter(subject_full %in% eye.subjects) %>% #get rid of training sessions
  mutate(.by = subject, block = subject_full %>% as.factor() %>% as.integer()) %>% 
  mutate(block = if_else(subject == 21 & block == 3, 4, block)) %>% #switch 3rd block of subject 21 to 4 (3rd block missing)
  mutate(subject_block = paste0(subject, "_", block)) %>% 
  select(subject, block, subject_block, everything())

eye.blocks = eye.messages %>% select(subject_full, block) %>% unique()
eye.messages = eye.messages %>% bind_rows(
  eye.messages %>% filter(message %>% grepl(expoID2, .)) %>% 
    mutate(message = message %>% gsub(expoID2, expoID, .))
) %>% mutate(time = if_else(message %>% grepl(expoID, .), time + fixCross, time), #adjust for message being sent at beginning of routine but anticipation rectangles being shown later. Check how Anticipation message time + trial message anticipation duration (3rd argument) now aligns with trial message time
             time = if_else(message %>% grepl(expoID2, .), time + anticipationStart, time)) %>% #adjust for message being sent at beginning of routine but anticipation rectangles being shown later. Check how Anticipation message time + trial message anticipation duration (3rd argument) now aligns with trial message time
  arrange(subject, block, trial, time)
# eye.messages.anticipation = eye.messages %>% filter(message %>% grepl(expoID2, .))
# eye.messages.cue = eye.messages %>% filter(message %>% grepl(expoID2, .) == F)

eye.fixations = "Fixations.txt" %>% paste0(path.eye, .) %>% 
  read_delim(delim="\t", locale=locale(decimal_mark=","), na=".", show_col_types=F) %>% 
  rename(subject_full = RECORDING_SESSION_LABEL,
         trial = TRIAL_LABEL,
         start = CURRENT_FIX_START,
         end = CURRENT_FIX_END,
         x = CURRENT_FIX_X,
         y = CURRENT_FIX_Y) %>% 
  filter(subject_full %in% eye.subjects) %>% #get rid of training sessions
  left_join(eye.blocks) %>% 
  mutate(subject = subject_full %>% gsub("_.*", "", .),
         subject_block = paste0(subject, "_", block),
         trial = trial %>% sub("Trial: ","", .) %>% as.integer(),
         y = screen.height - y) %>% 
  select(subject, block, subject_block, everything())


# Valid Fixations ---------------------------------------------------------
eye.fixations.valid.trial = eye.fixations %>% 
  left_join(eye.messages %>% filter(message %>% str_detect(expoID2)) %>% rename(onset = time) %>% select(-message)) %>% 
  left_join(eye.messages %>% filter(message %>% str_detect(expoID3)) %>% rename(offset = time) %>% select(-message)) %>% 
  mutate(end = ifelse(end > offset, offset, end), #discard fraction of fixation after cue onset (i.e., anticipation offset)
         start = start - onset, end = end - onset, #realign such that 0 = anticipation start
         start = ifelse(start < 0, 0, start), #discard fraction of fixation before anticipation
         dur = end - start) %>% filter(dur > 0) %>%
  summarize(.by = c(subject, trial, block),
            valid = sum(dur) / (first(offset) - first(onset)))

eye.fixations.valid.trial %>% 
  ggplot(aes(x = valid)) + facet_wrap(~block, labeller = "label_both") +
  geom_vline(xintercept = validFixTime.trial, color = "red", linewidth = 1.5) + #linetype = "dashed", 
  geom_histogram(color = "black") +
  myGgTheme

#exclude trials with insufficient valid fixations (need not be validated for their baseline)
eye.fixations.valid = eye.fixations.valid.trial %>% filter(valid > validFixTime.trial) %>%
  left_join(eye.fixations %>% mutate(trial = as.numeric(trial)), by=c("subject", "trial", "block")) %>% select(-valid)


# Baseline Validation -----------------------------------------------------
baselines.summary.block = validateBaselines(eye.fixations.valid %>% select(-subject) %>% rename(subject = subject_block), 
                                      eye.messages %>% select(-subject) %>% rename(subject = subject_block), 
                                      exclusions.eye, maxDeviation_rel, maxSpread, saveBaselinePlots, postfix="") #%>% select(subject, block, everything())
baselines.summary.block = baselines.summary.block %>% tibble() %>% 
  rename(subject_block = subject) %>% separate(subject_block, c("subject", "block"), remove=F) %>% 
  mutate(included_block = invalid <= outlierLimit.eye & range_x <= maxSpread & range_y <= maxSpread,
         block = block %>% as.integer())
baselines.trial = baselines.trial %>% bind_rows(.id = "subject") %>% tibble() %>% 
  rename(subject_block = subject) %>% separate(subject_block, c("subject", "block"), remove=F) %>% 
  mutate(block = block %>% as.integer()) %>% 
  mutate(.by = c(subject_block), trial = 1:n())


baselines.summary = baselines.summary.block %>% 
  summarize(.by=subject, 
            across(invalid:range_y, \(x) x %>% mean(na.rm=T))) %>% 
  mutate(included = invalid <= outlierLimit.eye & range_x <= maxSpread & range_y <= maxSpread)

exclusions.eye.baseline = baselines.summary %>% filter(included == F) %>% pull(subject) %>% unique()
exclusions.eye = exclusions.eye.baseline %>% c(exclusions.eye) %>% unique() %>% sort()
baselines.summary.valid = baselines.summary %>% filter(subject %in% exclusions.eye == F)


# ROI Analysis ------------------------------------------------------------
eye = tibble()
for (code in baselines.summary.valid %>% pull(subject) %>% unique() %>% sort()) {
  #code = inclusions.eye.baseline %>% sample(1)
  cat(paste(code, "... "))
  
  baselines.vp = baselines.trial %>% filter(subject == code)
  mes.vp = eye.messages %>% filter(subject == code)
  fix.vp = eye.fixations.valid %>% filter(subject == code) %>% 
    left_join(baselines.vp %>% rename(blx = x, bly = y), by = join_by(subject, block, subject_block, trial)) %>% 
    left_join(baselines.vp %>% filter(blok) %>% summarize(.by=block, x = mean(x), y = mean(y)) %>% rename(blx.avg = x, bly.avg = y), by = join_by(block)) %>% 
    mutate(x = x - if_else(blok, blx, blx.avg),
           y = y - if_else(blok, bly, bly.avg)) %>% 
    select(subject:y, condition) %>% #drop baseline parameters
    select(-subject_full) %>% #better overview
    mutate(roi = case_when(x <= leftRoi ~ "left",
                           x >= rightRoi ~ "right",
                           T ~ "center"))
  
  for (block_i in fix.vp %>% pull(block) %>% unique() %>% sort()) {
    #block_i = 1
    fix.vp.block = fix.vp %>% filter(block==block_i)
    mes.vp.block = mes.vp %>% filter(block==block_i)
    for (trial_i in fix.vp.block %>% pull(trial) %>% unique() %>% sort()) {
      #trial_i = 1
      fix.trial = fix.vp.block %>% filter(trial==trial_i)
      mes.trial = mes.vp.block %>% filter(trial==trial_i)
      
      #anticipation
      #anticipation.start = mes.trial %>% filter(message %>% grepl(expoID, .)) %>% pull(time)
      anticipation.start = mes.trial %>% filter(message %>% grepl(expoID2, .)) %>% pull(time)
      anticipation.end = mes.trial %>% filter(message %>% grepl(expoID3, .)) %>% pull(time) #also checks for "inkongruent"
      fix.trial.antic = fix.trial %>% 
        mutate(start = start - anticipation.start,
               start = if_else(start < 0, 0, start),
               end = if_else(end > anticipation.end, anticipation.end, end), #anticipation.end is not time corrected => prune times before adjusting by anticipation.start
               end = end - anticipation.start,
               dur = end - start) %>% filter(dur > 0) %>% 
        select(subject, block, trial, start, end, dur, x, y, roi) %>% 
        mutate(dist = pointDistance(lag(x), lag(y), x, y))
      
      result = tibble(subject = code, block = block_i, trial = trial_i) %>% 
        bind_cols(fix.trial.antic %>% summarize(antic_dist = sum(dist, na.rm=T))) %>% #scanpath in pixels
        bind_cols(fix.trial.antic %>% filter(roi != "center") %>% #only switches between left and right
                    summarize(antic_roiSwitches = {roi != lag(roi)} %>% sum(na.rm=T)))
      
      #cue
      cue.start = anticipation.end #mes.trial %>% filter(message %>% grepl(expoID3, .)) %>% pull(time)
      cue.end = cue.start + cueTime
      cue.conditions = mes.trial %>% filter(message %>% grepl(expoID3, .)) %>% select(message) %>% 
        separate(message, into=c(NA, NA, "antic", "pic", "picPos", "sound", "congruency", "target", "targetPos"), sep=", ") %>%
        #mutate(emotion = if_else(pic %>% grepl("_n_", .), "neutral", "angry"))
        separate(pic, into=c("model", "model_age", "model_gender", "emotion", "snapshot", "fileExt"), sep="_|\\.") %>% #, remove = F
        mutate(emotion = emotion %>% case_match("a" ~ "angry", "d" ~ "disgusted", "f" ~ "fearful", "n" ~ "neutral", "s" ~ "sad"),
               model_age = model_age %>% case_match("y" ~ "young", "m" ~ "middle", "o" ~ "old"),
               model_gender = model_gender %>% case_match("m" ~ "male", "f" ~ "female"))
      cueSide = cue.conditions %>% 
        pull(picPos) %>% as.numeric() %>% {if_else(. < 0, "left", "right")}
      cueEmotion = cue.conditions %>% pull(emotion)
      
      fix.trial.cue = fix.trial %>% 
        mutate(start = start - cue.start,
               start = if_else(start < 0, 0, start),
               end = end - cue.start,
               end = if_else(end > cueTime, cueTime, end),
               dur = end - start) %>% filter(dur > 0, start < cueTime) %>% 
        select(subject, block, trial, start, end, dur, x, y, roi) %>% 
        filter(start > 0) %>% #discard initial fixation (from anticipation phase)
        summarize(.by = roi,
                  cue_dwell = sum(dur)/cueTime,
                  cue_lat = min(start)) %>% 
        # filter(roi != "center") %>% 
        # pivot_wider(names_from = roi, values_from = -roi) %>% 
        filter(roi == cueSide) %>% select(-roi)
      
      if (nrow(fix.trial.cue) == 0) fix.trial.cue = tibble(cue_dwell = 0, cue_lat = cueTime)
      
      result = result %>% bind_cols(fix.trial.cue %>% mutate(emotion = cueEmotion)) #TODO bind_cols(cue.conditions) instead?
      
      #TODO analyze target phase? using congruency
      
      eye = eye %>% bind_rows(result)
    }
  }
}

eye %>% write_rds("eye.rds" %>% paste0(path.rds, .))
