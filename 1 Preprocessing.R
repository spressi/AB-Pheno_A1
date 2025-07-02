source("0 General.R")

#RTs
outlier.z = 2 #z score that an RT must exceed (within participant) in order to be classified as an outlier


exclusions.eye = c()

#eye baseline validation
baseline = c(-300, 0) #Baseline in ms relative to expoID onset; min(baseline) = start; max(baseline) = end
expoID = "fixCrossEnd"
#useAllBaselines = list("83" = 2, "84" = 3) #manually allow all baselines of vp83 phase 2 (Gen1) & vp84 phase 3 (Gen2)
saveBaselinePlots = F
driftPlots = T #c("vp30", "vp33")
maxDeviation_rel = 3 #max abs value of z-score
outlierLimit.eye = .5 #maximum percentage of invalid baselines per subject
maxSpread = 150 #maximum spread of valid baselines (diameter / edge length)
usePointDistance = T #use point (vector) distance instead of coordinates independently?
source("eye_helpers.R")


# Questionnaires ----------------------------------------------------------
# source(file.que.r); questionnaires = ds; rm(ds) #not working
questionnaires = file.que %>% read_csv2() %>% 
  rename(subject = VP01s, age = SD01, gender = SD04, 
         hand = SD06, edu = SD02, work = SD03, comment = SD07_01) %>% 
  rename_with(function(x) {gsub("FB02_", "stai", x)}) %>% 
  select(subject, everything()) %>% 
  filter(subject %>% grepl("test", .) == F) %>% #discard test
  filter(subject %>% duplicated(fromLast = T) == F) %>% #always keep last instance of subject number
  mutate(age = age + 17, #don't ask why xD... first response selection is "18")
         gender = gender %>% case_match(1 ~ "male",
                                        2 ~ "female",
                                        3 ~ "non-binary") %>% as_factor(),
         #reverse coding is already done implicitly within SoSciSurvey (cf. Codebook)
         #across(paste0("sias", c("05", "09", "11"), function(x) return(6 - x))),
  )
questionnaires = questionnaires %>% 
  mutate(sias = questionnaires %>% select(starts_with("sias")) %>% rowMeans(),
         stai = questionnaires %>% select(starts_with("stai")) %>% rowMeans()) %>% 
  select(subject, gender, age, sias, stai)

# Behavior ----------------------------------------------------------------
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

behavior.problems = behavior %>% count(subject) %>% filter(n != 256) %>% pull(subject)
behavior %>% select(subject, block) %>% unique() %>% filter(subject %in% behavior.problems)
#subject 10: block 1 started again after 3 trials => moved to subfolder in order to ignore
#subject 21: 3rd block missing

behavior = behavior %>% 
  mutate(.by = subject,
         #outlier correction
         outlier = abs((rt - mean(rt, na.rm=T))/sd(rt, na.rm=T)) > outlier.z,
         outlier = if_else(outlier %>% is.na(), F, outlier) #mark NAs as FALSE
  )


# Eye-Tracking ------------------------------------------------------------
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
  eye.messages %>% filter(message %>% grepl("Anticipation", .)) %>% 
    mutate(message = message %>% gsub("Anticipation", expoID, .))
) %>% mutate(time = if_else(message %>% grepl(expoID, .), time + fixCross, time), #adjust for message being sent at beginning of routine but anticipation rectangles being shown later. Check how Anticipation message time + trial message anticipation duration (3rd argument) now aligns with trial message time
             time = if_else(message %>% grepl("Anticipation", .), time + anticipationStart, time)) %>% #adjust for message being sent at beginning of routine but anticipation rectangles being shown later. Check how Anticipation message time + trial message anticipation duration (3rd argument) now aligns with trial message time
  arrange(subject, block, trial, time)
# eye.messages.anticipation = eye.messages %>% filter(message %>% grepl("Anticipation", .))
# eye.messages.cue = eye.messages %>% filter(message %>% grepl("Anticipation", .) == F)

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

#TODO check valid fixations
eye.fixations.valid = eye.fixations
#eye.fixations.valid = eye.fixations %>% mutate()
# eye.valid.trial = fixations %>% mutate(start = start - preStim, end = end - preStim, #realign such that 0 = stim start
#                                        start = ifelse(start < 0, 0, start), #discard fraction of fixation before stimulus
#                                        end = ifelse(end > ratingStart, ratingStart, end), #discard fraction of fixation after rating onset
#                                        dur = end - start) %>% filter(dur > 0) %>% 
#   #filter(block >= 2) %>% #only generalization
#   group_by(subject, trial, block) %>% summarise(valid = sum(dur) / ratingStart)
# #group_by(subject) %>% summarise(valid = sum(dur) / ratingStart / length(unique(trial)))
# with(eye.valid.trial %>% filter(block==2), hist(valid, breaks=seq(0, 1, length.out=20+1), main=paste0("Valid Fixation Time (Block ", unique(block), ")"))); abline(v=validFixTime.trial, col="red", lwd=3, lty=2)
# with(eye.valid.trial %>% filter(block==3), hist(valid, breaks=seq(0, 1, length.out=20+1), main=paste0("Valid Fixation Time (Block ", unique(block), ")"))); abline(v=validFixTime.trial, col="red", lwd=3, lty=2)
# 
# #exclude trials with insufficient valid fixations (need not be validated for their baseline)
# fixations.valid = eye.valid.trial %>% filter(valid > validFixTime.trial) %>% 
#   left_join(fixations %>% mutate(trial = as.numeric(trial)), by=c("subject", "trial", "block")) %>% select(-valid)

baselines.summary = validateBaselines(eye.fixations.valid %>% select(-subject) %>% rename(subject = subject_block), 
                                      eye.messages %>% select(-subject) %>% rename(subject = subject_block), 
                                      exclusions.eye, maxDeviation_rel, maxSpread, saveBaselinePlots, postfix="") #%>% select(subject, block, everything())
baselines.summary = baselines.summary %>% tibble() %>% 
  rename(subject_block = subject) %>% separate(subject_block, c("subject", "block"), remove=F) %>% 
  mutate(included = invalid <= outlierLimit.eye & range_x <= maxSpread & range_y <= maxSpread,
         block = block %>% as.integer())
baselines.trial = baselines.trial %>% bind_rows(.id = "subject") %>% tibble() %>% 
  rename(subject_block = subject) %>% separate(subject_block, c("subject", "block"), remove=F) %>% 
  mutate(block = block %>% as.integer()) %>% 
  mutate(.by = c(subject_block), trial = 1:n())


baselines.summary %>% summarize(.by=subject, included = mean(included)) %>% arrange(included)
baselines.summary %>% pull(included) %>% mean()

exclusions.eye.baseline = baselines.summary %>% filter(included == F) %>% pull(subject) %>% unique()
baselines.summary.valid = baselines.summary %>% filter(subject %in% exclusions.eye.baseline == F)

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
      anticipation.start = mes.trial %>% filter(message %>% grepl("Anticipation", .)) %>% pull(time)
      anticipation.end = mes.trial %>% filter(message %>% grepl("kongruent", .)) %>% pull(time) #also checks for "inkongruent"
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
      cue.start = anticipation.end #mes.trial %>% filter(message %>% grepl("kongruent", .)) %>% pull(time)
      cue.end = cue.start + cueTime
      cue.conditions = mes.trial %>% filter(message %>% grepl("kongruent", .)) %>% select(message) %>% 
        separate(message, into=c(NA, NA, "antic", "pic", "picPos", "sound", "congruency", "target", "targetPos"), sep=", ") %>% 
        mutate(emotion = if_else(pic %>% grepl("_n_", .), "neutral", "angry"))
      cueSide = cue.conditions %>% 
        pull(picPos) %>% as.numeric() %>% {if_else(. < 0, "left", "right")}
      cueEmotion = cue.conditions %>% pull(emotion)
      
      fix.trial.cue = fix.trial %>% 
        mutate(start = start - cue.start,
               start = if_else(start < 0, 0, start),
               end = end - cue.start,
               end = if_else(end > cue.end, cue.end, end),
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
      
      result = result %>% bind_cols(fix.trial.cue %>% mutate(emotion = cueEmotion))
      
      #TODO target phase? using congrency
      
      eye = eye %>% bind_rows(result)
    }
  }
}

# ECG ---------------------------------------------------------------------

