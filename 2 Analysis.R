#source("1 Preprocessing.R")

library(tidyverse)
library(ez)
library(apa)


# Read Preprocessed Files -------------------------------------------------
behavior = read_rds("behavior.rds" %>% paste0(path.rds, .))
questionnaires = read_rds("questionnaires.rds" %>% paste0(path.rds, .))
eye.valid = read_rds("eye.valid.rds" %>% paste0(path.rds, .))
#TODO add physio


# Apply Exclusions --------------------------------------------------------
subjects.analysis = behavior %>% pull(subject) %>% 
  intersect(questionnaires %>% pull(subject)) %>% 
  intersect(eye.valid %>% pull(subject))
#TODO add physio

behavior = behavior %>% filter(subject %in% subjects.analysis)
questionnaires = questionnaires %>% filter(subject %in% subjects.analysis)
eye.valid = eye.valid %>% filter(subject %in% subjects.analysis)
#TODO add physio


# Descriptives ------------------------------------------------------------
questionnaires %>% count(gender)
questionnaires %>% summarize(across(c(age, sias, stai), list(m = mean, sd = sd), na.rm=T))

questionnaires %>% ggplot(aes(x = sias)) + geom_histogram(fill = "orange", color = "black") + myGgTheme
questionnaires %>% ggplot(aes(x = stai)) + geom_histogram(fill = "violet", color = "black") + myGgTheme


# RT ----------------------------------------------------------------------

# * RT Data Quality -------------------------------------------------------
behavior.quality = behavior %>% 
  summarize(.by = subject,
            #valid = mean(correct & !outlier, na.rm=T),
            correct = mean(correct, na.rm=T),
            outlier = mean(outlier, na.rm=T)) %>% 
  #arrange(valid)
  arrange(correct)

behavior.quality
#Discussion: should have preregistered a criterion to remove subjects with too many incorrect answers (subject 30)

behavior.quality %>% summarize(across(-subject, list(m = mean, sd = sd), na.rm=T))

#TODO add reliability

# * RT Analysis -----------------------------------------------------------
behavior.rt = behavior %>% filter(correct) %>% 
  summarize(.by = c(subject, emotion, congruency), #, face_pos, target, keyAssignment
            rt = mean(rt, na.rm=T)) %>% 
  mutate(rt = rt * 1000) #convert to ms (readability of axis for plots)

behavior.rt %>% ez::ezANOVA(dv = rt,
                            wid = subject,
                            within = c(emotion, congruency),
                            detailed = T) %>% apa::anova_apa(force_sph_corr = T)

behavior.rt %>% summarize(.by = c(emotion, congruency),
                          rt.sd = sd(rt, na.rm=T),
                          rt.se = se(rt, na.rm=T),
                          rt = mean(rt, na.rm=T)) %>% 
  ggplot(aes(x = congruency, y = rt, color = emotion)) +
  geom_errorbar(aes(ymin = rt - rt.se, ymax = rt + rt.se), position = position_dodge(width = .5), width = .5, linewidth = 2) +
  geom_point(position = position_dodge(width = .5), size = 6) +
  ylab("Reaction Time (ms)") + myGgTheme



# Accuracy ----------------------------------------------------------------
behavior.acc = behavior %>% 
  summarize(.by = c(subject, emotion, congruency), #, face_pos, target, keyAssignment
            accuracy = mean(correct, na.rm=T)) %>% 
  mutate(accuracy = accuracy * 100) #convert to %

behavior.acc %>% ez::ezANOVA(dv = accuracy,
                             wid = subject,
                             within = c(emotion, congruency),
                             detailed = T) %>% apa::anova_apa(force_sph_corr = T)

behavior.acc %>% summarize(.by = c(emotion, congruency),
                           accuracy.sd = sd(accuracy, na.rm=T),
                           accuracy.se = se(accuracy, na.rm=T),
                           accuracy = mean(accuracy, na.rm=T)) %>% 
  ggplot(aes(x = congruency, y = accuracy, color = emotion)) +
  geom_errorbar(aes(ymin = accuracy - accuracy.se, ymax = accuracy + accuracy.se), position = position_dodge(width = .5), width = .5, linewidth = 2) +
  geom_point(position = position_dodge(width = .5), size = 6) +
  ylab("Accuracy (%)") + myGgTheme



# Eye: Anticipation -------------------------------------------------------
eye.antic = eye %>% select(subject:trial, starts_with("antic_")) %>% 
  summarize(.by = subject,
            antic_dist = mean(antic_dist, na.rm=T),
            antic_roiSwitches = mean(antic_roiSwitches, na.rm=T)) %>% 
  mutate(across(-subject, list(z_sq = function(x) {x = scale(x)[,1]^2}))) %>% 
  left_join(questionnaires)

#Scan Path Length
eye.antic %>% pull(antic_dist) %>% summary()
eye.antic %>% ggplot(aes(x = antic_dist)) + geom_histogram(fill = "red", color = "black") + myGgTheme
eye.antic %>% summarize(across(antic_dist, list(m = mean, sd = sd)))

with(eye.antic, cor.test(antic_dist_z_sq, sias, alternative="greater")) %>% apa::cor_apa(r_ci=T)
with(eye.antic, cor.test(antic_dist_z_sq, stai, alternative="greater")) %>% apa::cor_apa(r_ci=T)

#ROI Switches
eye.antic %>% pull(antic_roiSwitches) %>% summary()
eye.antic %>% ggplot(aes(x = antic_roiSwitches)) + geom_histogram(fill = "red4", color = "black") + myGgTheme
eye.antic %>% summarize(across(antic_roiSwitches, list(m = mean, sd = sd)))

with(eye.antic, cor.test(antic_roiSwitches_z_sq, sias, alternative="greater")) %>% apa::cor_apa(r_ci=T)
with(eye.antic, cor.test(antic_roiSwitches_z_sq, stai, alternative="greater")) %>% apa::cor_apa(r_ci=T)

#Intercorrelation: Scan Path & ROI switches
with(eye.antic, cor.test(antic_dist, antic_roiSwitches)) %>% apa::cor_apa(r_ci=T)


# Eye: Cue ----------------------------------------------------------------
eye.cue = eye %>% select(subject:trial, emotion, starts_with("cue_")) %>% 
  summarize(.by = c(subject, emotion),
            cue_dwell = mean(cue_dwell, na.rm=T),
            cue_lat = mean(cue_lat, na.rm=T)) %>% 
  pivot_wider(id_cols = subject, names_from = emotion, values_from = starts_with("cue_"))

#Latency
with(eye.cue, t.test(cue_lat_angry, cue_lat_neutral, 
                     alternative = "less", paired=T)) %>% apa::t_apa(es_ci=T)
eye.cue %>% summarize(cue_lat_angry = mean(cue_lat_angry),
                      cue_lat_neutral = mean(cue_lat_neutral))

#Dwell  Time
with(eye.cue, t.test(cue_dwell_angry, cue_dwell_neutral, 
                     alternative = "greater", paired=T)) %>% apa::t_apa(es_ci=T)
eye.cue %>% summarize(cue_dwell_angry = mean(cue_dwell_angry),
                      cue_dwell_neutral = mean(cue_dwell_neutral))

#intercorrelation between Latency and Dwell
with(eye.cue, cor.test(cue_lat_angry, cue_dwell_angry)) %>% apa::cor_apa(r_ci=T)
with(eye.cue, cor.test(cue_lat_neutral, cue_dwell_neutral)) %>% apa::cor_apa(r_ci=T)
#TODO ANOVA with continuous predictor to check significance of interaction

#TODO physio

# For bachelor student ----------------------------------------------------
# behavior.rt %>% full_join(behavior.acc) %>% 
#   pivot_wider(id_cols = subject, names_from = c(emotion, congruency), values_from = c(rt, accuracy)) %>% 
#   full_join(questionnaires, .) %>% #questionnaires first (but not in code because of pivot_wider)
#   full_join(eye.antic) %>% 
#   full_join(eye.cue) %>% 
#   arrange(subject) %>% 
#   write_csv("data/jamovi.csv")
