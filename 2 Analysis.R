source("1 Preprocessing.R") #TODO: rather work with imports of RDS files? (data/prepro)

library(ez)
library(apa)


# Descriptives ------------------------------------------------------------
questionnaires %>% select(age:stai) %>% summarize(across(everything(), list(m = mean, sd = sd), na.rm=T))
questionnaires %>% count(gender)

# RT ----------------------------------------------------------------------
behavior.exclude = behavior %>% 
  summarize(.by = subject,
            valid = mean(correct & !outlier, na.rm=T),
            correct = mean(correct, na.rm=T),
            outlier = mean(outlier, na.rm=T)) %>% 
  arrange(valid) %>% 
  filter(valid < .75) %>% pull(subject)

behavior.analysis = behavior %>% filter(subject %in% behavior.exclude == F)

behavior.rt = behavior.analysis %>% filter(correct & !outlier) %>% 
  summarize(.by = c(subject, emotion, congruency), #, face_pos, target, keyAssignment
            rt = mean(rt, na.rm=T))

behavior.rt %>% ez::ezANOVA(dv = rt,
                            wid = subject,
                            within = c(emotion, congruency),
                            detailed = T) %>% apa::anova_apa(sph_corr = "gg", force_sph_corr = T)

behavior.rt %>% summarize(.by = c(emotion, congruency),
                          rt.sd = sd(rt, na.rm=T),
                          rt.se = se(rt, na.rm=T),
                          rt = mean(rt, na.rm=T)) %>% 
  ggplot(aes(x = congruency, y = rt, color = emotion)) +
  geom_errorbar(aes(ymin = rt - rt.se, ymax = rt + rt.se), position = position_dodge(width = .5), width = .5) +
  geom_point(position = position_dodge(width = .5))



# Accuracy ----------------------------------------------------------------
behavior.acc = behavior.analysis %>% 
  summarize(.by = c(subject, emotion, congruency), #, face_pos, target, keyAssignment
            accuracy = mean(correct, na.rm=T))

behavior.acc %>% ez::ezANOVA(dv = accuracy,
                             wid = subject,
                             within = c(emotion, congruency),
                             detailed = T) %>% apa::anova_apa(sph_corr = "gg", force_sph_corr = T)

behavior.acc %>% summarize(.by = c(emotion, congruency),
                           accuracy.sd = sd(accuracy, na.rm=T),
                           accuracy.se = se(accuracy, na.rm=T),
                           accuracy = mean(accuracy, na.rm=T)) %>% 
  ggplot(aes(x = congruency, y = accuracy, color = emotion)) +
  geom_errorbar(aes(ymin = accuracy - accuracy.se, ymax = accuracy + accuracy.se), position = position_dodge(width = .5), width = .5) +
  geom_point(position = position_dodge(width = .5))



# Eye: Anticipation -------------------------------------------------------
eye.antic = eye %>% select(subject:trial, starts_with("antic_")) %>% 
  summarize(.by = subject,
            antic_dist = mean(antic_dist, na.rm=T),
            antic_roiSwitches = mean(antic_roiSwitches, na.rm=T)) %>% 
  mutate(across(-subject, list(z_sq = function(x) {x = scale(x)[,1]^2}))) %>% 
  left_join(questionnaires)

#Scan Path Length
eye.antic %>% pull(antic_dist) %>% summary()
eye.antic %>% pull(antic_dist) %>% hist()
eye.antic %>% summarize(across(antic_dist, list(m = mean, sd = sd)))

with(eye.antic, cor.test(antic_dist_z_sq, sias, alternative="greater")) %>% apa::cor_apa(r_ci=T)
with(eye.antic, cor.test(antic_dist_z_sq, stai, alternative="greater")) %>% apa::cor_apa(r_ci=T)

#ROI Switches
eye.antic %>% pull(antic_roiSwitches) %>% summary()
eye.antic %>% pull(antic_roiSwitches) %>% hist()
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

#Dwell  Time
with(eye.cue, t.test(cue_dwell_angry, cue_dwell_neutral, 
                     alternative = "greater", paired=T)) %>% apa::t_apa(es_ci=T)
eye.cue %>% summarize(cue_dwell_angry = mean(cue_dwell_angry),
                      cue_dwell_neutral = mean(cue_dwell_neutral))

#intercorrelation between Latency and Dwell
with(eye.cue, cor.test(cue_lat_angry, cue_dwell_angry)) %>% apa::cor_apa(r_ci=T)
with(eye.cue, cor.test(cue_lat_neutral, cue_dwell_neutral)) %>% apa::cor_apa(r_ci=T)
#TODO ANOVA with continuous predictor to check significance of interaction


# For bachelor student ----------------------------------------------------
behavior.rt %>% full_join(behavior.acc) %>% 
  pivot_wider(id_cols = subject, names_from = c(emotion, congruency), values_from = c(rt, accuracy)) %>% 
  full_join(questionnaires, .) %>% #questionnaires first (but not in code because of pivot_wider)
  full_join(eye.antic) %>% 
  full_join(eye.cue) %>% 
  arrange(subject) %>% 
  write_csv("data/jamovi.csv")
