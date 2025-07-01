source("1 Preprocessing.R") #TODO: rather work with imports of RDS files? (data/prepro)

library(ez)
library(apa)

# RT ----------------------------------------------------------------------
behavior.rt = behavior %>% filter(correct & !outlier) %>% 
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
behavior.acc = behavior %>% 
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


# For bachelor student ----------------------------------------------------
behavior.rt %>% full_join(behavior.acc) %>% 
#TODO join eye tracking (dwell & latency?)
  write_csv("data/jamovi.csv")
