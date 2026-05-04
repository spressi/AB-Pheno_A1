#source("0 General.R")
#source("1.1 Behavior.R")
library(tidyverse)


# Read --------------------------------------------------------------------
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
         stai = questionnaires %>% select(starts_with("stai")) %>% rowMeans(),
         sias_z_sq = scale(sias)[,1]^2,
         stai_z_sq = scale(stai)[,1]^2) %>% 
  select(subject, gender, age, sias, stai, contains("z_sq"))


# Checks ------------------------------------------------------------------
behavior %>% pull(subject) %>% setdiff(questionnaires %>% pull(subject)) #check missing questionnaire data
#questionnaires %>% ggplot(aes(y = sias_z_sq, x = sias)) + geom_point() + myGgTheme #check quadratic predictor


# Descriptives ------------------------------------------------------------
questionnaires %>% count(gender)
questionnaires %>% summarize(across(.cols = c(age, sias, stai),
                                    .fns = c(m = mean, sd = sd)))
questionnaires %>% ggplot(aes(x = sias)) + geom_histogram(fill = "orange", color = "black") + myGgTheme
questionnaires %>% ggplot(aes(x = stai)) + geom_histogram(fill = "violet", color = "black") + myGgTheme


# Output ------------------------------------------------------------------
questionnaires %>% write_rds("questionnaires.rds" %>% paste0(path.rds, .))
