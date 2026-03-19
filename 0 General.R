library(tidyverse)

trials.n = 256 #number of trials that shall be analyzed (if more trials are present, last ones will be taken)
breakPositions.theory = 256/4 * 1:3 #trial numbers after which a break should occur
sample.rate = 500

fixCross = 1500 #fixation cross visible for 1500 ms
anticipationStart = 2000 #rectangles onset
anticipationTime = c(3000, 8000)
cueTime = 1500
targetTime = c(0, 1500)

itiEnd = anticipationStart + anticipationTime + cueTime + targetTime #trial duration including ITI in ms

screen.height = 1080 #height of screen in pix
screen.width  = 1920 # width of screen in pix

leftRoi = -screen.width*(1/2-1/3)
rightRoi = screen.width*(1/2-1/3)


# Paths -------------------------------------------------------------------
path = "C:/Data/AB_A1/" #@work
#path = path %>% gsub("C:/Data/", "D:/Arbeit/", .) #@home
path.data = "data/" %>% paste0(path, .)
path.eye = "data/eye/Output/" %>% paste0(path, .)
path.physio = "physio/" %>% paste0(path.data, .)
path.rpeaks = "rpeaks/" %>% paste0(path.physio, .)

files.behavior = path.data %>% list.files(pattern = "_custom.csv", full.names = T)
files.physio = path.physio %>% list.files(pattern = ".txt", full.names = T)
file.que = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "data_ab-pheno_a1.csv", full.names = T)
# file.que.r = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "\\.r", full.names = T)

path.rds = "data/" #preprocessed files

# A Priori Exclusions -----------------------------------------------------
exclusions = c()

exclusions.phys.trials = list()
exclusions.phys.trials[["10"]] =  1:3 #discard trials 1-3 of subject 10 manually
exclusions.phys.trials[["24"]] =  1   #discard trial  1   of subject 24 manually
exclusions.phys.trials[["30"]] =  65:75 #discard trials 65-75 of subject 30 manually
exclusions.phys.trials[["41"]] =  1:2 #discard trials 1-2 of subject 41 manually
exclusions.phys.trials[["43"]] =  1:3 #discard trials 1-3 of subject 43 manually
exclusions.phys.trials[["46"]] =  1:2 #discard trials 1-2 of subject 46 manually
exclusions.phys.trials[["47"]] =  129:130 #discard trials 1-2 of subject 46 manually
exclusions.phys.trials[["54"]] =  129     #discard trial  129     of subject 54 manually


# Functions ---------------------------------------------------------------
pathToCode = function(path, path.sep="/", file.ext="\\.") {
  first = path %>% gregexpr(path.sep, .) %>% lapply(max) %>% unlist() %>% {. + 1} %>% 
    pmax(1, .) #if first not found, set it to start of string
  last = path %>% gregexpr(file.ext, .) %>% lapply(max) %>% unlist() %>% {. - 1}
  last = ifelse(last < 1, sapply(path, str_length), last) #if last cannot be found, set it to end of string
  return(path %>% substring(first, last))
}

se = function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

read.phys = function(path) {
  read_delim(path, delim="\t", na="", skip=9, progress=F, show_col_types=F) %>% suppressMessages() %>% 
    rename(EDA = "CH1", ECG = "CH2", Trigger = "CH28") %>%
    filter(Trigger <= 2^8)
}

correlation_out = function(coroutput) {
  names = coroutput$data.name %>% strsplit(" and ") %>% unlist()
  cat(paste0("r(", names[1], ", ", names[2], "): ", coroutput %>% apa::cor_apa(print=F)), "\n")
}

spearmanBrown = function(r, n=2) {
  n * r / (1 + (n-1) * r)
}

F_apa = function(x) {
  cat("F(", paste(x$parameter, collapse=", "), ") = ", 
      round(x$statistic, 2), 
      ", p ",
      ifelse(x$p.value < .001, "< .001",
             paste0("= ", round(x$p.value, 2))),
      "\n", sep="")
}

ez.ci = function(ez, conf.level = .95, sph.corr=T) {
  for (effect in ez$ANOVA$Effect) {
    index.sphericity = which(ez$`Sphericity Corrections`$Effect == effect)
    GGe = ifelse(sph.corr==F || length(index.sphericity)==0, 1, ez$`Sphericity Corrections`$GGe[index.sphericity])
    
    index.effect = which(ez$ANOVA$Effect == effect)
    ez$ANOVA %>% with(apaTables::get.ci.partial.eta.squared(F[index.effect], DFn[index.effect]*GGe, DFd[index.effect]*GGe, conf.level = conf.level)) %>% 
      sapply(round, digits=2) %>% 
      paste0(collapse=", ") %>% paste0(effect, ": ", round(conf.level*100), "% CI [", ., "]\n") %>% cat()
  }
}

lmer.ci = function(lmer, conf.level = .95, twotailed=T) {
  values = lmer %>% summary() %>% .$coefficients %>% .[, c("Estimate", "df")] %>% data.frame()
  effects = values %>% rownames()
  
  for (i in seq(effects)) {
    psych::r.con(values$Estimate[i], values$df[i], p = conf.level, twotailed=twotailed) %>% 
      round(digits=2) %>% 
      paste0(collapse=", ") %>% paste0(effects[i], ": ", round(conf.level*100), "% CI [", ., "]\n") %>% cat()
  }
}

dodge.width = .6
dodge = position_dodge(width=dodge.width)

#ggplot general theme
theme_set(myGgTheme <- theme_bw() + theme(
  #aspect.ratio = 1,
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill="white", color="white"),
  legend.background = element_rect(fill="white", color="grey"),
  legend.key=element_rect(fill='white'),
  axis.text = element_text(color="black"),
  axis.ticks.x = element_line(color="black"),
  axis.line.x = element_line(color="black"),
  axis.line.y = element_line(color="black"),
  legend.text = element_text(size=14, color="black"),
  legend.title = element_text(size=14, color="black"),
  strip.text.x = element_text(size=12, color="black"),
  axis.text.x = element_text(size=16, color="black"),
  axis.text.y = element_text(size=16, color="black"),
  axis.title = element_text(size=16, color="black"))
)
