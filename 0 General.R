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
path = "C:/Data/AB_A1/"
path.data = "data/" %>% paste0(path, .)
path.eye = "data/eye/Output/" %>% paste0(path, .)

files.behavior = path.data %>% list.files(pattern = "_custom.csv", full.names = T)
files.physio = "physio/" %>% paste0(path.data, .) %>% list.files(pattern = ".txt", full.names = T)
file.que = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "data_ab-pheno_a1.csv", full.names = T)
# file.que.r = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "\\.r", full.names = T)


# A Priori Exclusions -----------------------------------------------------
exclusions.phys.trials = list()
#exclusions.phys.trials[["vp01.txt"]] =  100:103 #discard trials 100-103 of subject vp01.txt manually


# Functions ---------------------------------------------------------------
se = function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

read.phys = function(path) {
  read_delim(path, delim="\t", na="", skip=9, progress=F, show_col_types=F) %>% suppressMessages() %>% 
    rename(EDA = "CH1", ECG = "CH2", Trigger = "CH28") %>%
    filter(Trigger <= 2^8)
}
