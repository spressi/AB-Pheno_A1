library(tidyverse)

path = "C:/Data/AB_A1/"
path.data = "data/" %>% paste0(path, .)
path.eye = "data/eye/Output/" %>% paste0(path, .)

files.behavior = path.data %>% list.files(pattern = "_custom.csv", full.names = T)
files.physio = "physio/" %>% paste0(path.data, .) %>% list.files(pattern = ".txt", full.names = T)
file.que = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "data_ab-pheno_a1.csv", full.names = T)
# file.que.r = "questionnaires/" %>% paste0(path.data, .) %>% list.files(pattern = "\\.r", full.names = T)


se <- function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

screen.height = 1080 #height of screen in pix
screen.width  = 1920 # width of screen in pix

leftRoi = -screen.width*(1/2-1/3)
rightRoi = screen.width*(1/2-1/3)

fixCross = 1500 #fixation cross visible for 1500 ms
anticipationStart = 2000
cueTime = 1500
