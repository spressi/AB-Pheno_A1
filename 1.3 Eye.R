#source("0 General.R")
library(tidyverse)
require(ggforce) #drawing circles in ggplot for baseline plots

# Preparation:
# 1. Read .edf data into EyeLink DataViewer (multiple import without subfolders)
# 2. Create summary reports: Fixations.txt & Messages.txt

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

#ROI analysis
validFixTime.trial = .5 #percentage of valid fixation time within trial in order to analyze trial
validFixTime.subj = .5 #percentage of trials with sufficient valid fixation time in order to analyze subject


# Functions ---------------------------------------------------------------
outlier_remove <- function(x, z=3) {
  # Remove outlier iteratively
  insample = 1 - is.na(x) #insample <- rep(1,length(x)); insample[is.na(x)] <- 0
  ok <- FALSE
  while (!ok && sum(insample) > 3) {
    xminpos <- (1:length(x))[x==min(x[insample==1], na.rm=T)] #can contain NAs
    xminpos <- xminpos[xminpos %in% (1:length(insample))[insample==1]][1] #eliminates NAs (and takes only first minimum if several are present)
    xmaxpos <- (1:length(x))[x==max(x[insample==1], na.rm=T)] #can contain NAs
    xmaxpos <- xmaxpos[xmaxpos %in% (1:length(insample))[insample==1]][1] #eliminates NAs (and takes only first maximum if several are present)
    tempinsample <- insample; tempinsample[c(xminpos,xmaxpos)] <- 0
    subx <- x[tempinsample==1]
    
    if (x[xminpos] < (mean(subx) - z*sd(subx))) {
      insample[xminpos] <- 0
      out1 <- TRUE
    } else {
      out1 <- FALSE
    }
    
    if (x[xmaxpos] > (mean(subx) + z*sd(subx))) {
      insample[xmaxpos] <- 0
      out2 <- TRUE
    } else {
      out2 <- FALSE
    }
    
    if (!out1 & !out2) { ok <- TRUE }
  }
  return(insample==1)
}

#removes values until (absolute) deviation is satisfied
outlier_remove_spread = function(x, deviation, insample=NULL) {
  if (is.null(insample)) insample = T %>% rep(length(x))
  insample[is.na(x)] = F
  
  spread = x[insample] %>% range() %>% diff()
  
  while (spread > deviation) {
    m = x[insample] %>% mean()
    x[!insample] = m #set outliers to m to ignore them in next step
    out.next = {x - m} %>% abs() %>% which.max()
    
    insample[out.next] = F #crucial: use x, not x[insample] to match vector length of insample
    spread = x[insample] %>% range() %>% diff()
  }
  return(insample)
}


#TODO after outlier removal is finished, look once (?) more if "outliers" can be INCLUDED (can happen for biased distributions)
outlier_remove_abs <- function(x, deviation) {
  # Remove outlier iteratively
  insample = 1 - is.na(x) #insample <- rep(1,length(x)); insample[is.na(x)] <- 0
  ok <- FALSE
  while (!ok && sum(insample) > 2) {
    xminpos <- (1:length(x))[x==min(x[insample==1], na.rm=T)]
    xminpos <- xminpos[xminpos %in% (1:length(insample))[insample==1]][1]
    xmaxpos <- (1:length(x))[x==max(x[insample==1], na.rm=T)]
    xmaxpos <- xmaxpos[xmaxpos %in% (1:length(insample))[insample==1]][1]
    tempinsample <- insample; tempinsample[c(xminpos,xmaxpos)] <- 0
    subx <- x[tempinsample==1]
    
    if (x[xminpos] < (mean(subx) - deviation)) {
      insample[xminpos] <- 0
      out1 <- TRUE
    } else {
      out1 <- FALSE
    }
    
    if (x[xmaxpos] > (mean(subx) + deviation)) {
      insample[xmaxpos] <- 0
      out2 <- TRUE
    } else {
      out2 <- FALSE
    }
    
    if (!out1 & !out2) { ok <- TRUE }
  }
  return(insample==1)
}

pointDistance = function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

outlier_remove_point = function(x, y, z=3, insample=NULL) {
  if (length(x) != length(y)) warning("outlier_remove_point: x- and y-coordinates of unequal length.")
  if (is.null(insample)) insample = T %>% rep(min(c(length(x), length(y))))
  insample[is.na(x) | is.na(y)] = F
  
  ok = F
  while (!ok && sum(insample) > 3) {
    xmean = mean(x[insample]); ymean = mean(y[insample]) #calculate centroid
    distances = pointDistance(x, y, xmean, ymean) #have to be updated every iteration because centroid shifts
    distances[!insample] = 0 #ignore previous outliers for this iteration
    out.next = distances %>% which.max() #find most extreme point (that has not yet been removed)
    insample[out.next] = F #temporarily remove point
    
    if (distances[out.next] <= mean(distances[insample]) + z*sd(distances[insample])) {
      insample[out.next] = T
      ok = T
    }
  }
  
  return(insample)
}

outlier_remove_point_spread = function(x, y, deviation, insample=NULL) {
  if (length(x) != length(y)) warning("outlier_remove_point_spread: x- and y-coordinates of unequal length.")
  if (is.null(insample)) insample = T %>% rep(min(c(length(x), length(y))))
  insample[is.na(x) | is.na(y)] = F
  
  ok = F
  while (!ok && sum(insample) > 2) {
    xmean = mean(x[insample]); ymean = mean(y[insample]) #calculate centroid
    distances = pointDistance(x, y, xmean, ymean) #have to be updated every iteration because centroid shifts
    distances[!insample] = 0 #ignore previous outliers for this iteration
    out.next = distances %>% which.max() #find most extreme point (that has not yet been removed)
    insample[out.next] = F #temporarily remove point
    
    if (distances[out.next] <= deviation/2) { #deviation = diameter but with distances only radius can be checked => deviation/2
      insample[out.next] = T
      ok = T
    }
  }
  
  return(insample)
}

pointInBounds = function(point.x, point.y, x.bounds, y.bounds) {
  if (is.na(point.x) || is.na(point.y) || length(point.x)==0 || length(point.y)==0) return(FALSE)
  return(point.x >= min(x.bounds) & point.x <= max(x.bounds) & 
           point.y >= min(y.bounds) & point.y <= max(y.bounds))
}

degToCm = function(angle, distance) {
  return(distance*tan(angle*pi/180))
}

degToPix = function(angle, distance, resolution, screenSize) {
  return(degToCm(angle, distance) * resolution / screenSize)
}

cmToPix = function(cm, resolution, screenSize) {
  return(cm * resolution / screenSize)
}

cmToDeg = function(cm, distance) {
  return(tan(cm/distance)*180/pi)
}

# Calculate distance to monitor for deflected fixations
distfix <- function(x,y,distance) {
  distcalc <- sqrt(x^2+y^2+distance^2)
  return(distcalc)
}

# Calculate angle between 2 vectors (to compute scanpath)
angle <- function(a,b){
  dot.prod <- a%*%b
  norm.a <- norm(a,type="2")
  norm.b <- norm(b,type="2")
  theta <- acos(dot.prod / (norm.a * norm.b))
  theta.deg <- theta/pi*180
  as.numeric(theta.deg)
  
  #as.numeric(acos(a%*%b / (norm(a,type="2") * norm(b,type="2")))/pi*180)
}

angle.coords <- function(x1, y1, x2, y2) {
  return(angle(c(x1, y1), c(x2, y2)))
}

#creates new variables that are accessible in the global environment (see "<<-")
validateBaselines = function(fixs, mess, exclusions, 
                             maxDeviation_rel, maxSpread,
                             saveBaselinePlots=FALSE, postfix="") {
  if (saveBaselinePlots) dir.create(paste0(path.data, "BL plots/"), showWarnings=F)
  
  vpn = fixs$subject %>% unique() %>% sort() %>% as.character() #all subjects in fixations
  vpn = vpn[!(vpn %in% exclusions)] #minus a priori exclusions
  vpn.n = length(vpn)
  
  trials.n = mess %>% group_by(subject) %>% summarise(trials = max(trial) - min(trial) + 1) %>% .$trials %>% max()
  
  baselines.trial = vector("list", length(vpn.n)) #list of evaluations of baselines per trial for every subject
  baselines.summary = data.frame(subject=character(vpn.n), ntrials=numeric(vpn.n), nValid=numeric(vpn.n), 
                                 invalid=numeric(vpn.n), na=numeric(vpn.n), 
                                 sd_x=numeric(vpn.n), sd_y=numeric(vpn.n),
                                 range_x=numeric(vpn.n), range_y=numeric(vpn.n),
                                 stringsAsFactors=FALSE)
  baselines.summary[,] = NA
  
  print("Validating baselines")
  for (vpIndex in 1:vpn.n) { #TODO make inner of loop enclosed function and call it with apply?
    code = vpn[vpIndex]
    cat(code, " ... ")
    
    # Determine trial number
    fix.vp = fixs %>% filter(subject==code)
    msg.vp = mess %>% filter(subject==code)
    trials.min = min(fix.vp$trial)
    trials.max = max(fix.vp$trial)
    #trials.n = trials.max - trials.min + 1 #must be same for every subject
    
    # baseline
    baseline.vp = data.frame(x=numeric(trials.n), y=numeric(trials.n), condition=character(trials.n), stringsAsFactors=FALSE)
    baseline.vp[,] = NA
    
    # Loop over trials to determine trial-by-trial baselines
    for (trial in fix.vp$trial %>% unique()) {
      # Select trial data
      fix.vp.trial = fix.vp[fix.vp$trial==trial,] #fix.vp %>% filter(trial==trial) #doesn't work
      msg.vp.trial = msg.vp[msg.vp$trial==trial,] #msg.vp %>% filter(trial==trial)
      
      # Determine onset (in ms)
      include <- grep(expoID, msg.vp.trial$message)
      if (length(include) > 0) {
        MsgIndex = grep(expoID, msg.vp.trial$message)
        onset <- msg.vp.trial$time[MsgIndex] %>% as.numeric()
        condition = msg.vp.trial$message[MsgIndex] %>% as.character() %>% gsub(".*,\\s+", "", .)
        
        # Subtract onset from startamps
        fix.vp.trial$start  <- fix.vp.trial$start - onset
        fix.vp.trial$end <- fix.vp.trial$end - onset
        
        # Caluculate baseline as weighted average of fixations
        fix.vp.trial.bl <- fix.vp.trial[fix.vp.trial$end>min(baseline) & fix.vp.trial$start<max(baseline),]
        if (nrow(fix.vp.trial.bl) > 0) {
          # Restrict fixation data to baseline
          fix.vp.trial.bl$start[1] <- max(c(min(baseline), first(fix.vp.trial.bl$start)))
          fix.vp.trial.bl$end[nrow(fix.vp.trial.bl)] <- min(c(max(baseline), last(fix.vp.trial.bl$end))) #ifelse(tail(fix.vp.trial.bl$end,1)>blen,blen,tail(fix.vp.trial.bl$end,1)) # = min(blen, tail...) ?
          fix.vp.trial.bl$dur <- fix.vp.trial.bl$end - fix.vp.trial.bl$start
          
          # Calculate baseline coordinates
          xbl <- sum(fix.vp.trial.bl$x*fix.vp.trial.bl$dur)/sum(fix.vp.trial.bl$dur)
          ybl <- sum(fix.vp.trial.bl$y*fix.vp.trial.bl$dur)/sum(fix.vp.trial.bl$dur)
          
          # Store values
          baseline.vp[trial-trials.min+1,] = c(xbl,ybl, condition)
        } else {
          # When no valid fixations are available store NA as baseline for current trial
          baseline.vp[trial-trials.min+1, "condition"] = condition
        }
      }
    }
    
    baseline.vp = baseline.vp %>% mutate(x = as.numeric(x), y = as.numeric(y))
    
    # Determine outlier
    if (usePointDistance) {
      baseline.vp = baseline.vp %>% mutate(
        blok = outlier_remove_point(x, y, maxDeviation_rel) %>%
          outlier_remove_point_spread(x, y, maxSpread, insample=.))
    } else {
      blxok = outlier_remove(baseline.vp$x, maxDeviation_rel) %>% outlier_remove_spread(baseline.vp$x, maxSpread, .)
      blyok = outlier_remove(baseline.vp$y, maxDeviation_rel) %>% outlier_remove_spread(baseline.vp$y, maxSpread, .)
      baseline.vp$blok = blxok & blyok #Baseline is valid when x and y coordinates are ok (i.e. no outlier)
    }
    
    # Store number of valid baselines per subject
    nValid = sum(baseline.vp$blok)
    invalid = 1 - nValid / trials.n
    nas = sum(is.na(baseline.vp$x)) / trials.n
    x.coords = baseline.vp %>% filter(blok) %>% .$x; mean_x = mean(x.coords); sd_x = sd(x.coords); range_x = x.coords %>% range() %>% diff()
    y.coords = baseline.vp %>% filter(blok) %>% .$y; mean_y = mean(y.coords); sd_y = sd(y.coords); range_y = y.coords %>% range() %>% diff()
    baselines.summary[vpIndex, 2:9] = c(trials.n, nValid, invalid, nas, sd_x, sd_y, range_x, range_y); baselines.summary[vpIndex , 1] = code
    baselines.trial[[code %>% as.character()]] = baseline.vp
    
    #plot subject
    if (saveBaselinePlots==TRUE || code %in% saveBaselinePlots || as.character(code) %in% saveBaselinePlots) {
      filename = paste0(path.data, "BL plots/", if_else(postfix=="", "", paste0(postfix, "_")), code, ".png")
      
      borders.rel.x = c(mean_x - maxDeviation_rel*sd_x, mean_x + maxDeviation_rel*sd_x)
      borders.rel.y = c(mean_y - maxDeviation_rel*sd_y, mean_y + maxDeviation_rel*sd_y)
      borders.abs.x = c(x.coords %>% range() %>% mean() - maxSpread/2, 
                        x.coords %>% range() %>% mean() + maxSpread/2)
      borders.abs.y = c(y.coords %>% range() %>% mean() - maxSpread/2, 
                        y.coords %>% range() %>% mean() + maxSpread/2)
      
      if (driftPlots==T || code %in% driftPlots || as.character(code) %in% driftPlots) {
        blplot = baseline.vp %>% mutate(trial=1:n()) %>% 
          ggplot(aes(x=x, y=y, color=trial)) + 
          xlim(0, screen.width) + ylim(0, screen.height) + #restrict area to screen
          geom_rect(xmin=0, ymin=0, xmax=screen.width, ymax=screen.height, color="black", fill=NA) +
          geom_hline(yintercept=mean_y, linetype="longdash") + geom_vline(xintercept = mean_x, linetype="longdash") #centroid of valid baselines
        
        #add borders depending on whether point distance was used (circles) or not (rectangles)
        if (usePointDistance) {
          distances = pointDistance(x.coords, y.coords, mean_x, mean_y)
          r_abs = mean(distances) + maxDeviation_rel*sd(distances)
          
          blplot = blplot + #circles
            ggforce::geom_circle(aes(x0=mean_x, y0=mean_y, r=r_abs), color="red", fill=NA, inherit.aes=F) + #borders for validity (relative)
            ggforce::geom_circle(aes(x0=mean_x, y0=mean_y, r=maxSpread/2), color="orange", fill=NA, inherit.aes=F) #borders for validity (absolute)
        } else {
          borders.rel.x = c(mean_x - maxDeviation_rel*sd_x, mean_x + maxDeviation_rel*sd_x)
          borders.rel.y = c(mean_y - maxDeviation_rel*sd_y, mean_y + maxDeviation_rel*sd_y)
          borders.abs.x = c(x.coords %>% range() %>% mean() - maxSpread/2, 
                            x.coords %>% range() %>% mean() + maxSpread/2)
          borders.abs.y = c(y.coords %>% range() %>% mean() - maxSpread/2, 
                            y.coords %>% range() %>% mean() + maxSpread/2)
          
          blplot = blplot + #rectangles
            geom_rect(xmin=min(borders.rel.x), ymin=min(borders.rel.y), xmax=max(borders.rel.x), ymax=max(borders.rel.y), color="red", fill=NA) + #borders for validity (relative)
            geom_rect(xmin=min(borders.abs.x), ymin=min(borders.abs.y), xmax=max(borders.abs.x), ymax=max(borders.abs.y), color="orange", fill=NA) #borders for validity (absolute)
        }
        
        #add points
        blplot = blplot + 
          geom_point() + scale_color_continuous(low="blue", high="green") + #all points (color coded by trial)
          geom_point(data=baseline.vp %>% filter(blok==F), mapping=aes(x=x, y=y), color="red") + #invalid baselines red
          geom_point(x=screen.width/2, y=screen.height/2, shape="+", size=5, color="black") + #fixation cross
          #theme(panel.border = element_rect(color = "black", fill=NA, size=5)) +
          coord_fixed() +
          ggtitle(paste0(code, if_else(postfix=="", "", paste0("_", postfix)), " (", round(invalid, digits=2)*100, "% out, ", round(nas, digits=2)*100, "% NAs)"))
        blplot %>% ggsave(filename=filename, plot=., device="png", dpi=300, units="in", width=1920/300, height = 1080/300)
      } else {
        borders.rel.x = c(mean_x - maxDeviation_rel*sd_x, mean_x + maxDeviation_rel*sd_x)
        borders.rel.y = c(mean_y - maxDeviation_rel*sd_y, mean_y + maxDeviation_rel*sd_y)
        borders.abs.x = c(x.coords %>% range() %>% mean() - maxSpread/2, 
                          x.coords %>% range() %>% mean() + maxSpread/2)
        borders.abs.y = c(y.coords %>% range() %>% mean() - maxSpread/2, 
                          y.coords %>% range() %>% mean() + maxSpread/2)
        
        #x11() #plot in new windows (max of 63)
        png(filename, 
            width=1920, height=1080)
        plot(baseline.vp$x, baseline.vp$y, pch=16, col=ifelse(baseline.vp$blok==0, "red", "black"), xlab="x (px)", ylab="y (px)", xlim=c(0, screen.width), ylim=c(0, screen.height), asp=1)
        title(paste0(code, if_else(postfix=="", "", paste0("_", postfix)), " (", round(invalid, digits=2)*100, "% out, ", round(nas, digits=2)*100, "% NAs)"))
        abline(h=c(mean_y, screen.height), v=mean_x); abline(v=borders.rel.x, h=borders.rel.y, col="red"); points(x=mean(c(0, screen.width)), y=screen.height/2, pch=3, col="blue")
        
        dev.off()
      }
    }
    
    #invisible(readline(prompt="Baseline created! Press [enter] to continue"))
  }
  
  baselines.trial <<- baselines.trial
  
  print("Baseline validation finished")
  return(baselines.summary)
}


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


# Baseline Validation -----------------------------------------------------
baselines.summary.block = validateBaselines(eye.fixations %>% select(-subject) %>% rename(subject = subject_block), 
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

## check valid fixation time by problematic baselines
# eye.fixations.valid.trial %>% 
#   mutate(good = subject %in% exclusions.eye.baseline == F) %>%
#   #mutate(good = subject %in% c("03", "13", "28") == F) %>% 
#   ggplot(aes(x = valid, fill = good)) + 
#   facet_wrap(~block, labeller = "label_both") +
#   geom_vline(xintercept = validFixTime.trial, color = "red", linewidth = 1.5) + #linetype = "dashed", 
#   geom_histogram(color = "black") +
#   myGgTheme


# ROI Analysis ------------------------------------------------------------
eye = tibble()
for (code in baselines.summary.valid %>% pull(subject) %>% unique() %>% sort()) {
  #code = inclusions.eye.baseline %>% sample(1)
  cat(paste(code, "... "))
  
  baselines.vp = baselines.trial %>% filter(subject == code)
  mes.vp = eye.messages %>% filter(subject == code)
  fix.vp = eye.fixations %>% filter(subject == code) %>% 
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

#eye %>% write_rds("eye.rds" %>% paste0(path.rds, .))


# Valid Fixations ---------------------------------------------------------
eye.fixations.valid.trial = eye.fixations %>% 
  left_join(eye.messages %>% filter(message %>% str_detect(expoID2)), by = join_by(subject, block, subject_block, subject_full, trial)) %>% select(-message) %>% 
              rename(onset = time) %>% #expoID2 = anticipation onset
  left_join(eye.messages %>% filter(message %>% str_detect(expoID3)), by = join_by(subject, block, subject_block, subject_full, trial)) %>% select(-message) %>% 
              rename(offset = time) %>%  #expoID3 = anticipation offset
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


#exclude trials with insufficient valid fixations
eye.valid = eye.fixations.valid.trial %>% filter(valid > validFixTime.trial) %>%
  left_join(eye %>% mutate(trial = as.numeric(trial)), by=c("subject", "trial", "block")) %>% select(-valid)
#eye.valid %>% select(subject, trial, block) %>% unique() %>% count(subject)

eye.valid %>% write_rds("eye.valid.rds" %>% paste0(path.rds, .))
