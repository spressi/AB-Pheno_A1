require("ggforce") #drawing circles in ggplot for baseline plots

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
