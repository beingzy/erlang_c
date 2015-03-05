# ############################################## #
# OPTIMIZATION OF THE RISK FOR ELECTORNIC SYSTEM #
# OPTIMIZING THE STAFF SCHEUDLE TO MAXIMUM THE   #
# WORKED CASE/ALERT PERCENTAGE AND REDUCE        #
# AVERAGE WAIT TIME                              #
#                                                #
# Author: Yi Zhang                               #
# Date: May/08/2014                              #
# ############################################## #
library(plyr)
library(reshape2)
library(ggplot2)
library(outliers)
library(GA)
library(scales)
 
repo          <- list()
repo$db_store <- ""
repo$output   <- ""
repo$sup      <- ""
 
# ################################### #
# FUNCTION DEFINITION ----------------
# ################################### #
# ################################## #
# Risk Function Array                #
# ################################## #
riskCurve   <- function(x, r=10, cap=20){
                # linearity assumption
                  res <- r * x
                  res <- ifelse(res < cap, res, cap)
                  return(res)
               }
zRiskFunc1  <- function(x, r = 100/1,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # THE_WHITELIST
zRiskFunc2  <- function(x, r = 100/2,  cap = ??) { riskCurve(x=x, r=r, cap=cap) } # CVV_DECLINES
zRiskFunc3  <- function(x, r = 100/3,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # AutoBlock_BadMerch
zRiskFunc4  <- function(x, r = 100/4,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # P1
zRiskFunc5  <- function(x, r = 100/5,  cap = ??)   { riskCurve(x=x, r=r, cap=cap) } # P2
zRiskFunc6  <- function(x, r = 100/6,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # P3
zRiskFunc7  <- function(x, r = 100/7,  cap = ??)     { riskCurve(x=x, r=r, cap=cap) } # P4
zRiskFunc8  <- function(x, r = 100/8,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # P5
zRiskFunc9  <- function(x, r = 100/9,  cap = ??)    { riskCurve(x=x, r=r, cap=cap) } # P6
zRiskFunc10 <- function(x, r = 100/10, cap = ??)   { riskCurve(x=x, r=r, cap=cap) } # Deposit
 
calCost <- function(x, func.index = 1){
  # ##################################### #
  # Calculate the risk based on x(time)   #
  # and func.index (channel)              #
  # ##################################### #
  func.index <- ifelse(func.index > 5, 5, func.index)
  fun.name <- paste("zRiskFunc", 1, sep="")
  if(length(x) >  0) res <- do.call(what=fun.name, args=list(x=x))
  if(length(x) == 0) res <- 0
  res <- sum(res)
  return(res)
}
 
# ################################## #
# STAFF MATRIX OPERATION             #
# ################################## #
staffInit <- function(maxNumAgent = 20) {
  # ############################## #
  # initiate the Staff df          #
  # ############################## #
  res           <- data.frame(matrix(0, ncol = 3, nrow = maxNumAgent))
  colnames(res) <- c("ava", "rest", "load")
  return(res)
}
 
staffAvaAgent <- function(staff.df, type = 'total') {
  # #################################### #
  # Return row index for availible agent #
  # #################################### #
  ava.idx  <- staff.df$ava  == 1
  rest.idx <- staff.df$rest == 0
  load.idx <- staff.df$load == 0
  index      <- c(1:nrow(staff.df))[ava.idx & rest.idx & load.idx]
  if (type == "total") res <- length(index)
  if (type == "index") res <- index
  return(res)
}
 
staffUpdate <- function(staff.df, new.agent) {
  # ######################### #
  # Update the staffed agent  #
  # ######################### #
  staff.df$ava <- c(rep(1, times = new.agent),
                    rep(0, times = nrow(staff.df) - new.agent))
  return(staff.df)
}
 
staffAssignAlerts <- function(staff.df, alerts.demand) {
  # ############################### #
  # Assign alerts to queue of staff #
  # ############################### #
  idx <- staffAvaAgent(staff.df=staff.df, type="index")
  if (length(idx) > 0)  staff.df$load[idx] <- alerts.demand
  return(staff.df)
}
 
staffWork <- function(staff.df, time.elapse = 1){
  # ############################## #
  # Resolve workload               #
  # ############################## #
  staff.df$rest <- sapply(staff.df$rest - time.elapse, function(x) ifelse(x <= 0, yes=0, no=x))
  staff.df$load <- sapply(staff.df$load - time.elapse, function(x) ifelse(x <= 0, yes=0, no=x))    
  return(staff.df)
}
 
 
staffRestAgent <- function(staff.df) {
  # ############################################ #
  # Return row index for agent will take a break #
  # ############################################ #
  ava.idx  <- staff.df$ava  == 1
  rest.idx <- staff.df$rest == 0
  load.idx <- staff.df$load == 1
  res.idx  <- c(1:nrow(staff.df))[ava.idx & rest.idx & load.idx]
  return(res.idx)
}
 
# passage of 1 min
staffRest <- function(staff.df) {
  # #################################### #
  # Calcualte how long a agent will take #
  # for a break                          #
  # #################################### #
  rest.agent.idx <- staffRestAgent(staff.df)
  if(length(rest.agent.idx) > 0){
    rand <- runif(n=length(rest.agent.idx), min=0, max = 1)
    staff.df$rest[rest.agent.idx] <- sapply(rand, function(x) ifelse(x > .772, yes=1, no=0))
  }
  return(staff.df)
}
 
# ################################## #
# QUEUE FUNCTIONS                    #
# ################################## #
queueInit <- function(size = NA, listname = NA) {
  if(is.na(size) & is.na(listname)) stop("No neccessary configuration information (size or listname) for list !\n")
  if(is.na(size))                   size = length(listname)
  if(is.na(listname))               listname <- c("THE_WHITELIST", "CVV_Declines", "AutoBlock_BadMerch", "P1", "P2", "P3", "P4", "P5", "P6", "Deposit")
 
  res        <- replicate(n=size, list())
  names(res) <- listname
  # Initiate each component of list with numeric(0)
  for(i in 1:length(res)) res[[i]] <- numeric(0)
 
  return(res)
}
 
 
queueLoadAlerts <- function(queue, alerts.vec) {
  # ####################################### #
  # Load the alerts to the right channel of #
  # queue                                   #
  # ####################################### #
  queue.chn <- names(alerts.vec)[alerts.vec > 0]
  for(i in 1:length(queue.chn)) {
    the.queue.chn  <- queue.chn[i]
    num.alerts.chn <- alerts.vec[the.queue.chn]
    queue[[the.queue.chn]] <- c(rep(0, times = num.alerts.chn), queue[[the.queue.chn]])
  }
  return(queue)
}
 
queueReleaseAlerts <- function(queue, num.relAlerts) {
  # ################################## #
  # Release specified number of alerts #
  # to agents                          #
  # ################################## #
  # need.alerts        <- num.relAlerts
  seq.nAlerts        <- c(do.call("cbind", lapply(queue, length)))
  cum.nAlerts        <- cumsum(x=seq.nAlerts)
  fully.unload.queue <- suppressWarnings(max(which(cum.nAlerts - num.relAlerts <= 0)))
  part.unload.queue  <- suppressWarnings(min(which(cum.nAlerts - num.relAlerts >  0)))
  fully.unload.queue <- ifelse(abs(fully.unload.queue) == Inf, 0, fully.unload.queue)
  part.unload.queue  <- ifelse(abs(part.unload.queue)  == Inf, 0, part.unload.queue) 
  
  if(fully.unload.queue > 0){
    for(i in 1:fully.unload.queue){
      queue[[i]] <- queue[[i]][-c(1:length(queue[[i]]))]
      if(length(queue[[i]]) > 0) if(is.na(queue[[i]])) queue[[i]] <- numeric(0)
    }
  }
 
  if(part.unload.queue  > 0 ) {
    left <- (cum.nAlerts - num.relAlerts)[part.unload.queue]
    #tot  <- length(queue[[part.unload.queue]])
    queue[[part.unload.queue]] <- queue[[part.unload.queue]][1:left]
  }
 
  res           <- list()
  res$queue     <- queue
  res$RelAlerts <- min(max(cum.nAlerts), num.relAlerts)
  return(res)
}
 
queueTotAlerts <- function(queue){
  # ###################### #
  # return total alerts    #
  # ###################### #
  res <- sum(c(do.call("cbind", lapply(queue, length))))
  return(res)
}
 
queueDemandTime <- function(num.relAlerts, oncall = 1, time.req = NA) {
  # #################################### #
  # Calculate the demand time to resolve #
  # #################################### #
  if(num.relAlerts > 0) {
    rand <- runif(n=num.relAlerts, min=0, max=1)
    if(is.na(time.req)) {
      time.req                <- list()
      time.req$prob.fraud     <- .07
      time.req$oncall_fraud   <- 5.18
      time.req$oncall_nfraud  <- 2.5
      time.req$offcall_fraud  <- 2.5
      time.req$offcall_nfraud <- 1.0
    }
 
    if(oncall == 1) {
      res  <- sapply(rand, FUN=function(x) ifelse(x <= time.req$prob.fraud , time.req$oncall_fraud,  time.req$oncall_nfraud))
    }else{
      res  <- sapply(rand, FUN=function(x) ifelse(x <= time.req$prob.fraud , time.req$offcall_fraud, time.req$offcall_nfraud))
    }
  }else{
    res <- c()
  }
  return(res)
}
 
queueWait <- function(queue, time.elapse = 1) {
  # #################################### #
  # Increase the untouch time for alerts #
  # ####################################
  seq.nAlerts <- c(do.call("cbind", lapply(queue, length)))
  nempty.chn  <- which(seq.nAlerts > 0)
  for(i in nempty.chn) queue[[i]] <- queue[[i]] + 1
  return(queue)
}
 
# queueMaxUntouch <- function(queue) {
#   # #################################### #
#   # Increase the untouch time for alerts #
#   # #################################### #
#   res <- c(do.call("cbind", lapply(queue, max)))
#   return(res)
# }
queueRiskQueue <- function(queue, type = "sum") {
  # ################################## #
  # Return the risk of given queue     #
  # ################################## #
  res <- rep(0, length(queue))
  for(i in 1:length(res)) {
    res[i] <- calCost(x=queue[[i]], func.index=i)
  }
  if(type == "sum") res <- sum(res)
  return(res)
}
 
queueClean <- function(queue) {
  # ######################################## #
  # Drop the alerts staying within queue     #
  # for 3 days                               #
  # ######################################## #
  seq.maxTime    <- c(do.call("cbind", lapply(queue, function(x) ifelse(length(x) > 0, max(x), 0) )))
  chn4dropExpire <- which(seq.maxTime >= 4320) # 3-day expiration
  if(length(chn4dropExpire)) {
    for(i in chn4dropExpire) {
      queue[[i]] <- queue[[i]][queue[[i]] < 4320]
    }
  }
  queue <- queue
}
 
# ######################################## #
# OTHER:                                   #
# ######################################## #
getDataPath <- function(filename, dir = repo$db_store) {
  res <- paste(dir, filename, sep="")
  return(res)
}
 
 
getResearchSchedule <- function(first.weekendday = NA, start.hour = NA, start.lunch = NA) {
  # ########################################################################## #
  # RETURN A PERSONAL SCHEDULE FOR A WEEK                                      #
  # BASED ON ASSUMPTION/CONSTRICTION                                           #
  # 1. 9(or 8.5) hours/day assignment                                          #
  # 2. lunch time last .5 or 1 hour/day                                        #
  # 3. lunch time starts between post-.5-hour pre-8th-work hour                #
  # 4. 2 consecutive days as weekend break                                     #
  # ########################################################################## #
  # PARAMETERS:                                                                #
  # first.weekendday in from 1 to 7                                            #
  # start.hour       in from 9 to 17 by .5                                     #
  # start.lunch      in from .5 to 7 by .5                                     #
  # length.lunch     in .5 or 1                                                #
  # ########################################################################## #
  weekdays  <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
  weekhours <- c(seq(from=8, to=17, by=.5), .5, 1)
  random    <- list()
  res       <- list()
 
  if(is.na(first.weekendday)) { random$first.weekendday  <- sample(1:7, 1)                      }
  else                        { random$first.weekendday  <- first.weekendday                    }
  if(is.na(start.hour))       { random$start.hour <- sample(seq(from=9, to=17, by=.5), size=1)  }
  else                        { random$start.hour <- start.hour                                 }                      
  if(is.na(start.lunch))      { random$start.lunch <- sample(seq(from=.5, to=7, by=.5), size=1) }
  else                        { random$start.lunch <- start.lunch                               }
  #if(is.na(length.lunch))     { random$length.lunch <- sample(c(.5, 1), size=1)                 }
  #else                        { random$length.lunch <- length.lunch                             }
  random$length.lunch <- 1
 
  random$second.weekendday <- ifelse(random$first.weekendday < 7, random$first.weekendday + 1, 1)
  random$workhours         <- c( seq(from = random$start.hour, 
                                     to   = random$start.hour + random$start.lunch - .5,      by = .5),
                                 seq(from = random$start.hour + random$start.lunch + random$length.lunch,
                                     to   = random$start.hour + 8 + random$length.lunch - .5, by = .5) )
  random$workhours <- sapply(random$workhours, FUN=function(x) ifelse(x >= 24, x - 24, x))
  res$weekend   <- c(random$first.weekendday, random$second.weekendday)
  res$workhours <- random$workhours
  
  # TIMECARD matrix
  timecard           <- data.frame(matrix(0, nrow=48, ncol=7))
  colnames(timecard) <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
  rownames(timecard) <- seq(from=0, to=23.5, by=.5)
  #timecard$Hour      <- as.integer(seq(from=0, to=23.5, by=.5))
  #timecard$Min       <- ifelse(seq(from=0, to=23.5, by=.5) - timecard$Hour == .5, 30, 0)
 
  timecard[as.numeric(rownames(timecard)) %in% res$workhours, -c(res$weekend)] <- 1
 
  res$timecard <- timecard
  return(res)
}
 
getTimeCardInventory <- function(timecard.idx, timecard.ds) {
  # ######################################### #
  # Return time card                          #
  # ######################################### #
  if ( length(timecard.idx) == 1 ) {
    the.index <- 1
    res <- getResearchSchedule(first.weekendday = timecard.ds$weekendday[the.index],
                               start.hour       = timecard.ds$start.hour[the.index],
                               start.lunch      = timecard.ds$start.lunch[the.index])$timecard
  }else{
    int.mtx <- matrix(0, nrow = 48, ncol = 7)
    for( i in 1:length(timecard.idx) ) {
      the.index <- timecard.idx[i]
      temp.mtx <- getResearchSchedule(first.weekendday = timecard.ds$weekendday[the.index],
                                      start.hour       = timecard.ds$start.hour[the.index],
                                      start.lunch      = timecard.ds$start.lunch[the.index])$timecard
      int.mtx <- int.mtx + temp.mtx
    }
    res <- int.mtx
  }
  res$Hour  <- as.integer(seq(from=0, to=23.5, by=.5))
  res$Min   <- sapply(seq(from=0, to=23.5, by=.5) - res$Hour, function(x) ifelse(x == .5, 30, 0))
  return(res)
}
 
getNumAgent <- function(Hour, Min, weekday, staffplan.day ) {
  # ################################## #
  # Get the number of deployed agents  #
  # at specified hour, min, based on   #
  # the particular day's schedule      #
  # staffplay.day (matrix: 3 cols)     #
  # Hour, Min, num_agents              #
  # ################################## #
  the.Hour  <- Hour
  the.Min   <- Min
  staffplan.day <- staffplan.day[, c("Hour", "Min", weekday)]
  time.col  <- which(colnames(staffplan.day) %in% c("Hour", "Min"))
  agent.vec <- subset(staffplan.day, Hour == the.Hour)[, -time.col]
  res       <- agent.vec[ ifelse(the.Min < 30, yes=1, no=2) ]
  return(res)
}
 
compressor <- function(alerts, staffplan.day, weekday) {
  # ########################################## #
  # Return a compressed version of environment #
  # change over a day;                         #
  # given alerts distribution for a given day  #
  # and the same day's staff plan;             #
  # RETURN a data frame:                       #
  # Hour,Min, Queue 1 - 10, NumAgent           #
  # ########################################## #
  weekday.array        <- as.data.frame(cbind(shortname = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
                                             fullname  = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                                        stringsAsFactors = F)
  the.weekday          <-  weekday.array$shortname[weekday.array$fullname == weekday]
  pulse                <-  alerts$TimeStamp[apply(alerts[, 6:15], MARGIN=1, sum) > 0]
  alerts.comp          <-  subset(alerts, TimeStamp %in% pulse)[,c(3:4, 6:15)]
  alerts.comp$NumAgent <-  apply(alerts.comp[, c("Hour", "Min")], MARGIN=1,
                                function(x, weekday, staff){
                                  getNumAgent(Hour=x[1], Min=x[2], weekday=weekday, staffplan.day=staff)
                                }, weekday = the.weekday, staff = staffplan.day)
  return(alerts.comp)
}
 
areaCal <- function(x, val) {
  # ############################# #
  # Calcualte the area:           #
  #               ___             #
  #      ____     | |             #
  # _____|  |-----| |   ______    #
  # |    |  |     | |   |    |    #
  #-*----*--*-----*-*---*----*--  #
  # ############################# #
  #if(length(x) != length(val)) warning("length(x) is not compatible with length(val)!\n")
  #interval <- c(x[2:length(x)] - x[1:(length(x)-1)], 0)
  interval <- x
  res      <- interval %*% val
  return(res)
}
 
shapeRestore <- function(db){
  # ############################### #
  # restore the compressed data     #
  # by inserting the dropped time   #
  # points                          #
  # ############################### #
  require(plyr)
  time.cols <- c("Hour", "Min")
  val.cols  <- colnames(db)[which(!(colnames(db) %in% time.cols))]
 
  hour.range <- range(db[, "Hour"])
 
  res.df    <- as.data.frame(matrix(0,
                                    nrow = (max(hour.range) - min(hour.range) + 1) * 60,
                                    ncol=2 + length(val.cols)))
  colnames(res.df)           <- c("Hour", "Min", val.cols)
  res.df[, c("Hour", "Min")] <- merge(x=seq(from=min(hour.range), to=max(hour.range), by=1),
                                      y=seq(0, 59))
  res.df <- arrange(res.df, Hour, Min)
  inited <- 0
  for(i in 1:nrow(db)) {
   
    if(i != nrow(db)) {
      # Not last row
      now.row  <- which(res.df$Hour == db$Hour[i] & res.df$Min == db$Min[i])
      next.row <- which(res.df$Hour == db$Hour[i + 1] & res.df$Min == db$Min[i+1])
     
      res.df[now.row:(next.row - 1), val.cols] <- db[i, val.cols]
    }else{
      now.row   <- which(res.df$Hour == db$Hour[i] & res.df$Min == db$Min[i])
      next.row  <- nrow(res.df)
     
      res.df[now.row:next.row, val.cols] <- db[i, val.cols]
    }
 
  }
 
  return(res.df)
}
# ###################### #
# TUNING FUNCTION        #
# ###################### #
searchCardIndex <- function(timecard.db, weekday = NA, start.time = NA, lunch.time = NA) {
  if(!is.na(weekday) & !is.na(start.time) & !is.na(lunch.time)) {
    idx <- which(timecard.db$weekendday == weekday & timecard.db$start.hour == start.time & timecard.db$start.lunch == lunch.time)
  }
  if(is.na(lunch.time)) {
    idx <- which(timecard.db$weekendday == weekday & timecard.db$start.hour == start.time)
  }
  return(idx)
}
 
 
# ################################################################# #
# <===============================================================> #
# SIMULATION STARTS HERE !!!!!!!!!!!!!!!!!!!!!! ----------------------
# <===============================================================> #
# ################################################################# #
 
# ######################################### #
# ASSEMLBE THE SYSTEM/STREAMLINE ---------------
# ######################################### #
# staffplan is an input
sys.wrapper <- function(alerts=sup.db$alerts.sample, staff.plan, full.loop = FALSE, show.plot = FALSE, prefix_png_name = "test") {
  gc()
  report    <- list()
  loop      <- list()
  tot_risk  <- 0
  worked    <- c()
  #WL_mean   <- c()
  WL_size   <- c()
  WL_wait   <- c()
 
  if(full.loop){
    loop$month <- 1:12
  } else {
    loop$month <- 6
  }
 
  loop$weekdays <- c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  for(month in loop$month){
  staff.df <- staffInit()
  queue    <- queueInit(size=10)
 
  #png(filename=paste(prefix_png_name, "_simu.png", sep=''), width = 1440, height=900)
  if(show.plot) par(mfrow = c(2, 4), mar = c(2, 2, 2, 1))
 
  for(weekday in loop$weekdays) {
    cat("CURRENT: ", weekday, " of ", month, "\n")
    alerts.db                   <- subset(alerts, Month == month & Weekdays == weekday)
    #staff.plan                 <- test$staffplan
    alerts_staff.encap          <- compressor(alerts=alerts.db, staffplan.day=staff.plan, weekday=weekday)
    interval                    <- apply(alerts_staff.encap[, 1:2], MARGIN=1, function(x) x[1] * 60 + x[2])
    interval                    <- c(interval[2:length(interval)] - interval[1:(length(interval) - 1)], 24 * 60 - max(interval))
    report[[weekday]]           <- data.frame(matrix(0, ncol = 6, nrow = nrow(alerts_staff.encap)))
    colnames(report[[weekday]]) <- c("Hour", "Min", "RelAlerts", "Utility", "Risk", "NumAgents")
    report[[weekday]][, 1:2]    <- alerts_staff.encap[, c("Hour", "Min")]
   
    for(events in 1:nrow(alerts_staff.encap) ) {
      # Load time-wise data
      the.time          <- apply(alerts_staff.encap[events, 1:2], MARGIN=1, function(x) x[1] + x[2]/60 )
      alerts.vec        <- alerts_staff.encap[events, 3:12]
      num.agent         <- alerts_staff.encap$NumAgent[events]
      # Update the environment
      staff.df          <- staffUpdate(staff.df=staff.df, new.agent=num.agent)
      queue             <- queueLoadAlerts(queue = queue, alerts.vec = alerts.vec)
     
      # Count the number of ready agents
      num_ava_agent     <- staffAvaAgent(staff.df = staff.df)
      # Release alerts of size equal to the number of available agents
      temp.queue        <- queueReleaseAlerts(queue = queue, num.relAlerts = num_ava_agent)
      queue             <- temp.queue$queue
      actual.relAlerts  <- temp.queue$RelAlerts
      # Calcualte the demanding time of released alerts
      # demandTime    <- queueDemandTime(num.relAlerts=actual.relAlerts, oncall=oncall)
      # old: demandTime    <- ifelse(the.time >= 8.5 & the.time <= 21, rep(6, rep = actual.relAlerts), rep(2.5, rep = actual.relAlerts))
      demandTime    <- ifelse(the.time >= 8.5 & the.time <= 21, rep(6, rep = actual.relAlerts), rep(3, rep = actual.relAlerts))
     
      # reporting
      report[[weekday]]$RelAlerts[events]     <- actual.relAlerts
      #report[[weekday]]$AlertsInQueue[events] <- queueTotAlerts(queue)
      report[[weekday]]$Risk[events]          <- queueRiskQueue(queue=queue, type="sum")
      report[[weekday]]$NumAgents[events]     <- alerts_staff.encap$NumAgent[events]
      report[[weekday]]$Utility[events]       <- num.agent - num_ava_agent + actual.relAlerts
      report[[weekday]]$Utility[events]       <- min(report[[weekday]]$Utility[events], report[[weekday]]$NumAgents[events])
      #report[[weekday]]$WLA_MEAN_WAIT[events] <- queue[[]]
      #report[[weekday]]$AverageDemand[events] <- mean(demandTime)
      #if(!(weekday %in% c("Saturday", "Sunday")))
      WL_size <- c(WL_size, length(queue$THE_WHITELIST))
      #if(!(weekday %in% c("Saturday", "Sunday")))
      WL_wait <- c(WL_wait, queue$THE_WHITELIST)
     
      staff.df      <- staffAssignAlerts(staff.df=staff.df, alerts.demand=demandTime) # Assign alerts to agents 
      staff.df      <- staffWork(  staff.df = staff.df, time.elapse = interval[events] ) # System update
      queue         <- queueWait(  queue = queue, time.elapse = interval[events] )
      queue         <- queueClean( queue = queue ) # drop alerts' age >= 3 days
      # staff.df    <- staffRest( staff.df = staff.df ) # If agent would take a break       
    }
   
    tot_risk    <- tot_risk + areaCal(x=interval, val=report[[weekday]]$Risk)
    loop_worked <- sum(alerts.db[, 6:15])
    worked      <- c(worked, sum(report[[1]]$RelAlerts) /  loop_worked )
   
    
    if(show.plot) {
      alerts_staff.encap$total.alerts <- apply(alerts_staff.encap[, 4:12], MARGIN=1, FUN=sum)
      # restore data
      db4plot01 <- shapeRestore(db = alerts_staff.encap[, c("Hour", "Min", "total.alerts")])
      db4plot02 <- shapeRestore(db = report[[weekday]])
     
     midday_pos  <- which(db4plot01$Hour == 12 & db4plot01$Min == min(db4plot01$Min[db4plot01$Hour == 12]))
    
     plot(db4plot01$total.alerts, ylim=c(0, 12), col = 'darkgoldenrod', new = T)
     lines(db4plot01$total.alerts, col = 'darkgoldenrod1')
     #barplot(db4plot01$total.alerts, width=.1,   col = 'darkgoldenrod1', new = F)
     polygon(x=c(0, 1:length(db4plot02$Utility), length(db4plot02$Utility)),
             y=c(0, db4plot02$Utility, 0),
             col= alpha('olivedrab3', .5))
     lines(db4plot02$RelAlerts, col = alpha('forestgreen', .5))
     lines(db4plot02$NumAgents, col = 'black', lty = 5, lwd = 3)
     abline(v=midday_pos, col = 'firebrick4')
 
     title(paste(weekday))
    }
   
    }
  }
  if(show.plot) {
    barplot(WL_size)
    title("WL ALERTS IN QUEUE")
    par(mfrow=c(1, 1))
  }
 
  res            <- list()
 
  if(full.loop){
    res$risk_reduction <- 1 - tot_risk /  (1000000 * 2550 * 12)
  }else{
    res$risk_reduction <- 1 - tot_risk /  (1000000 * 2550)
  }
 
  res$worked     <- mean(worked)
  res$WL_size    <- mean(WL_size)
  res$WL_wait    <- mean(WL_wait)
 
  cat("REPORT ######################\n")
  cat("* RISK REDUCTION: ", res$risk_reduction, "* WORKED PRECENTAGE", res$worked, "\n")
  cat("* WhiteList Resolution: \n")
  cat("* AVERAGE NUMBER OF WL in QUEUE: ", res$WL_size, "\n")
  cat("* AVERAGE WAITING TIME OF WL in QUEUE(WEEKDAYS): ", res$WL_wait, "\n")
  return(res)
}
 
objective <- function(x, measure.type = 1, full.loop = FALSE, show.plot = TRUE) {
  # cat("************** NEW SIMULATION *******************\n")
  staff.index <- round(x)
  cat("## CONFIGURATION: ", paste(staff.index, collapse=" | "), ' ##\n', sep='')
  staff.plan  <- getTimeCardInventory(timecard.idx=staff.index, timecard.ds=sup.db$timecards)
  result      <- sys.wrapper(alerts=sup.db$alerts.sample, staff.plan=staff.plan,
                             full.loop=full.loop,
                             show.plot=show.plot, prefix_png_name="test")
 
  #volume.worked    <- result$workedPercentage
  #balanced.measure <- (5 * risk.reduction + 5 * volume.worked) / 10
 
  if (measure.type == 1) res <- result$risk_reduction
  if (measure.type == 2) res <- res$worked
  # if (measure.type == 3) res <- balanced.measure
 
  file.name <- getDataPath(filename=paste("/OPTIMIZATION/OPTIMIZATION_FTE_V2_", length(staff.index), '.txt', sep = ''), dir=repo$output)
  #fileConn  <- file(file.name)
  cat(paste(staff.index, collapse=","), ",", result$risk_reduction, ",", result$worked, ",",
      result$WL_size, ",", result$WL_wait, "\n", sep='', file=file.name, append=TRUE)
  #close(fileConn)
 return(res)
}
 
# ################################## #
# LOAD SUPPORT DATA ------------------
# ################################## #
sup.db               <- list()
sup.db$alerts.sample <- read.csv(file=getDataPath("SAMPLE_DATA_ALERTS_DISTR.csv", dir=repo$sup), header=T, stringsAsFactor = F)
#sup.db$timecards     <- read.csv(file=getDataPath("SUPPORT_DB_ALL_TIMECARDS_V3.csv", dir=repo$sup), header=T, stringsAsFactor = F)
sup.db$timecards     <- read.csv(file=getDataPath("SUPPORT_DB_ALL_TIMECARDS_V2.csv", dir=repo$sup), header=T, stringsAsFactor = F)
 
ga.opt <- ga(type="real-valued",
             fitness=objective,
             min=rep(1,    times = 10),
             max=rep(nrow(sup.db$timecards), times = 10),
             pmutation=.5,
             crossover = gareal_blxCrossover,
             maxiter = 100, run = 50, seed = 20140430)
 