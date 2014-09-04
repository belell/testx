#setwd("~/Courses/R programming/Week 4")

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  #outcome <- "heart attack"
  if (num == "best") num <- 1
    
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(dat[, 11] <- as.numeric(dat[, 11]))
  suppressWarnings(dat[, 17] <- as.numeric(dat[, 17]))
  suppressWarnings(dat[, 23] <- as.numeric(dat[, 23]))
  
  ## Check that state and outcome are valid
  #if (!(state %in% dat$State)) stop("invalid state")
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  if (outcome == "heart attack") {col <- 11} else if (outcome == "heart failure") {col <- 17} else {col <- 23}
  ## For each state, find the hospital of the given rank
  dat2 <- dat[,c(2, 7, col)]
  names(dat2) <- c("hospital", "state", "rate")
  if (num == "worst") {
    dat3 <- dat2[order(dat2$state, -dat2$rate, dat2$hospital),]
    num <- 1
  }
  else dat3 <- dat2[order(dat2$state, dat2$rate, dat2$hospital),]
  head(dat3)
  
  dat3 <- dat3[!is.na(dat3$rate),]
  dat62 <- split(dat3$hospital, dat3$state)
  dat72 <- sapply(dat62, function(x) {x[num]})
  as.data.frame(cbind(hospital = dat72, state = names(dat72)))
}