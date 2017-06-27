rm(list = ls())
forecast.years <- 20
#forecast.time <- 20*12
time.1 <- 10
yield.1 <- 1000
yield.2 <- 2000
yield.3 <- 5000
t.docProp <- 1


Yield <- function(forecast.time, time.1, yield.1, yield.2, yield.3)
{
  time.units <- ifelse(t.docProp == 1, "Months", "Days")
  t.units <- ifelse(time.units == "Months", 12, 365)
  forecast.time <- forecast.years * t.units
  
  ##1st segment##
  a.1 <- log( yield.1 / yield.2) / time.1 ##calc nominal decline
  t.1 <- seq_along(1 : time.1) #length of 1st segment
  
  ##ratio calc for the month
  ratio.1 <- yield.1 * exp( -a.1 * (t.1 - 1)) #ratio beginning of month
  ratio.2 <- yield.1 * exp( -a.1 * t.1) #ratio at the end of the month
  
  ##ratio for the month 
  ##if true then flat yield
  r.1 <- if(yield.1 == yield.2){ 
   ratio.1
  }else{
   (ratio.1 - ratio.2) / a.1
  }
  
  ###2nd segment
  t.2 <- forecast.time - time.1
  t.3 <- seq_along(1 : t.2)
  
  ###check to see if flat yield for 2nd seg
  ###if true then append r.1 to flat forecast
  if(yield.2 == yield.3){
    append(r.1, (rep(yield.2, t.2)))
  } else {
    
    a.2 <- log(yield.2 / yield.3) / t.2 ###calc nominal decline
    
    ###ratio for the month
    ratio.3 <- yield.2 * exp( -a.2 * (t.3 - 1))
    ratio.4 <- yield.2 * exp( -a.2 * t.3)
    
    r.2 <- (ratio.3 - ratio.4) / a.2
    
    ###append r.1 and r.2 for gor/yield forecast
    #append(r.1,r.2)
    
    result <- data.frame(Time = seq_along(1:forecast.time), Yield = append(r.1,r.2))
  }
  result
}

(GOR <- Yield(forecast.time, time.1, yield.1, yield.2, yield.3))

#add this line...