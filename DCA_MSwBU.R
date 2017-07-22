rm(list = ls())
library(dplyr)


###load for testing
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
#Average <- read.csv("average_nobu.csv", stringsAsFactors = FALSE)
#econTbl <- read.csv("econTbl.csv", stringsAsFactors = FALSE)
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")
Average.Monthly <- Average

#write.csv(Average, file = "average.csv")

b <- 1.1
di <- 82
dmin <- 8
years <- 30

t.units <- 12
ms <- 1 #multisegment forecast 1 = On 2 = Off
abRate <- 150
time.ms <- 3
di.ms <- 90
prod.time <- years * 12 #convert years to months
t.exp.har <- seq_len(prod.time) #time units for exp and har declines
t.exp.har1 <- t.exp.har - 1 #time units for exp and har declines


di <- di/100
dmin <- dmin/100
di.ms <- di.ms/100




##############table structure is demostrated below
#curve_matrix <- matrix(2:7,3,2)
#colnames(curve_matrix) <- c("Gas", "Oil")
#rownames(curve_matrix) <- c("Average", "P10", "P90")
#user inputs 
og_select <- 2
curve_select <- 0
##add the inputs together to get the column number to use in the average table
cInput <- og_select + curve_select
##############table structure

if(nrow(Average.Monthly) > 1){
  user.phase <- data.frame(
    #month = ifelse(nrow(Average.Monthly) > 1, as.numeric(Average.Monthly[[1]]), 0),  
    month = as.numeric(Average.Monthly[[1]]),
    phase = as.numeric(Average.Monthly[[cInput]]))
}else{
  user.phase <- data.frame(month = c(0), phase = c(0))
}



t.units <- 12
mnth1.rate <- first(user.phase[[2]], 1)
qi <- max(slice(user.phase[2], 1:12), na.rm = TRUE)
time.to.peak <- user.phase$month[which.max(user.phase$phase)]
a.yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
t.trans <- ceiling(( a.yr / ( -log (1 - dmin)) - 1)/( a.yr * b) * t.units) #time to reach dmin

##############build up calculations...not finished.  can't seem to get the math right
#a.bu <- log(mnth1.rate / qi) / time.to.peak #find ai during BuildUp.then q/Np at the specific time
#t.bu <- seq(1 : time.to.peak) #sequence time to peak rate
#t.bu1 <- t.bu - 1
#init.rate <- mnth1.rate / (exp(-a.bu))
#exp.bu1 <- init.rate / a.bu * (1 - exp(-a.bu * t.bu))
#exp.bu2 <- init.rate1 / a.bu * (1 - exp(-a.bu * t.bu1))
#exp.bu1 - exp.bu2
#initial.rate / (exp(-a.bu))




########################old parameters
#qi <- 100000
#b <- 1.1
#di <- 75
#dmin <- 10
#years <- 30
#Day.Month = "Months"
#t.units <- ifelse(Day.Month == "Months", 12, 365)
#prod.time <- forecast.years * t.units
#prod.time <- forecast.years * 12
#pPhase <- quote(Oil)
#time.ms <- 10 #time for multisegment forecast
#di.ms <- 10 #decline for multisgement decline
#di <- di/100
#dmin <- dmin/100
#di.ms <- di.ms/100
########################old parameters

##----------------DCA function notes------------
######forecast.bu 
#calc the buildup rate (if any). right now, the code just copies the rates to time.to.peak
#further work is need to iterate on what the qi and decline would need to be to generate the monthly volumes

######forecast.exp
#takes the peak volume and back calc the qi need to produce the peak volumes reported (qi.bu) at a given ai.ms
#time start at month 2 as not to duplicate month 1 from forecast.bu

######forecast.hyp
#if b == 0 or 1 then step 1 is to calc ai then back calc the qi needed to generate month 1 volume bc that number 
#will always be month 1 bc forecast.bu is setup to return the volumes for time.to.peak
#if you use month 1 volume as qi then your forecast will be low


DCA <- function(b, di, dmin, di.ms, years, time.ms)
{

forecast.bu <- 
  if(time.to.peak > 1)
  {
      time.to.peak1 <- time.to.peak - 1
      user.phase$phase[1:time.to.peak1]
  }
  

forecast.exp <-
  if(ms == 1)
      { #multi.segment forecast = ms
      t.ms <- seq_len(time.ms)
      t.ms2 <- t.ms - 1
      ai.ms <- -log(1 - di.ms) / t.units
      qi.bu <- qi/(1 - exp(-ai.ms*1))*ai.ms ##qi back calc from di.ms and qi from average tbl
      Exp.Np1.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms))
      Exp.Np2.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms2))
      exp.ms <- Exp.Np1.ms - Exp.Np2.ms
  
      #t <- seq(time.ms, prod.time)
      #t2 <- t - 1
      qi <- qi * exp(-ai.ms * time.ms)
      exp.ms
      }  
      #c(forecast.bu, exp.ms)

forecast.hyp <- 
  if(b == 0){

      ai.exp <- -log( 1 - di ) / t.units
      qi.exp <- (qi * ai.exp) / (log(1 + ai.exp))
      Exp.Np1 <- qi.exp / ai.exp * (1 - exp(-ai.exp * (t.exp.har)))
      Exp.Np2 <- qi.exp / ai.exp * (1 - exp(-ai.exp * (t.exp.har1)))
    
      #exp <- data.frame(time = t.exp, prod.vol = Exp.Np1 - Exp.Np2)
      Exp.Np1 - Exp.Np2
  
  }else if(b == 1){
      
      ai.har <- (di / (1 - di) / t.units)
      qi.har <- (qi * ai.har) / (log(1 + ai.har))
      Har.Np1 <- qi.har / ai.har * log(1 + ai.har * (t.exp.har))
      Har.Np2 <- qi.har / ai.har * log(1 + ai.har * (t.exp.har1))
    
      Har.Np1 - Har.Np2

  }else{
    
      ai.hyp <- (1 / (t.units * b))*((1 - di)^- b-1) #nominal deline per time units 
      a.yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
      #part1 <- qi * ai * (1 - b)
      #part2 <- 1 - 1 / (1 + ai * b)^((1 - b)/b)
      #part3 <- part1/part2
      qi.hyp <- (qi * ai.hyp * (1 - b)) / (1 - 1 / (1 + ai.hyp * b)^((1 - b)/b)) #back calc qi based on Np (month 1)
    
      ###############Hyperbolic - b value is not 0 or 1
      #determine parameters for dmin
      t.trans <- ceiling(( a.yr / ( -log (1 - dmin)) - 1)/( a.yr * b) * t.units) #time to reach dmin
      t.trans.seq <- seq(1:t.trans)
      t.trans.seq2 <- t.trans.seq - 1
    
      ###########forecast to dmin################
      Hyp.Np.todmin1 <- (qi.hyp / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b *  t.trans.seq)^((1 - b) / b))))
      Hyp.Np.todmin2 <- (qi.hyp / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b *  t.trans.seq2)^((1 - b) / b))))
      Hyp.Npdmin <- Hyp.Np.todmin1  - Hyp.Np.todmin2
    
      ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
      admin <- -log(1 - dmin)/t.units
      q.trans <- qi.hyp / ((1 + b * ai.hyp * t.trans))^(1 / b) #rate at transition month 
      Np.trans <- (qi.hyp / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b * t.trans)^((1 - b) / b)))) #cum volume at tranistion month
      t.exp.final <- seq(1:(prod.time - t.trans))
      t.exp.final1 <- 0:(prod.time - t.trans - 1)
    
      #########forecast from dmin to end of forecast
      Exp.Np1 <- Np.trans + q.trans / admin * (1 - exp(-admin * t.exp.final))
      Exp.Np2 <- Np.trans + q.trans / admin * (1 - exp(-admin * t.exp.final1))
      Exp.Np <- Exp.Np1 - Exp.Np2
    
      c(Hyp.Npdmin, Exp.Np)

  } 
      data.frame(primary = c(forecast.bu, forecast.exp, forecast.hyp))
}

if(nrow(Average.Monthly) == 1)
{
  typecurve <- data.frame(Time = c(0), primary = 0)
  
}else{
  
  typecurve <- data.frame(DCA(b, di, dmin, di.ms, years, time.ms)) %>%
    mutate(rowcount = 1,
           Time = as.numeric(cumsum(rowcount))) %>%
    filter(Time <= prod.time &
             primary > abRate)  %>%
    select(Time, primary)
}  
write.csv(typecurve, file = "typecurve.csv")


#--------------------------------------------------------------------------------
##join tc table with yield table...did this to reduce calc time

if(nrow(Average.Monthly) == 1)
{
  typecurve <- data.frame(Time = c(Sys.time()), Gas.mcf = c(0), Oil.bbl = c(0),
                          Ratio = c(0), cumGas.mmcf = c(0) ,cumOil.mbo = c(0))
  
}else{
  typecurve <- left_join(typecurve, YieldForecast, by = c("Time")) %>%
    mutate(Gas.mcf = if(og_select == 5){primary/1000 * secondary}else{primary}, #mcf
           Oil.bbl = if(og_select == 5){primary}else{primary * secondary/1000}, #bbl
           Ratio = secondary,
           cumGas.mmcf = cumsum(Gas.mcf)/1000, #mmcf
           cumOil.mbo = cumsum(Oil.bbl)/1000) %>% #mbo 
    select(Time, 
           Gas.mcf, 
           Oil.bbl, 
           Ratio, 
           cumGas.mmcf, 
           cumOil.mbo)
  
}

#----------------------------------------------------------------------
##table to summarise eur and 12mo cum

if(nrow(Average.Monthly) == 1)
{
  forecast.table <- data.frame(CumGas12MO.mmcf = c(0), CUMOil12MO.mbo = c(0), GasEUR.mmcf = c(0),
                               OilEUR.mbo = c(0), TotalEUR.mboe = c(0))
}else{
  
  forecast.table <- typecurve %>%
    mutate(CumGas12MO.mmcf = cumGas.mmcf, #mmcf
           CumOil12MO.mbo = cumOil.mbo, #mbo
           GasEUR.mmcf = sum(Gas.mcf, na.rm = TRUE)/1000, #mmcf
           OilEUR.mbo = sum(Oil.bbl, na.rm = TRUE)/1000, #mbo
           TotalEUR.mboe = GasEUR.mmcf/6 + OilEUR.mbo) %>% #mboe
    filter(Time == 12) %>%
    select(CumGas12MO.mmcf,
           CumOil12MO.mbo,
           GasEUR.mmcf, 
           OilEUR.mbo, 
           TotalEUR.mboe)
}   



