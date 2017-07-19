rm(list = ls())
library(dplyr)


###load for testing
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
#Average <- read.csv("average_nobu.csv", stringsAsFactors = FALSE)
#econTbl <- read.csv("econTbl.csv", stringsAsFactors = FALSE)
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")
Average.Monthly <- Average

#write.csv(Average, file = "average.csv")

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

user.phase <- data.frame(
  month = as.numeric(Average.Monthly[[1]]), 
  phase = as.numeric(Average.Monthly[[2]]))


mnth1.rate <- first(user.phase[[2]], 1)
qi <- max(slice(user.phase[2], 1:12), na.rm = TRUE)
time.to.peak <- user.phase$month[which.max(user.phase$phase)]


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
b <- 1.1
di <- 75
dmin <- 10
years <- 30
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

di <- di/100
dmin <- dmin/100
di.ms <- di.ms/100


t.units <- 12
ms <- 0 #multisegment forecast 1 = On 2 = Off
abRate <- 150
time.ms <- 10
di.ms <- 10
prod.time <- years * 12 #convert years to months

a.yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
t.trans <- ceiling(( a.yr / ( -log (1 - dmin)) - 1)/( a.yr * b) * t.units) #time to reach dmin


DCA <- function(b, di, dmin, di.ms, years, time.ms)
{
  

  di <- di/100
  dmin <- dmin/100
  di.ms <- di.ms/100

forecast.bu <- 
  if(time.to.peak > 1)
  {
    user.phase$phase[1:time.to.peak]
  }


forecast.exp <-
    if(ms == 1)
      { #multi.segment forecast = ms
      t.ms <- 2:time.ms
      t.ms2 <- t.ms - 1
      ai.ms <- -log(1-di.ms)/t.units
      qi.bu <- qi/(1 - exp(-ai.ms*1))*ai.ms ##qi back calc from di.ms and qi from average tbl
      Exp.Np1.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms))
      Exp.Np2.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms2))
      exp.ms <- Exp.Np1.ms - Exp.Np2.ms
  
      #t <- seq(time.ms, prod.time)
      #t2 <- t - 1
      qi <- qi * exp(-ai.ms * time.ms)
      exp.ms
      }  
#c(rate.bu, exp.ms)

forecast.hyp <- 
  if(b == 0){

  ai <- -log(1-di)/t.units
  Exp.Np1 <- qi / ai * (1 - exp(-ai * t))
  Exp.Np2 <- qi / ai * (1 - exp(-ai * t2))
    
  #exp <- data.frame(time = t.exp, prod.vol = Exp.Np1 - Exp.Np2)
  Exp.Np1 - Exp.Np2
  
  }else if(b == 1){
    
    ai <- (di / (1 - di) / t.units)
    Har.Np1 <- qi / ai * log(1 + ai * t)
    Har.Np2 <- qi / ai * log(1 + ai * t2)
    
    Har.Np1 - Har.Np2

    
  }else{
    
    ###############Hyperbolic - b value is not 0 or 1
    #determine parameters for dmin
    ai <- (1 / (t.units * b))*((1 - di)^- b-1) #nominal deline per time units 
    a.yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
    t.trans <- ceiling(( a.yr / ( -log (1 - dmin)) - 1)/( a.yr * b) * t.units) #time to reach dmin
    t.trans.seq <- seq(1:t.trans)
    t.trans.seq2 <- t.trans.seq - 1
    
    ###########forecast to dmin################
    Hyp.Np.todmin1 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t.trans.seq)^((1 - b) / b))))
    Hyp.Np.todmin2 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t.trans.seq2)^((1 - b) / b))))
    Hyp.Npdmin <- Hyp.Np.todmin1  - Hyp.Np.todmin2
    
    ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
    admin <- -log(1 - dmin)/t.units
    q.trans <- qi / ((1 + b * ai * t.trans))^(1 / b) #rate at transition month 
    Np.trans <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b * t.trans)^((1 - b) / b)))) #cum volume at tranistion month
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


typecurve <- data.frame(DCA(1.1, 75, 10, 10, 30, 10)) %>%
  mutate(rowcount = 1,
         Time = as.numeric(cumsum(rowcount))) %>%
  filter(Time <= prod.time &
           primary > abRate)  %>%
  select(Time, primary)


#--------------------------------------------------------------------------------
##join tc table with yield table...did this to reduce calc time

if(nrow(Average.Monthly) == 1)
{
  typecurve <- data.frame(Time = c(Sys.time()), Gas.mcf = c(0), Oil.bbl = c(0),
                          Ratio = c(0), cumGas.mmcf = c(0) ,cumOil.mbo = c(0))
  
}else{
  typecurve <- left_join(typecurve, YieldForecast, by = c("Time")) %>%
    mutate(Gas.mcf = if(og_select == 1){primary/1000 * Secondary}else{primary}, #mcf
           Oil.bbl = if(og_select == 1){primary}else{primary * Secondary/1000}, #bbl
           Ratio = Secondary,
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



