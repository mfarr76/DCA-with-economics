rm(list = ls())
library(dplyr)

###load for testing
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")
#########################LOAD - TESTING ONLY#############################################################
b <- 1.1
di <- 82
dmin <- 8
years <- 30
t_units <- 12
ms <- 1 #multisegment forecast 1 = On 2 = Off
abRate <- 150
time_ms <- 3
di_ms <- 90
og_select <- 5
curve_select <- 0
###########################################################################################################


##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

##inputs from user through property controls
og_select
curve_select
b
di
dmin
years
ms #multisegment forecast 1 = On 2 = Off
abRate #abandonment rate
time_ms #length of ms
di_ms #decline of ms



##data loaded from tables
AVERAGE.MONTHLY

##############table structure is demostrated below
#curve_matrix <- matrix(2:7,3,2)
#colnames(curve_matrix) <- c("Gas", "Oil")
#rownames(curve_matrix) <- c("Average", "P10", "P90")
#user inputs 
#og_select <- 2
#curve_select <- 0
##add the inputs together to get the column number to use in the average table
cInput <- og_select + curve_select
##############table structure

if(nrow(AVERAGE.MONTHLY) > 1){
  user_phase <- data.frame(
    month = as.numeric(AVERAGE.MONTHLY[[1]]),
    phase = as.numeric(AVERAGE.MONTHLY[[cInput]]))
}else{
  user_phase <- data.frame(month = c(0), phase = c(0))
}



di <- di/100
dmin <- dmin/100
di_ms <- di_ms/100

t_units <- 12
mnth1_rate <- first(user_phase[[2]], 1)
qi <- max(slice(user_phase[2], 1:12), na.rm = TRUE)
time_to_peak <- user_phase$month[which.max(user_phase$phase)]
a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
t.trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * t_units) #time to reach dmin


#ms <- 0 #multisegment forecast 1 = On 2 = Off
#abRate <- 150
#time.ms <- 10
#di.ms <- 10
prod_time <- years * 12 #convert years to months
t_exp_har <- seq_len(prod_time) #time units for exp and har declines
t_exp_har1 <- t_exp_har - 1 #time units for exp and har declines


##----------------DCA function notes------------
######forecast.bu 
#calc the buildup rate (if any). right now, the code just copies the rates to time.to.peak
#further work is need to iterate on what the qi and decline would need to be to generate the monthly volumes

######forecast.exp
#takes the peak volume and back calc the qi need to produce the peak volumes reported (qi.bu) at a given ai.ms
#time start at month 2 as not to duplicate month 1 from forecast.bu


DCA <- function(b, di, dmin, di_ms, years, time_ms)
{
  
  forecast_bu <- 
    if(time_to_peak > 1)
    {
      time_to_peak1 <- time_to_peak - 1
      user_phase$phase[1:time_to_peak1]
    }
  
  forecast_exp <-
    if(ms == 1)
    { #multi.segment forecast = ms
      t_ms <- seq_len(time_ms)
      t_ms2 <- t_ms - 1
      ai_ms <- -log(1-di_ms)/t_units
      qi_bu <- qi/(1 - exp(-ai_ms*1))*ai_ms ##qi back calc from di.ms and qi from average tbl
      Exp_Np1_ms <- qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms))
      Exp_Np2_ms <- qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms2))
      exp_ms <- Exp_Np1_ms - Exp_Np2_ms
      
      #t <- seq(time.ms, prod.time)
      #t2 <- t - 1
      qi <- qi_bu * exp(-ai_ms * time_ms)
      exp_ms
    }  
  #c(rate.bu, exp.ms)
  
  forecast_hyp <- 
    if(b == 0){
      
      ai_exp <- -log( 1 - di ) / t_units
      if(ms == 2){qi <- (qi * ai_exp) / (log(1 + ai_exp))}
      
      Exp_Np1 <- qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har)))
      Exp_Np2 <- qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har1)))
      
      #exp <- data.frame(time = t.exp, prod.vol = Exp.Np1 - Exp.Np2)
      Exp_Np1 - Exp_Np2
      
    }else if(b == 1){
      
      ai_har <- (di / (1 - di) / t_units)
      
      if(ms == 2){qi <- (qi * ai_har) / (log(1 + ai_har))}
      
      Har_Np1 <- qi / ai_har * log(1 + ai_har * (t_exp_har))
      Har_Np2 <- qi / ai_har * log(1 + ai_har * (t_exp_har1))
      
      Har_Np1 - Har_Np2
      
      
      
    }else{
      
      ###############Hyperbolic - b value is not 0 or 1
      ai_hyp <- (1 / (t_units * b))*((1 - di)^- b-1) #nominal deline per time units 
      a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
      #part1 <- qi * ai * (1 - b)
      #part2 <- 1 - 1 / (1 + ai * b)^((1 - b)/b)
      #part3 <- part1/part2
      if(ms == 2)
      {qi <- (qi * ai_hyp * (1 - b)) / (1 - 1 / (1 + ai_hyp * b)^((1 - b)/b))} #back calc qi based on Np (month 1)
      
      #determine parameters for dmin
      t_trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * t_units) #time to reach dmin
      t_trans_seq <- seq(1:t_trans)
      t_trans_seq2 <- t_trans_seq - 1
      
      ###########forecast to dmin################
      Hyp_Np_to_dmin1 <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq)^((1 - b) / b))))
      Hyp_Np_to_dmin2 <- (qi/ (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq2)^((1 - b) / b))))
      Hyp_Npdmin <- Hyp_Np_to_dmin1  - Hyp_Np_to_dmin2
      
      ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
      admin <- -log(1 - dmin)/ t_units
      q_trans <- qi / ((1 + b * ai_hyp * t_trans))^(1 / b) #rate at transition month 
      Np_trans <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b * t_trans)^((1 - b) / b)))) #cum volume at tranistion month
      t_exp_final <- seq(1:(prod_time - t_trans))
      t_exp_final1 <- 0:(prod_time - t_trans - 1)
      
      #########forecast from dmin to end of forecast
      Exp_Np1 <- Np_trans + q_trans / admin * (1 - exp(-admin * t_exp_final))
      Exp_Np2 <- Np_trans + q_trans / admin * (1 - exp(-admin * t_exp_final1))
      Exp_Np <- Exp_Np1 - Exp_Np2
      
      c(Hyp_Npdmin, Exp_Np)
      
    } 
  data.frame(primary = c(forecast_bu, forecast_exp, forecast_hyp))
}

if(nrow(AVERAGE.MONTHLY) == 1)
{
  DCA.Forecast <- data.frame(Time = c(0), primary = 0)
  
}else{
  
  DCA.Forecast <- data.frame(DCA(b, di, dmin, di_ms, years, time_ms)) %>%
    mutate(rowcount = 1,
           Time = as.numeric(cumsum(rowcount))) %>%
    filter(Time <= prod_time &
             primary > abRate)  %>%
    select(Time, primary)
}

#--------------------------------------------------------------------------------
##join tc table with yield table...did this to reduce calc time


if(nrow(AVERAGE.MONTHLY) == 1)
{
  DCA.Forecast <- data.frame(Time = c(Sys.time()), Gas_mcf = c(0), Oil_bbl = c(0),
                          Ratio = c(0), cumGas_mmcf = c(0) ,cumOil_mbo = c(0))
  
}else{
  DCA.Forecast <- left_join(DCA.Forecast, YieldForecast, by = c("Time")) %>%
    mutate(Gas_mcf = if(og_select == 5){primary/1000 * secondary}else{primary}, #mcf
           Oil_bbl = if(og_select == 5){primary}else{primary * secondary/1000}, #bbl
           Ratio = secondary,
           cumGas_mmcf = cumsum(Gas_mcf)/1000, #mmcf
           cumOil_mbo = cumsum(Oil_bbl)/1000) %>% #mbo 
    select(Time, 
           Gas_mcf, 
           Oil_bbl, 
           Ratio, 
           cumGas_mmcf, 
           cumOil_mbo)
  
}

#----------------------------------------------------------------------
##table to summarise eur and 12mo cum

if(nrow(AVERAGE.MONTHLY) == 1)
{
  Forecast.Table <- data.frame(CumGas12MO_mmcf = c(0), CUMOil12MO_mbo = c(0), GasEUR_mmcf = c(0),
                               OilEUR_mbo = c(0), TotalEUR_mboe = c(0))
}else{
  
  Forecast.Table <- DCA.Forecast %>%
    mutate(CumGas12MO_mmcf = cumGas_mmcf, #mmcf
           CumOil12MO_mbo = cumOil_mbo, #mbo
           GasEUR_mmcf = sum(Gas_mcf, na.rm = TRUE)/1000, #mmcf
           OilEUR_mbo = sum(Oil_bbl, na.rm = TRUE)/1000, #mbo
           TotalEUR_mboe = GasEUR_mmcf/6 + OilEUR_mbo) %>% #mboe
    filter(Time == 12) %>%
    select(CumGas12MO_mmcf,
           CumOil12MO_mbo,
           GasEUR_mmcf, 
           OilEUR_mbo, 
           TotalEUR_mboe)
}   

TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/DCAwBU.RData',sep=''), RFormat=T )))