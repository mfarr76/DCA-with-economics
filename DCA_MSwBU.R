rm(list = ls())
library(dplyr)
##LOAD - TESTING ONLY=============================================================
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.AT.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/avg.mnth.at.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU_AT.RData")

##inputs for testing==================================================
b <- 1
di <- 60
dMin <- 8
forecast_years <- 30
ms <- 2 #multisegment forecast 1 = On 2 = Off
abRate <- 1
time_ms <- 3
di_ms <- 90
og_select <- 6
curve_select <- 2
write.csv(AVERAGE.MONTHLY, file = "prod.csv")
write.csv(DCA.Forecast, file = "dca.csv")

##Spotfire code below==============================================================

##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

##inputs from spotfire================================================================
AVERAGE.MONTHLY          #data loaded from average table 
og_select                #primary phase
curve_select             #Average,P10, P50, P90 curve selection
b                        #b factor
di                       #effective decline
dMin                     #terminal decline
forecast_year            #number of forecast year
ms                       #multisegment forecast 1 = On 2 = Off
abRate                   #abandonment rate
time_ms                  #length of ms
di_ms                    #decline of ms
t_select <- 1            #select months or days
t_units <- ifelse(t_select == 1, 12, 365/12)

##Decline parameters==================================================================
di <- di/100
dMin <- dMin/100
di_ms <- di_ms/100
t_units <- ifelse(t_select == 1, 12, 365/12)
a_yr <- ( 1 / b) * (( 1 / ( 1 - di ))^b - 1 ) #nominal deline in years


##############table structure is demostrated below
#curve_matrix <- matrix(2:9,4,2)
#colnames(curve_matrix) <- c("Gas", "Oil")
#rownames(curve_matrix) <- c("Average", "P10", "P50", "P90")
#user inputs 
#og_select <- 2
#curve_select <- 0
##add the inputs together to get the column number to use in the average table
cInput <- og_select + curve_select
##create table to calculate buildup properties====================================


if(nrow(AVERAGE.MONTHLY) == 1){
  mnth1_rate <- c(0)
  qi <- c(0)
  time_to_peak <- c(0)
}else if(autoPick == 1){
    user_phase <- AVERAGE.MONTHLY[1:6, 1:9] * 365 / 12       #convert to monthly volumes
    mnth1_rate <- (user_phase[1,cInput])                     #1st month volume
    qi <- (max(user_phase[cInput], na.rm = TRUE))            #max rate
    time_to_peak <- which.max(as.matrix(user_phase[cInput])) #time to max volume...limited by user_phase table
    
  }else{
    mnth1_rate <- (AVERAGE.MONTHLY[1,cInput]) * 365 / 12     #1st month volume
    qi <- (AVERAGE.MONTHLY[IPMonth,cInput])   * 365 / 12     #max rate
    time_to_peak <- IPMonth                                  #time to max volume...limited by user_phase table
}


##dMin calcs====================================================================
a_dMin <- -log(1 - dMin)/ t_units #nominal decline dMin
t_trans <- ceiling(( a_yr / ( -log ( 1 - dMin )) - 1 )/( a_yr * b ) * t_units ) #time to reach dMin
t.trans <- t_trans #remove underscores to conform with Spotfire standards for output variables
t_trans_seq1 <- seq(1:t_trans) #time to transistion 
t_trans_seq2 <- t_trans_seq1 - 1

##back calculate qi=============================================================
#qi_back_calc that is calculated based upon type of decline and is used as an output variable 
#for the user to see and use when checking the dca
if(ms == 1){
  ai_ms <- -log(1-di_ms)/t_units
  qi_back_calc <- qi/(1 - exp(-ai_ms*1))*ai_ms ##exponential - qi back calc from di.ms and qi from average tbl
}else if(b == 0){
  ai_exp <- -log(1-di)/t_units
  qi_back_calc <- qi/(1 - exp(-ai_exp*1))*ai_exp ##exponential - back calc qi based on Np (month 1)
}else if(b == 1){
  ai_har <- (di / (1 - di) / t_units)
  qi_back_calc <- (qi * ai_har) / (log(1 + ai_har)) ##harmonic - back calc qi based on Np (month 1)
}else{
  ai_hyp <- (1 / (t_units * b))*((1 - di)^- b-1) #nominal deline per time units 
  qi_back_calc <- (qi * ai_hyp * (1 - b)) / (1 - 1 / (1 + ai_hyp * b)^((1 - b)/b)) ##hyperbolic - #back calc qi based on Np (month 1)
}


##time calculation for different decline equations===============================

prod_time <- forecast_years * t_units #convert years to months/days
t_exp_har1 <- seq_len(prod_time) #time units for exp and har declines
t_exp_har2 <- t_exp_har1 - 1 #time units for exp and har declines
t_exp_final1 <- seq(1:(prod_time - t_trans)) #time for dMin decline
t_exp_final2 <- 0:(prod_time - t_trans - 1) #time for dMin decline

##DCA function notes=============================================================
######forecast_bu 
#calc the buildup rate (if any). right now, the code just copies the rates to time.to.peak
#further work is need to iterate on what the qi and decline would need to be to generate the monthly volumes

######forecast_exp
#takes the peak volume and back calc the qi need to produce the peak volumes reported (qi_back_calc) at a given ai_ms
#time start at month 2 as not to duplicate month 1 from forecast.bu


DCA <- function(b, di, dMin, di_ms, forecast_years, time_ms) #function to run DCA
{
  forecast_bu <- #copy monthly rates from monthly prod table
    if(time_to_peak > 1)
    {
      time_to_peak1 <- time_to_peak - 1
      AVERAGE.MONTHLY[ 1:time_to_peak1, cInput ] * 365 / 12 
    }
  
  forecast_exp <- #expontial arps
    if(ms == 1)
    { #multi.segment forecast = ms
      t_ms1 <- seq_len(time_ms)
      t_ms2 <- t_ms1 - 1
      ai_ms <- -log(1-di_ms)/t_units
      Exp_Np1_ms <- qi_back_calc / ai_ms * (1 - exp(-ai_ms * t_ms1))
      Exp_Np2_ms <- qi_back_calc / ai_ms * (1 - exp(-ai_ms * t_ms2))
      exp_ms <- Exp_Np1_ms - Exp_Np2_ms
      
      qi_back_calc <- qi_back_calc * exp(-ai_ms * time_ms) #new qi for rest of forecast
      #this will not over write the initial qi_back_calc which is used in a visualization because
      #it is in a function whereas the other in the global environment
      exp_ms
    }  
  
  forecast_hyp <- #hyperbolic arps
    if(b == 0){
      ai_exp <- -log(1-di)/t_units
      Exp_Np1 <- qi_back_calc / ai_exp * (1 - exp(-ai_exp * (t_exp_har1)))
      Exp_Np2 <- qi_back_calc / ai_exp * (1 - exp(-ai_exp * (t_exp_har2)))
      
      Exp_Np1 - Exp_Np2
      
    }else if(b == 1){
      
      ai_har <- (di / (1 - di) / t_units)
      
      ###########forecast to dMin################
      Har_Np_to_dMin1 <- qi_back_calc / ai_har * log(1 + ai_har * (t_trans_seq1))
      Har_Np_to_dMin2 <- qi_back_calc / ai_har * log(1 + ai_har * (t_trans_seq2))
      Har_Np_dMin <- Har_Np_to_dMin1 - Har_Np_to_dMin2
      
      Har_q_trans <- (qi_back_calc/( 1 + ai_har * t_trans)) #rate at transition time
      Har_Np_trans <- (qi_back_calc / ai_har * log( 1 + ai_har * t_trans)) #cum volume at transition time
      
      #########forecast from dMin to end of forecast    
      Har_Exp_Np1 <- Har_Np_trans + Har_q_trans / a_dMin * (1 - exp(-a_dMin * t_exp_final1)) #add Np + q at dMin time to forecast_years
      Har_Exp_Np2 <- Har_Np_trans + Har_q_trans / a_dMin * (1 - exp(-a_dMin * t_exp_final2)) #add Np + q at dMin time to forecast_years
      Har_Exp_Np <- Har_Exp_Np1 - Har_Exp_Np2
      
      c(Har_Np_dMin, Har_Exp_Np)
      
    }else{
      
      ###############Hyperbolic - b value is not 0 or 1
      a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
      ai_hyp <- (1 / (t_units * b))*((1 - di)^- b-1) #nominal deline per time units 
      
      ###this was moved into the global environment
      #part1 <- qi * ai * (1 - b)
      #part2 <- 1 - 1 / (1 + ai * b)^((1 - b)/b)
      #part3 <- part1/part2
      #if(ms == 2)
      #{qi <- (qi * ai_hyp * (1 - b)) / (1 - 1 / (1 + ai_hyp * b)^((1 - b)/b))} #back calc qi based on Np (month 1)
      
      ###########forecast to dMin################
      Hyp_Np_to_dMin1 <- (qi_back_calc / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq1)^((1 - b) / b))))
      Hyp_Np_to_dMin2 <- (qi_back_calc / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq2)^((1 - b) / b))))
      Hyp_Np_dMin <- Hyp_Np_to_dMin1  - Hyp_Np_to_dMin2
      
      ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
      Hyp_q_trans <- qi_back_calc / ((1 + b * ai_hyp * t_trans))^(1 / b) #rate at transition month 
      Hyp_Np_trans <- (qi_back_calc / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b * t_trans)^((1 - b) / b)))) #cum volume at tranistion month
      
      #########forecast from dMin to end of forecast
      Hyp_Exp_Np1 <- Hyp_Np_trans + Hyp_q_trans / a_dMin * (1 - exp(-a_dMin * t_exp_final1))  #add Np + q at dMin time to forecast_years
      Hyp_Exp_Np2 <- Hyp_Np_trans + Hyp_q_trans / a_dMin * (1 - exp(-a_dMin * t_exp_final2))  #add Np + q at dMin time to forecast_years
      Hyp_Exp_Np <- Hyp_Exp_Np1 - Hyp_Exp_Np2
      
      c(Hyp_Np_dMin, Hyp_Exp_Np)
      
    } 
  data.frame(primary = c(forecast_bu, forecast_exp, forecast_hyp))
}

##join tc table with yield forecast=============================================

if(nrow(AVERAGE.MONTHLY) == 1)
{
  DCA.Forecast <- data.frame(Time = c(Sys.time()), Gas_mcf = c(0), Oil_bbl = c(0),
                             Ratio = c(0), cumGas_mmcf = c(0) ,cumOil_mbo = c(0))
  
}else{
  DCA.Forecast <- DCA(b, di, dMin, di_ms, forecast_years, time_ms) %>%
    mutate(rowcount = 1,
           Time = cumsum(rowcount)) %>%
    filter(Time <= prod_time, primary > abRate) %>%
    select(Time, primary) %>%
    left_join(., YieldForecast, by = c("Time")) %>%
    mutate(Gas_mcf = if(og_select == 6){primary/1000 * secondary}else{primary}, #mcf
           Oil_bbl = if(og_select == 6){primary}else{primary * secondary/1000}, #bbl
           Gas_mcf_d = Gas_mcf / ( 365 / 12 ), #mcf/d
           Oil_bbl_d = Oil_bbl / ( 365 / 12 ), #bbl/d
           Ratio = secondary,
           cumGas_mmcf = cumsum(Gas_mcf)/1000, #mmcf
           cumOil_mbo = cumsum(Oil_bbl)/1000) %>% #mbo 
    select(Time, 
           Gas_mcf, 
           Oil_bbl,
           Gas_mcf_d,
           Oil_bbl_d,
           Ratio, 
           cumGas_mmcf, 
           cumOil_mbo) %>%
    left_join(., AVERAGE.MONTHLY %>% #join avg daily oil and gas rates from average table
                rename(Gas_avg_mcf_d = Gas_avg, 
                       Oil_avg_bbl_d = Oil_avg) %>%
                select(Time = Months, WellCount, 
                       Gas_avg_mcf_d, Oil_avg_bbl_d), by = "Time")
  
}

#12MO cum summary table=========================================================
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
           TotalEUR_mboe = GasEUR_mmcf/6 + OilEUR_mbo, #mboe
           qi_back_calc = qi_back_calc / (365 / 12), 
           time_to_peak = as.integer(time_to_peak)) %>% #conver to days - mboe
    filter(Time == 12) %>%
    select(CumGas12MO_mmcf,
           CumOil12MO_mbo,
           GasEUR_mmcf, 
           OilEUR_mbo, 
           TotalEUR_mboe,
           qi_back_calc, 
           time_to_peak)
}   

TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/DCAwBU.RData',sep=''), RFormat=T )))