rm(list = ls())
library(dplyr)




###load for testing
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
#Average <- read_csv("average_nobu_csv", stringsAsFactors = FALSE)
#econTbl <- read_csv("econTbl_csv", stringsAsFactors = FALSE)
load("C:/Users/MFARR/Documents/R_files/Spotfire_data/Yield_RData")
Average_Monthly <- Average

#write_csv(Average, file = "average_csv")

b <- 1.1
di <- 82
dmin <- 8
years <- 30

t_units <- 12
ms <- 1 #multisegment forecast 1 = On 2 = Off
abRate <- 150
time_ms <- 3
di_ms <- 90
prod_time <- years * 12 #convert years to months
t_exp_har <- seq_len(prod_time) #time units for exp and har declines
t_exp_har1 <- t_exp_har - 1 #time units for exp and har declines


di <- di/100
dmin <- dmin/100
di_ms <- di_ms/100




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

if(nrow(Average_Monthly) > 1){
  user_phase <- data_frame(
    #month = ifelse(nrow(Average_Monthly) > 1, as_numeric(Average_Monthly[[1]]), 0),  
    month = as_numeric(Average_Monthly[[1]]),
    phase = as_numeric(Average_Monthly[[cInput]]))
}else{
  user_phase <- data_frame(month = c(0), phase = c(0))
}



t_units <- 12
mnth1_rate <- first(user_phase[[2]], 1)
qi <- max(slice(user_phase[2], 1:12), na_rm = TRUE)
time_to_peak <- user_phase$month[which_max(user_phase$phase)]


a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
t_trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * t_units) #time to reach dmin

##############build up calculations___not finished_  can't seem to get the math right
#a_bu <- log(mnth1_rate / qi) / time_to_peak #find ai during BuildUp_then q/Np at the specific time
#t_bu <- seq(1 : time_to_peak) #sequence time to peak rate
#t_bu1 <- t_bu - 1
#init_rate <- mnth1_rate / (exp(-a_bu))
#exp_bu1 <- init_rate / a_bu * (1 - exp(-a_bu * t_bu))
#exp_bu2 <- init_rate1 / a_bu * (1 - exp(-a_bu * t_bu1))
#exp_bu1 - exp_bu2
#initial_rate / (exp(-a_bu))




########################old parameters
#qi <- 100000
#b <- 1_1
#di <- 75
#dmin <- 10
#years <- 30
#Day_Month = "Months"
#t_units <- ifelse(Day_Month == "Months", 12, 365)
#prod_time <- forecast_years * t_units
#prod_time <- forecast_years * 12
#pPhase <- quote(Oil)
#time_ms <- 10 #time for multisegment forecast
#di_ms <- 10 #decline for multisgement decline
#di <- di/100
#dmin <- dmin/100
#di_ms <- di_ms/100
########################old parameters

##----------------DCA function notes------------
######forecast_bu 
#calc the buildup rate (if any)_ right now, the code just copies the rates to time_to_peak
#further work is need to iterate on what the qi and decline would need to be to generate the monthly volumes

######forecast_exp
#takes the peak volume and back calc the qi need to produce the peak volumes reported (qi_bu) at a given ai_ms
#time start at month 2 as not to duplicate month 1 from forecast_bu

######forecast_hyp
#if b == 0 or 1 then step 1 is to calc ai then back calc the qi needed to generate month 1 volume bc that number 
#will always be month 1 bc forecast_bu is setup to return the volumes for time_to_peak
#if you use month 1 volume as qi then your forecast will be low


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
      { #multi_segment forecast = ms
      t_ms <- seq_len(time_ms)
      t_ms2 <- t_ms - 1
      ai_ms <- -log(1 - di_ms) / t_units
      qi_bu <- qi/(1 - exp(-ai_ms*1))*ai_ms ##qi back calc from di_ms and qi from average tbl
      Exp_Np1_ms <- qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms))
      Exp_Np2_ms <- qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms2))
      exp_ms <- Exp_Np1_ms - Exp_Np2_ms
      
  
      #t <- seq(time_ms, prod_time)
      #t2 <- t - 1
      qi <- qi_bu * exp(-ai_ms * (time_ms))
      exp_ms
      }  
      #c(forecast_bu, exp_ms)

forecast_hyp <- 
  if(b == 0){

      ai_exp <- -log( 1 - di ) / t_units
      if(ms == 2){qi <- (qi * ai_exp) / (log(1 + ai_exp))}
      
      Exp_Np1 <- qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har)))
      Exp_Np2 <- qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har1)))
    
      #exp <- data_frame(time = t_exp, prod_vol = Exp_Np1 - Exp_Np2)
      Exp_Np1 - Exp_Np2
  
  }else if(b == 1){
      
      ai_har <- (di / (1 - di) / t_units)
      
      if(ms == 2){qi <- (qi * ai_har) / (log(1 + ai_har))}
      
      Har_Np1 <- qi / ai_har * log(1 + ai_har * (t_exp_har))
      Har_Np2 <- qi / ai_har * log(1 + ai_har * (t_exp_har1))
    
      Har_Np1 - Har_Np2

  }else{
    
      ai_hyp <- (1 / (t_units * b))*((1 - di)^- b-1) #nominal deline per time units 
      a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
      #part1 <- qi * ai * (1 - b)
      #part2 <- 1 - 1 / (1 + ai * b)^((1 - b)/b)
      #part3 <- part1/part2
      if(ms == 2)
        {qi <- (qi * ai_hyp * (1 - b)) / (1 - 1 / (1 + ai_hyp * b)^((1 - b)/b))} #back calc qi based on Np (month 1)
      
      ###############Hyperbolic - b value is not 0 or 1
      #determine parameters for dmin
      t_trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * t_units) #time to reach dmin
      t_trans_seq <- seq(1:t_trans)
      t_trans_seq2 <- t_trans_seq - 1
    
      ###########forecast to dmin################
      Hyp_Np_todmin1 <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq)^((1 - b) / b))))
      Hyp_Np_todmin2 <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq2)^((1 - b) / b))))
      Hyp_Npdmin <- Hyp_Np_todmin1  - Hyp_Np_todmin2
    
      ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
      admin <- -log(1 - dmin)/t_units
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
      data_frame(primary = c(forecast_bu, forecast_exp, forecast_hyp))
}

if(nrow(Average_Monthly) == 1)
{
  typecurve <- data_frame(Time = c(0), primary = 0)
  
}else{
  
  typecurve <- data_frame(DCA(b, di, dmin, di_ms, years, time_ms)) %>%
    mutate(rowcount = 1,
           Time = as_numeric(cumsum(rowcount))) %>%
    filter(Time <= prod_time &
             primary > abRate)  %>%
    select(Time, primary)
}  
write_csv(typecurve, file = "typecurve_csv")


#--------------------------------------------------------------------------------
##join tc table with yield table___did this to reduce calc time

if(nrow(Average_Monthly) == 1)
{
  typecurve <- data_frame(Time = c(Sys_time()), Gas_mcf = c(0), Oil_bbl = c(0),
                          Ratio = c(0), cumGas_mmcf = c(0) ,cumOil_mbo = c(0))
  
}else{
  typecurve <- left_join(typecurve, YieldForecast, by = c("Time")) %>%
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

if(nrow(Average_Monthly) == 1)
{
  forecast_table <- data_frame(CumGas12MO_mmcf = c(0), CUMOil12MO_mbo = c(0), GasEUR_mmcf = c(0),
                               OilEUR_mbo = c(0), TotalEUR_mboe = c(0))
}else{
  
  forecast_table <- typecurve %>%
    mutate(CumGas12MO_mmcf = cumGas_mmcf, #mmcf
           CumOil12MO_mbo = cumOil_mbo, #mbo
           GasEUR_mmcf = sum(Gas_mcf, na_rm = TRUE)/1000, #mmcf
           OilEUR_mbo = sum(Oil_bbl, na_rm = TRUE)/1000, #mbo
           TotalEUR_mboe = GasEUR_mmcf/6 + OilEUR_mbo) %>% #mboe
    filter(Time == 12) %>%
    select(CumGas12MO_mmcf,
           CumOil12MO_mbo,
           GasEUR_mmcf, 
           OilEUR_mbo, 
           TotalEUR_mboe)
}   



