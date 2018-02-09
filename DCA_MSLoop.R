rm(list = ls())
library(dplyr)


###load for testing
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/average_RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/tbl4r_RData")
forecast_parm <- read.csv("forecast_parm.csv", stringsAsFactors = FALSE)



#qi <- 100000
#b <- 1_1
#Di <- _75
#dmin <- _10
forecast_years <- 30
Day_Month = "Months"
#t_units <- ifelse(Day_Month == "Months", 12, 365)
t_units <- 12
prod_time <- forecast_years * t_units
abRate <- 150
#pPhase <- quote(Oil)
ms <- 2 #multisegment forecast 1 = On 2 = Off
#time_ms <- 10 #time for multisegment forecast
#di_ms <- _1 #decline for multisgement decline

##create list with wellnames===============================================================
wellnames <- unique(forecast_parm$tcName[!is.na(forecast_parm$tcName)])
tc_num <- 1

tc_list <- list()
for(i in seq_len(length(wellnames)))
{
  tc_list[i] <- list(wellnames[i])
}

t <- seq_len(prod_time)
t2 <- t - 1

##dca setup for no buildup with multi forecast to loop through===============================
DCA <- function(tc_num)
{
  user_parms <- subset(forecast_parm, tcName == tc_list[tc_num]);
  qi <- as.numeric(user_parms[2]);
  time_ms <- as.numeric(user_parms[3]);
  di_ms <- as.numeric(user_parms[4]);
  b <- as.numeric(user_parms[5]);
  di <- as.numeric(user_parms[6]);
  dmin <- as.numeric(user_parms[7]);
  
  #Di <- Di/100
  #dmin <- dmin/100
  
  
  forecast_exp <-
    if(ms == 1)
    { #multi_segment forecast = ms
      t_ms <- seq_len(time_ms)
      t_ms2 <- t_ms - 1 
      ai_ms <- -log(1 - di_ms) / t_units      #nominal decline
      qi_bu <- qi/(1 - exp(-ai_ms))*ai_ms     #qi back calc from di_ms and qi from average tbl
      
      exp_ms <- qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms)) - 
        qi_bu / ai_ms * (1 - exp(-ai_ms * t_ms2))

      qi <- qi_bu * exp(-ai_ms * (time_ms))
      exp_ms
    }  
  
  
  forecast_hyp <- 
    if(b == 0){
      t_exp_har1 <- seq_len(prod_time)        #time units for exp and har declines
      t_exp_har2 <- t_exp_har1 - 1            #time units for exp and har declines
      
      ai_exp <- -log( 1 - di ) / t_units
      qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har1))) - 
        qi / ai_exp * (1 - exp(-ai_exp * (t_exp_har2)))
      
    }else if(b == 1){
      
      ai_har <- (di / (1 - di) / t_units) #nominal decline

      qi / ai_har * log(1 + ai_har * (t_exp_har1)) - 
        qi / ai_har * log(1 + ai_har * (t_exp_har2))
      
      
    }else{
      
##Hyperbolic - b value is not 0 or 1=========================================================
##determine parameters for dmin
      ai_hyp <- (1 / (t_units * b))*((1 - di)^- b-1) #nominal deline per time units 
      a_yr <- (1 / b) * ((1 / (1 - di))^b - 1) #nominal deline in years
      t_trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * t_units) #time to reach dmin
      t_trans_seq <- seq(1:t_trans)
      t_trans_seq2 <- t_trans_seq - 1

      Hyp_Npdmin <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq)^((1 - b) / b)))) - 
        (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq2)^((1 - b) / b))))

##forecast expontintal to end of forecast years (Terminal decline portion of the curve)=========
      a_dmin <- -log(1 - dmin)/t_units
##rate at transition month
      q_trans <- qi / ((1 + b * ai_hyp * t_trans))^(1 / b)  
##cum volume at tranistion month
      Np_trans <- (qi / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b * t_trans)^((1 - b) / b)))) 
      t_exp_final1 <- seq_len(prod_time - t_trans - time_ms);
      t_exp_final2 <- 0:(prod_time - t_trans - time_ms - 1)
      
##forecast from dmin to end of forecast=========================================================
      Exp_Np <- (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final1))) - 
        (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final2)))
      
      c(Hyp_Npdmin, Exp_Np)
      
    } 
  data_frame(primary = c(forecast_exp, forecast_hyp))
}

df <- DCA(1)

##yield forecast================================================================================

rForecast <- function(forecast_time, t1Segment, ratio1, ratio2, ratio3)
{
  ##1st segment##
  a1 <- if( ratio1 == 0 ) { 0
  }else{
    log( ratio1 / ratio2 ) / t1Segment } ##calc nominal decline
  t1 <- seq_along(1 : t1Segment) #length of 1st segment
  
  ##ratio for the month 
  ##if true then flat yield
  r1_forecast <- if( ratio1 == ratio2 ){ 
    ratio1
  }else{
    ( ratio1 * exp( -a1 * (t1 - 1)) - 
        ratio1 * exp( -a1 * t1))/a1
  }
  
  ###2nd segment
  t2 <- forecast_time - t1Segment
  t3 <- seq_along( 1 : t2 )
  
  ###check to see if flat yield for 2nd seg
  ###if true then append r_1 to flat forecast
  if( ratio2 == ratio3 ){
    append( r1_forecast, (rep(ratio2, t2)))
  } else {
    a2 <- log( ratio2 / ratio3 ) / t2 ###calc nominal decline
    
    ###ratio for the month
    r2_forecast <- ( ratio2 * exp( -a2 * (t3 - 1)) - 
                       ratio2 * exp( -a2 * t3)) / a2
    
    ###append gor/yield forecast
    c( r1_forecast,r2_forecast )
    
  }
  
}

rForecast(30, 7, 500, 1300, 1300)

##end of forecast==========================================================================
#Econ_Metrics <- data_frame(wellnames, map_df(seq_along(wellnames), CF_Metrics))


typewell <- DCA(1)  %>%
  mutate(tcName = "tw1",
         Time = seq_len(prod_time),
         Ratio = aries_yield(prod_time, time_1, yield_1, yield_2, yield_3), 
         Gas_mcf = primary * Ratio / 1000) %>%
  select(tcName, Time, Oil_bbl = primary, Gas_mcf, Ratio)
write_csv(typewell, file = "typewell_csv")


head(typewell)
