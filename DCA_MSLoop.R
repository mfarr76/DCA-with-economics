rm(list = ls())
library(dplyr)

##load for testing===========================================================================
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/average_RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/tbl4r_RData")
forecast_parm <- read.csv("forecast_parm.csv", stringsAsFactors = FALSE)

forecast_years <- 30
Day_Month = "Months"
t_units <- 12
prod_time <- forecast_years * t_units
abRate <- 150
ms <- 2 #multisegment forecast 1 = On 2 = Off
pPhase <- "GAS"

##load into spotfire

##create list with wellnames===============================================================
#wellnames <- unique(forecast_parm$tcName[!is.na(forecast_parm$tcName)])
forecast_parm <- na.omit(forecast_parm)
names <- unique(forecast_parm$tcName)

#num <- 3
##dca setup for no buildup with multi forecast to loop through==============================
dca_loop <- function(num)
{
  user_parms <- subset(forecast_parm, tcName == names[num]);
  qi <- user_parms$qi;
  time_ms <- ifelse(ms == 1, user_parms$ms_time, 0);
  di_ms <- user_parms$ms_Di;
  b <- user_parms$b;
  di <- user_parms$Di;
  dmin <- user_parms$Dmin;
  
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
      t_trans_seq <- seq_len(t_trans)
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
      t_exp_final2 <- t_exp_final1 - 1
      
##forecast from dmin to end of forecast=========================================================
      Exp_Np <- (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final1))) - 
        (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final2)))
      
      c(Hyp_Npdmin, Exp_Np)
      
    } 
  data_frame(Primary = c(forecast_exp, forecast_hyp)) %>%
    mutate(Name = user_parms[[1]], 
           Time = seq_len(prod_time)) %>%
    select(Name, Time, Primary) 
}
##end dca_loop==============================================================================

#system.timedca_loop(3)

##yield forecast============================================================================
#num <- 3
rForecast <- function(num)
{
  user_parms <- subset(forecast_parm, tcName == names[num]);
  time1 <- user_parms$seg1_ratio;
  r1 <- user_parms$ratio1;
  r2 <- user_parms$ratio2;
  r3 <- user_parms$ratio3;
  
  
  ##1st segment##
  a1 <- if( r1 == 0 ) { 0
  }else{
    log( r1 / r2 ) / time1 } ##calc nominal decline
  t1 <- seq_len(time1) #length of 1st segment
  
  ##ratio for the month 
  ##if true then flat yield
  r1_fc <- if( r1 == r2 ){ 
    r1
  }else{
    ( r1 * exp( -a1 * (t1 - 1)) - 
        r1 * exp( -a1 * t1))/a1
  }
  
  ###2nd segment
  t2 <- prod_time - time1
  t3 <- seq_len(t2)
  
  ###check to see if flat yield for 2nd seg
  ###if true then append r_1 to flat forecast
  if( r2 == r3 ){
    append( r1_fc, (rep(r2, t2)))
  } else {
    a2 <- log( r2 / r3 ) / t2 ###calc nominal decline
    
    ###r for the month
    r2_fc <- ( r2 * exp( -a2 * (t3 - 1)) - 
                       r2 * exp( -a2 * t3)) / a2
    
    ###append gor/yield forecast
    c( r1_fc,r2_fc )
    
  }
  
}
##end of yield=============================================================================

#system.time(rf <- rForecast(3))

##write output of dca and yield============================================================
dca <- data.frame()
system.time(
  for(i in seq_along(names)){
    
    df <- dca_loop(i)
    df$Ratio <- rForecast(i) 
    dca <- rbind(dca, df)
    
  }
)

dca <- dca %>%
  group_by(Name, Time) %>%
  mutate(Secondary = ifelse(pPhase == "GAS", Ratio * Primary / 1000, 
                            (Ratio * Primary) / 1000))



colnames(dca) <- c("Name", "Time", "Rato", ifelse(pPhase == "GAS", "GrGas_Mcf", "GrOil_Bbl"), 
                   ifelse(pPhase == "GAS", "GrOil_Bbl", "GrGas_Mcf"))

