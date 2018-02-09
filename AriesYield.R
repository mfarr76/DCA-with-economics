rm(list = ls())
forecast_years <- 30
forecast_time <- 30*12
time_segment <- 7
ratio_1_user <- 500
ratio_2_user <- 1300
ratio_3_user <- 1300
mnth_day_select <- 1
og_select <- 1

#time_units <- ifelse(mnth_day_select == 1, 12, 365)

forecast_time <- forecast_years * 12
time_segment <- as.integer(20*30.44)
GOR1T

aries_yield <- function(forecast_time, time_segment, ratio_1_user, ratio_2_user, ratio_3_user)
{

  
  ##1st segment##
  a_1 <- if(ratio_1_user == 0) {0
    }else{
      log( ratio_1_user / ratio_2_user) / time_segment} ##calc nominal decline
  t_1 <- seq_along(1 : time_segment) #length of 1st segment
  
  ##ratio calc for the month
  ratio_1_forecast <- ratio_1_user * exp( -a_1 * (t_1 - 1)) #ratio beginning of month
  ratio_2_forecast <- ratio_1_user * exp( -a_1 * t_1) #ratio at the end of the month
  
  ##ratio for the month 
  ##if true then flat yield
  r_1 <- if(ratio_1_user == ratio_2_user){ 
    ratio_1_forecast
  }else{
   (ratio_1_forecast - ratio_2_forecast) / a_1
  }
  
  ###2nd segment
  t_2 <- forecast_time - time_segment
  t_3 <- seq_along(1 : t_2)
  
  ###check to see if flat yield for 2nd seg
  ###if true then append r.1 to flat forecast
  if(ratio_2_user == ratio_3_user){
    append(r_1, (rep(ratio_2_user, t_2)))
  } else {
    
    a_2 <- log(ratio_2_user / ratio_3_user) / t_2 ###calc nominal decline
    
    ###ratio for the month
    ratio_3_forecast <- ratio_2_user * exp( -a_2 * (t_3 - 1))
    ratio_4_forecast <- ratio_2_user * exp( -a_2 * t_3)
    
    r_2 <- (ratio_3_forecast - ratio_4_forecast) / a_2
    
    ###append r.1 and r.2 for gor/yield forecast
    append(r_1,r_2)
    
    ##Spotfire for some reason doesn't pick this up...only return Yield column??
    #result <- data.frame(Time = seq_along(1:forecast.time), Yield = append(r.1,r.2))
  }

}

##must change class of Time to numeric to join with DCA table
YieldForecast <- data.frame(Time = as.numeric(seq_along(1:forecast_time)), secondary = aries_yield(forecast_time, time_segment, ratio_1_user, ratio_2_user, ratio_3_user))



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
  ###if true then append r.1 to flat forecast
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
