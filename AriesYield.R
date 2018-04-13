rm(list = ls())


t1 <- 5
r1 <- 1
r2 <- 1
r3 <- 1
years <- 30




###Michael Farr SM Energy
##yield/gor forecast that was checked against aries LOG 2 segment forecast

####user inputs from property controls
mnth_day_select <- 1 #for monthly forecast
og_select
t1
r1
r2
r3
years

#t_units <- ifelse(mnth_day_select == 1, 12, 365)
t_units <- 12
prod_time <- years * t_units

ratio <- function(t1, r1, r2, r3)
{
  ##1st segment##
  a1 <- if( r1 == 0 ) { 0
  }else{
    log( r1 / r2 ) / t1 
  } ##calc nominal decline
  
  time1 <- seq_len( t1 ) #length of 1st segment
  
  ##ratio for the month 
  ##if true then flat yield
  r1_fc <- if( r1 == r2 ){ 
    rep(r1, t1)
  }else{
    ( r1 * exp( -a1 * ( time1 - 1 )) - 
        r1 * exp( -a1 * time1 ))/a1
  }
  
  
  ###2nd segment
  t2 <- prod_time - t1
  t3 <- seq_len( t2 )
  
  ###check to see if flat yield for 2nd seg
  ###if true then append r_1 to flat forecast
  if( r2 == r3 ){
    append( r1_fc, (rep( r2, t2 )))
  } else {
    a2 <- log( r2 / r3 ) / t2 ###calc nominal decline
    
    ###r for the month
    r2_fc <- ( r2 * exp( -a2 * ( t3 - 1 )) - 
                 r2 * exp( -a2 * t3 )) / a2
    
    ###append gor/yield forecast
    c( r1_fc,r2_fc )
    
  }
  
}

Yield <- data.frame(Time = as.numeric(seq_along(1:prod_time)), secondary = ratio(t1, r1, r2, r3))



TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/Yield.AT.RData',sep=''), RFormat=T )))
