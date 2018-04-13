rm(list = ls())
library(dplyr)

##load for testing==========================================================================
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/average.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/tbl4r.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/dcaloop.RData")
#tc_inputs <- read.csv("forecast_parm.csv", stringsAsFactors = FALSE)

#forecast_years <- 30
#Day_Month = "Months"
tUnits <- 12
#prod_time <- forecast_years * tUnits
#abRate <- 150
#ms <- 2 #multisegment forecast 1 = On 2 = Off
pPhase <- "GAS"

##load into spotfire

##create list with wellnames===============================================================
#wellnames <- unique(tc_inputs$tcName[!is.na(tc_inputs$tcName)])
tc_inputs <- na.omit(tc_inputs)
names <- unique(tc_inputs$tcName)



#num <- 16
##dca setup for no buildup with multi forecast to loop through=============================
dca_loop <- function(num)
{
  tc_inputs <- subset(tc_inputs, tcName == names[num]);
  q1 <- tc_inputs$q1 * 365 / 12;
  q2 <- tc_inputs$q2 * 365 / 12;
  bu <- tc_inputs$bu;
  time_bu <- ifelse(bu == 1, tc_inputs$time_bu, 0)
  b <- tc_inputs$b;
  di <- tc_inputs$di;
  dmin <- tc_inputs$dmin;
  abRate <- tc_inputs$abRate;
 
  ##parameters need for transition to dmin
  a_dmin <- -log(1 - dmin) / tUnits;
  a_yr <- (1 / b) * ((1 / (1 - di))^b - 1);                                  #nominal deline in years
  t_trans <- ceiling(( a_yr / ( -log (1 - dmin)) - 1)/( a_yr * b) * tUnits); #time to reach dmin
  t_trans_seq1 <- seq_len(t_trans);
  t_trans_seq2 <- t_trans_seq1 - 1;
  
  prod_time <- tc_inputs$years * tUnits;
  t_exp_1 <- seq_len(prod_time - time_bu);           #time for exp
  t_exp_2 <- t_exp_1 - 1;                            #time for exp 
  t_exp_final1 <- seq(1:(length(t_exp_1) - t_trans)) #time for dMin decline
  t_exp_final2 <- 0:(length(t_exp_1)  - t_trans - 1) #time for dMin decline
  
  #Di <- Di/100
  #dmin <- dmin/100
  
  
##new buildup=======================================
  forecast_bu <-
    if(bu == 1)
    { #multi_segment forecast = bu
      t_bu_1 <- seq_len( time_bu )
      t_bu_2 <- t_bu_1 - 1
      #calc nominal decline from q1 and q2
      ai_bu <- -( log(( q2 ) / q1) ) / time_bu
      #calc cum gas/oil
      exp_bu <- q1 / ai_bu * ( 1 - exp( - ai_bu * t_bu_1)) -
        q1 / ai_bu * ( 1 - exp( - ai_bu * t_bu_2))
      
    } 
  
  forecast_hyp <- 
    if(b == 0){#expontial decline

      ai_exp <- -log( 1 - di ) / tUnits
      q2 / ai_exp * (1 - exp(-ai_exp * (t_exp_1))) - 
        q2 / ai_exp * (1 - exp(-ai_exp * (t_exp_2)))
      
    }else if(b == 1){

      ai_har <- (di / (1 - di) / tUnits) #nominal decline
      
      ###########forecast to dMin################
      Har_Np_dMin <- q2 / ai_har * log(1 + ai_har * (t_trans_seq1)) -
        q2 / ai_har * log(1 + ai_har * (t_trans_seq2))
      
      ###########transition rate/volum################
      Har_q_trans <- (q2/( 1 + ai_har * t_trans)) #rate at transition time
      Har_Np_trans <- (q2 / ai_har * log( 1 + ai_har * t_trans)) #cum volume at transition time
      
      #########forecast from dMin to end of forecast   
      #add Np + q at dMin time to forecast_years
      Har_Exp_Np <- (Har_Np_trans +  Har_q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final1))) -
       ( Har_Np_trans + Har_q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final2)))
      
     c(Har_Np_dMin, Har_Exp_Np)
      
    }else{
      
##Hyperbolic - b value is not 0 or 1========================================================
##determine parameters for dmin
      ai_hyp <- (1 / (tUnits * b))*((1 - di)^- b-1) #nominal deline per time units 
      

      Hyp_Npdmin <- (q2 / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq1)^((1 - b) / b)))) - 
        (q2 / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b *  t_trans_seq2)^((1 - b) / b))))
      
##forecast expontintal to end of forecast years (Terminal decline portion of the curve)=====

##rate at transition month
      q_trans <- q2 / ((1 + b * ai_hyp * t_trans))^(1 / b)  
##cum volume at tranistion month
      Np_trans <- (q2 / (( 1 - b) * ai_hyp)) * (1-(1/((1 + ai_hyp * b * t_trans)^((1 - b) / b)))) 

##forecast from dmin to end of forecast=====================================================
      Exp_Np <- (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final1))) - 
        (Np_trans + q_trans / a_dmin * (1 - exp(-a_dmin * t_exp_final2)))
      
      c(Hyp_Npdmin, Exp_Np)
      
    } 
  data.frame(Primary = c(forecast_bu, forecast_hyp)) %>%
    mutate(Name = tc_inputs[[1]], 
           Time = as.numeric(seq_len(prod_time))) %>%
    select(Name, Time, Primary)
}
##end dca_loop==============================================================================
#system.time(
#  dca1 <- dca_loop(11))


##yield forecast============================================================================
#num <- 2
rForecast <- function(num)
{
  tc_inputs <- subset(tc_inputs, tcName == names[num]);
  time1 <- tc_inputs$seg1_ratio;
  r1 <- tc_inputs$ratio1;
  r2 <- tc_inputs$ratio2;
  r3 <- tc_inputs$ratio3;
  prod_time <- tc_inputs$years * tUnits;

  ##1st segment##
  
  a1 <- if( r1 == 0 ) { 0
  }else{
    log( r1 / r2 ) / time1 } ##calc nominal decline

  ##ratio for the month 
  ##if true then flat yield
  t1 <- seq_len( time1 ) #length of 1st segment
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

#system.time(yield <- rForecast(2))

##write output of dca and yield============================================================
dca <- data.frame()
  for(i in seq_along(names)){
    tbl <- dca_loop(i)
    tbl$Ratio <- rForecast(i) 
    dca <- rbind(dca, tbl)
}

dca <- dca %>%
  rowwise() %>%
  mutate(Secondary = ifelse(pPhase == "GAS", 
                            Ratio * Primary / 1000, 
                            (Ratio * Primary) / 1000), 
         Year = ifelse(Time > 60, 5, ceiling(Time / 12))) 

colnames(dca) <- c("Name", "Time", ifelse(pPhase == "GAS", "GrGas_Mcf", "GrOil_Bbl"), 
                   "Rato", ifelse(pPhase == "GAS", "GrOil_Bbl", "GrGas_Mcf"), "Year")

library(ggplot2)
ggplot(dca, aes(x = Time, y = GrGas_Mcf, group = Name, colour = Name)) + 
  geom_line() + scale_y_log10()

dca %>% 
  group_by(Name) %>%
  summarise(EURGas_MMcf = sum(GrGas_Mcf, na.rm = TRUE) / 1000, 
            EUROil_Mbo = sum(GrOil_Bbl, na.rm = TRUE) / 1000)

#write.csv(dca, file = "dca.csv")


