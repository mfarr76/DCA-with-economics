rm(list = ls())
library(dplyr)


###load for testing
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
econTbl <- read.csv("econTbl.csv", stringsAsFactors = FALSE)


#qi <- 100000
#b <- 1.1
#Di <- .75
#Dmin <- .10
forecast.years <- 30
Day.Month = "Months"
#t.units <- ifelse(Day.Month == "Months", 12, 365)
t.units <- 12
prod.time <- forecast.years * t.units
abRate <- 150
pPhase <- quote(Oil)
ms <- 1 #multisegment forecast 1 = On 2 = Off
#time.ms <- 10 #time for multisegment forecast
#di.ms <- .1 #decline for multisgement decline

wellnames <- unique(econTbl$tcName[!is.na(econTbl$tcName)])
tc.num <- 1

tc.list <- list()
for(i in seq_len(length(wellnames)))
{
  tc.list[i] <- list(wellnames[i])
}

t <- seq_len(prod.time)
t2 <- t - 1

##dca setup for no buildup with multi forecast to loop through
DCA <- function(tc.num)
{
  user.parms <- subset(econTbl, tcName == tc.list[tc.num])
  qi <- as.numeric(user.parms[12]);
  time.ms <- as.numeric(user.parms[13]);
  di.ms <- as.numeric(user.parms[14]);
  Di <- as.numeric(user.parms[15]);
  b <- as.numeric(user.parms[16]);
  Dmin <- as.numeric(user.parms[17]);
  
  #Di <- Di/100
  #Dmin <- Dmin/100
  
  
  forecast.exp <-
    if(ms == 1)
    { #multi.segment forecast = ms
      t.ms <- seq_len(time.ms)
      t.ms2 <- t.ms - 1
      ai.ms <- -log(1 - di.ms) / t.units
      qi.bu <- qi/(1 - exp(-ai.ms))*ai.ms ##qi back calc from di.ms and qi from average tbl
      Exp.Np1.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms))
      Exp.Np2.ms <- qi.bu / ai.ms * (1 - exp(-ai.ms * t.ms2))
      exp.ms <- Exp.Np1.ms - Exp.Np2.ms
      
      #t <- seq(time.ms, prod.time)
      #t2 <- t - 1
      qi <- qi.bu * exp(-ai.ms * (time.ms))
      exp.ms
    }  
  #c(forecast.bu, exp.ms)
  
  forecast.hyp <- 
    if(b == 0){
      
      ai.exp <- -log( 1 - di ) / t.units
      #qi.exp <- (qi * ai.exp) / (log(1 + ai.exp))
      Exp.Np1 <- qi / ai.exp * (1 - exp(-ai.exp * (t.exp.har)))
      Exp.Np2 <- qi / ai.exp * (1 - exp(-ai.exp * (t.exp.har1)))
      
      #exp <- data.frame(time = t.exp, prod.vol = Exp.Np1 - Exp.Np2)
      Exp.Np1 - Exp.Np2
      
    }else if(b == 1){
      
      ai.har <- (di / (1 - di) / t.units)
      #qi.har <- (qi * ai.har) / (log(1 + ai.har))
      Har.Np1 <- qi / ai.har * log(1 + ai.har * (t.exp.har))
      Har.Np2 <- qi / ai.har * log(1 + ai.har * (t.exp.har1))
      
      Har.Np1 - Har.Np2
      
      
    }else{
      
      ###############Hyperbolic - b value is not 0 or 1
      #determine parameters for Dmin
      ai.hyp <- (1 / (t.units * b))*((1 - Di)^- b-1) #nominal deline per time units 
      a.yr <- (1 / b) * ((1 / (1 - Di))^b - 1) #nominal deline in years
      #qi.hyp <- (qi * ai.hyp * (1 - b)) / (1 - 1 / (1 + ai.hyp * b)^((1 - b)/b)) #back calc qi based on Np (month 1)
      
      t.trans <- ceiling(( a.yr / ( -log (1 - Dmin)) - 1)/( a.yr * b) * t.units) #time to reach Dmin
      t.trans.seq <- seq(1:t.trans)
      t.trans.seq2 <- t.trans.seq - 1
      
      ###########forecast to Dmin################
      Hyp.Np.toDmin1 <- (qi / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b *  t.trans.seq)^((1 - b) / b))))
      Hyp.Np.toDmin2 <- (qi / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b *  t.trans.seq2)^((1 - b) / b))))
      Hyp.NpDmin <- Hyp.Np.toDmin1  - Hyp.Np.toDmin2
      
      ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
      aDmin <- -log(1 - Dmin)/t.units
      q.trans <- qi / ((1 + b * ai.hyp * t.trans))^(1 / b) #rate at transition month 
      Np.trans <- (qi / (( 1 - b) * ai.hyp)) * (1-(1/((1 + ai.hyp * b * t.trans)^((1 - b) / b)))) #cum volume at tranistion month
      t.exp.final <- seq_len(prod.time - t.trans - time.ms);
      t.exp.final1 <- 0:(prod.time - t.trans - time.ms - 1)
      
      #########forecast from Dmin to end of forecast
      Exp.Np1 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.exp.final))
      Exp.Np2 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.exp.final1))
      Exp.Np <- Exp.Np1 - Exp.Np2
      
      append(Hyp.NpDmin, Exp.Np)
      
    } 
  data.frame(primary = c(forecast.exp, forecast.hyp))
}

DCA(1)

#Econ_Metrics <- data.frame(wellnames, map_df(seq_along(wellnames), CF.Metrics))


typewell <- DCA(1)  %>%
  mutate(tcName = "tw1",
         Time = seq_len(prod.time),
         Ratio = aries_yield(prod.time, time.1, yield.1, yield.2, yield.3), 
         Gas.mcf = primary * Ratio / 1000) %>%
  select(tcName, Time, Oil.bbl = primary, Gas.mcf, Ratio)
write.csv(typewell, file = "typewell.csv")


head(typewell)
