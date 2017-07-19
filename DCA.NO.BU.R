rm(list = ls())
library(dplyr)

#TypeWell <- read.csv("1Well.csv", header = TRUE)
#TypeWell$Oil <- as.numeric(TypeWell$Oil)
#TypeWell$Gas <- as.numeric(TypeWell$Gas)

###load for testing
econTbl <- read.csv("econTbl.csv", stringsAsFactors = FALSE)

#qi <- 100000
#b <- 1.1
#Di <- .75
#Dmin <- .10
forecast.years <- 30
Day.Month = "Months"
t.units <- ifelse(Day.Month == "Months", 12, 365)
prod.time <- forecast.years * t.units
abRate <- 150
pPhase <- quote(Oil)
ms <- 0 #multisegment forecast 1 = On 2 = Off
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

Time1 <- 15
Yield1 <- 100
Yield2 <- 50
Yield3 <- 35

#class(b)
#qi <- 5000 *30.4
#Di <- 0.7
#class(qi)

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
      ai.ms <- -log(1-di.ms)/t.units
      Exp.Np1.ms <- qi / ai.ms * (1 - exp(-ai.ms * t.ms))
      Exp.Np2.ms <- qi / ai.ms * (1 - exp(-ai.ms * t.ms2))
      exp.ms <- Exp.Np1.ms - Exp.Np2.ms
  
      #t <- seq(time.ms, prod.time)
      #t2 <- t - 1
      qi <- qi * exp(-ai.ms * time.ms)
      exp.ms
      }  


forecast.hyp <- 
  if(b == 0){

  ai <- -log(1-Di)/t.units
  Exp.Np1 <- qi / ai * (1 - exp(-ai * t))
  Exp.Np2 <- qi / ai * (1 - exp(-ai * t2))
    
  #exp <- data.frame(time = t.exp, prod.vol = Exp.Np1 - Exp.Np2)
  Exp.Np1 - Exp.Np2
  
  }else if(b == 1){
    
    ai <- (Di / (1 - Di) / t.units)
    Har.Np1 <- qi / ai * log(1 + ai * t)
    Har.Np2 <- qi / ai * log(1 + ai * t2)
    
    Har.Np1 - Har.Np2

    
  }else{
    
    ###############Hyperbolic - b value is not 0 or 1
    #determine parameters for Dmin
    ai <- (1 / (t.units * b))*((1 - Di)^- b-1) #nominal deline per time units 
    a.yr <- (1 / b) * ((1 / (1 - Di))^b - 1) #nominal deline in years
    t.trans <- ceiling(( a.yr / ( -log (1 - Dmin)) - 1)/( a.yr * b) * t.units) #time to reach Dmin
    t.trans.seq <- seq(1:t.trans)
    t.trans.seq2 <- t.trans.seq - 1
    
    ###########forecast to Dmin################
    Hyp.Np.toDmin1 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t.trans.seq)^((1 - b) / b))))
    Hyp.Np.toDmin2 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t.trans.seq2)^((1 - b) / b))))
    Hyp.NpDmin <- Hyp.Np.toDmin1  - Hyp.Np.toDmin2
    
    ##########forecast expontintal to end of forecast years (Terminal decline portion of the curve)#########
    aDmin <- -log(1 - Dmin)/t.units
    q.trans <- qi / ((1 + b * ai * t.trans))^(1 / b) #rate at transition month 
    Np.trans <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b * t.trans)^((1 - b) / b)))) #cum volume at tranistion month
    t.exp.final <- seq(1:(prod.time - t.trans))
    t.exp.final1 <- 0:(prod.time - t.trans - 1)
    
    #########forecast from Dmin to end of forecast
    Exp.Np1 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.exp.final))
    Exp.Np2 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.exp.final1))
    Exp.Np <- Exp.Np1 - Exp.Np2
    
    append(Hyp.NpDmin, Exp.Np)

  } 
  append(forecast.exp, forecast.hyp)
    
}




##############
xy <- DCA(1)

##########left off here...need to add in buildup...

plot(DCA(1))

dca.tbl <- map(seq_along(wellnames), DCA)
  
  data.frame(wellnames, map_df(seq_along(wellnames), CF.Metrics))

AriesYield <- function(Time1, Yield1, Yield2, Yield3)
{
  
  #calc nominal decline
  a1 <- log( Yield1 / Yield2) / Time1
  T1 <- seq(1 : Time1)
  
  ###1st segment
  #ratio for end of the month
  ratio1 <- Yield1 * exp( -a1 * T1)
  ratio2 <- Yield1 * exp( -a1 * (T1 - 1))
  
  #ratio used for the month 
  r1 <- if(Yield1 == Yield2)
  {
    ratio1
    
  }else{
    (ratio2 - ratio1) / a1
  }
    
  ###2nd segment
  T2 <- time - Time1
  T3 <- seq(1 : T2)
  
  ##check to see if flat yield
  if(Yield2 == Yield3)
  {
    #append the segments
    append(r1, (rep(Yield2, T2)))
    
  } else {
    #calc nominal decline
    a2 <- log(Yield2 / Yield3) / T2
    #ratio for end of the month
    ratio3 <- Yield2 * exp( -a2 * T3)
    ratio4 <- Yield2 * exp( -a2 * (T3 - 1))
    #append the segments
    r2 <- (ratio4 - ratio3) / a2
    
    append(r1,r2)
  }
  
}



#TypeWell <- data.frame(Time = seq(1:time), 
#                       qProd = DCA(qi, Di, b, Dmin),
#                       NpProd = cumsum(DCA(qi, Di, b, Dmin)),
#                       Yield = AriesYield(Time1, Yield1, Yield2, Yield3))


tw <- data.table(seq_len(time))

for(i in seq_len(nrow(econTbl)))
{
  
  
}

Econ_Metrics <- data.frame(wellnames, map_df(seq_along(wellnames), CF.Metrics))

typewell <- data.frame(Time = seq_len(time))
typewell <- typewell %>%
  mutate(tcName = "tw--1",
         Gas.mcf = DCA(qi, Di, b, Dmin),
         Ratio = AriesYield(Time1, Yield1, Yield2, Yield3), 
         Oil.bbl = Gas.mcf / 1000 * Ratio)

head(typewell)



#EUR <- max(typewell$NpProd, na.rm = TRUE)/1000
#EUR

write.csv(typewell, file = "TW.csv",row.names=FALSE)


