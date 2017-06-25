rm(list = ls())
library(dplyr)

#TypeWell <- read.csv("1Well.csv", header = TRUE)
#TypeWell$Oil <- as.numeric(TypeWell$Oil)
#TypeWell$Gas <- as.numeric(TypeWell$Gas)


qi <- 10000
b <- 1.2
Di <- 80
Dmin <- 10
Years <- 30
Day.Month = "Months"
Time1 <- 5
Yield1 <- 100
Yield2 <- 1000
Yield3 <- 2000
pPhase <- quote(Oil)  
t.units <- ifelse(Day.Month == "Months", 12, 365)
time <- Years * t.units
abRate <- 150

DCA <- function(qi, Di, b, Dmin)
{
  Di <- Di/100
  Dmin <- Dmin/100
  
  prod.time <- seq(1:time)
  t2 <- prod.time - 1
  
TypeCurve <- if(b == 0)
  {
    ai <- -log(1-Di)/t.units
    Exp.Np1 <- qi / ai * (1 - exp(-ai * prod.time))
    Exp.Np2 <- qi / ai * (1 - exp(-ai * t2))
    
    Exp.Np1 - Exp.Np2

    
  }else if(b == 1){
    
    ai <- (Di / (1 - Di) / t.units)
    Har.Np1 <- qi / ai * log(1 + ai * prod.time)
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
    
    ##########forecast expontintal to abandonment rate (Terminal decline portion of the curve)#########
    aDmin <- -log(1 - Dmin)/t.units
    q.trans <- qi / ((1 + b * ai * t.trans))^(1 / b) #rate at transition month 
    Np.trans <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b * t.trans)^((1 - b) / b)))) #cum volume at tranistion month
    t.ab <- seq(1:(time - t.trans))
    t.ab1 <- 0:(time - t.trans - 1)
    
    #########forecast from Dmin to end of forecast
    Exp.Np1 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.ab))
    Exp.Np2 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * t.ab1))
    Exp.Np <- Exp.Np1 - Exp.Np2
    
    append(Hyp.NpDmin, Exp.Np)

  }
    ifelse(TypeCurve >= abRate, TypeCurve, NA)
}
##############
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



TypeWell <- data.frame(Time = seq(1:time), 
                       qProd = DCA(qi, Di, b, Dmin),
                       NpProd = cumsum(DCA(qi, Di, b, Dmin)),
                       Yield = AriesYield(Time1, Yield1, Yield2, Yield3))

EUR <- max(TypeWell$NpProd, na.rm = TRUE)/1000
EUR

#write.csv(TypeWell, file = "TW.csv",row.names=FALSE)


