
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
library(dplyr)
library(purrr)
price <- read.csv("price.csv", stringsAsFactors = FALSE)



#############----------------------function-------------------------
price

NCF.tbl <- data.frame()

NCF.tbl <- TypeCurve %>%
  mutate(Time = as.numeric(Time),
         NetDryGas = as.numeric(Gas.mcf * shrink * NRI * work_int),
         NetOil = as.numeric(Oil.bbl * NRI * work_int),
         NetNGL = as.numeric(Gas.mcf/1000 * ngl_yield * NRI * work_int)) %>%
  select(Time, NetDryGas, NetOil, NetNGL)

rev.fx <- function(y)
{
  pr <- subset(price, SCENARIO == y)
  df <- data.frame(
    Scenario = y,
    GasRev = NCF.tbl$NetDryGas * pr$GAS,
    OilRev = NCF.tbl$NetOil * pr$OIL,
    NGLRev = NCF.tbl$NetNGL * pr$NGL)
   
  return(df)
}

tCF <- cbind(NCF.tbl, rev.fx(NCF.tbl, "A"))

CF <- tCF %>%
  mutate(
    NetRev = GasRev + OilRev + NGLRev,
    OpIncome = NetRev - opex,
    Undisc.CF = OpIncome - ifelse(Time == capex_mnth, capex * 1000, 0),
    Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*capex*1000, 0)),
    Disc.CF = as.numeric(OpIncome *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - Disc.Capex)) %>%
  filter(OpIncome > 0) %>%
  select(Scenario, Time, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF)
  
      


y <- price

cshflw_redo <- function(y)
{
  Scenario = y$SCENARIO
  Time = as.numeric(TypeCurve$Time)
  NetDryGas = as.numeric(TypeCurve$Gas.mcf * shrink * NRI * work_int)
  NetOil = as.numeric(TypeCurve$Oil.bbl * NRI * work_int)
  NetNGL = as.numeric(TypeCurve$Gas.mcf/1000 * ngl_yield * NRI * work_int)
  GasRev = NetDryGas * y$GAS
  OilRev = NetOil * y$OIL
  NGLRev = NetNGL * y$NGL
  NetRev = GasRev + OilRev + NGLRev
  OpIncome = ifelse(NetRev - opex > 0, NetRev - opex, 0)
  Undisc.CF = OpIncome - ifelse(Time == capex_mnth, capex * 1000, 0)
  Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*capex*1000, 0))
  Disc.CF = as.numeric(OpIncome *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(Time, NetDryGas, NetOil, NetNGL, GasRev, OilRev, NGLRev, OpIncome, Undisc.CF, 
               Disc.Capex, Disc.CF)
  results <- filter(results, OpIncome > 0) 
  
  results$Scenario <- Scenario

  
  return(results)
}


y <- subset(price, SCENARIO == "B")

cfTble <- cshflw_redo(y)
write.csv(cfTble, file = "cfTble.csv")

sapply(price, cshflw_redo)
map(price, cshflw_redo)




########-------------------FOR LOOP-------------
output <- vector("double", 0)
output <- data.frame()

for(i in 1:4){
  gas_price <- price[i,2]
  oil_price <- price[i,3]
  ngl_price <- price[i,4]
  #output <- rbind(output, data.frame(gas_price, oil_price, ngl_price))
  #output <- cbind(Name = paste(price[i,1], i, sep="-"))
  output <- rbind(output, cshflw_redo(TypeCurve))
  
}
output

pr.sen <- data.frame()
for(i in 1:9)
{
  pl <- plist[i]
  pr.sen <- rbind(pr.sen, rev.fx(pl))
}

pr.sen

pr.sen <- data.frame()
for(i in 1:9)
{
  pl <- price[i,i]
  pr.sen <- rbind(pr.sen, cshflw_redo(pl))
}



plist <- c("A","B", "C", "D", "E", "F", "G", "H", "I")

seeifthisworks <- function(x, Time){
  NCF <- TypeCurve %>%
    mutate(Time = as.numeric(Time),
           NetDryGas = as.numeric(Gas.mcf * shrink * NRI * work_int),
           NetOil = as.numeric(Oil.bbl * NRI * work_int),
           NetNGL = as.numeric(Gas.mcf/1000 * ngl_yield * NRI * work_int))
  
  pr <- subset(price, SCENARIO == x)
  df <- data.frame(
    Scenario = x,
    GasRev = NCF$NetDryGas * pr$GAS,
    OilRev = NCF$NetOil * pr$OIL,
    NGLRev = NCF$NetNGL * pr$NGL)
  
  NCF2 <- df %>%
    mutate(
    NetRev = GasRev + OilRev + NGLRev,
    OpIncome = NetRev - opex,
    Undisc.CF = OpIncome - ifelse(Time == capex_mnth, capex * 1000, 0),
    Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*capex*1000, 0)),
    Disc.CF = as.numeric(OpIncome *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - Disc.Capex)) %>%
    filter(OpIncome > 0) %>%
    select(Scenario, Time, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF)
  cbind(NCF, df, NCF2)
  
}
seeifthisworks("A", 1:302)
rm(seeifthisworks)
Time$TypeCurve


sapply("A", function(x) rev.fx)


###########END FOR LOOP-----------
sapply(price, cshflw_redo)


rm(output, output1, output2)

###############FOR TO CALC NET CASH FLOW----------------
Cashflow$Cumdcf <- 0 # Calculate NCF cummulative using previous value
for(i in 1:length(Cashflow$disc_cash_flow)){
  if(i==1){ Cashflow$Cumdcf[1] <- Cashflow$disc_cash_flow[1] # Start ftom t=1
  }else{
    Cashflow$Cumdcf[i] <- Cashflow$disc_cash_flow[i] + Cashflow$Cumdcf[(i-1)]
  }
}




Time = as.numeric(TypeCurve$Time)
NetDryGas = as.numeric(TypeCurve$Gas.mcf * shrink * NRI * work_int)
  
gMatrix <- matrix(2, ncol = length(gPrice), nrow = length(TypeCurve$Time))
  
gmMatrix <- matrix(0, 50, 5)
  
for(i in seq_along(1:length(gmMatrix))){
    gmMatrix[i,] <- rep(gPrice, each = 1)
}
gmMatrix  
  

  

##create a matrix called "result"
#result <- data.frame(matrix(0, ncol = 2, nrow = length(discRateLng)))

gMatrix <- matrix(0, ncol = length(gPrice), nrow = length(TypeCurve$Time))

test <- cshflw_redo(TypeCurve)

as.numeric(TypeCurve$Gas.mcf * shrink * NRI * work_int)

cumArps <- function(Qi, A, B, Months) {
  if(B==0){
    return(Qi*(1-exp(-A*Months))/A)
  }else if (B==1){
    return(Qi*log(1+A*Months)/A)
  }else {
    return( ( Qi/((1-B)*A))*(1-(1+B*A*Months)^(1-(1/B))) )
  }
}



