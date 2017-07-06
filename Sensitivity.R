
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow_v2.RData")

library(dplyr)
library(purrr)
###########-------------------------------------------

###cashflow - F11 output 


wellnames <- unique(TCGroups$Name)

gUser <- 3
oUser <- 100
nUser <- oUser * 0.4
user_price <- cbind(gUser, oUser, nUser)


tc_list <- list()
for(i in 1:length(wellnames))
{
  tc_list[i] <- list(wellnames[i])
}
capex_mnth <- 1
#inputs <- userinputs[1, 1:ncol(userinputs)]
inputs <- userinputs

inputs[2,1]

red_tc <- subset(TCGroups, Name == tc_list[1])

red_inputs <- subset(inputs, wellname == tc_list[1])



##this function is setup to loop through the individual typecurves
##and use the user variable in the inputs table (capex, opex, wi, nri)
##x = tc_list generates unique names of typecurves
##y = price file name --- user pricefile or sensivity price 
##z = row in the matrix --- user only has 1 row -- sensitivity price has 9 rows

cshflow <- function(x, y, z) 

{ 
  red_tc <- subset(TCGroups, Name == tc_list[x]) #reduce/filter the typecurves one at a time 
  #red_inputs <- inputs[x, 1:ncol(inputs)] #reduce/filter the user inputs to match the typecurves
  red_inputs <- subset(inputs, wellname == tc_list[x])
  #red_inputs <- if(tc_list[x] != inputs[x, 1]){stop("Error", call. = FALSE)}else{inputs[x, 1:ncol(inputs)]}
  TCName = red_tc$Name
  Time <-  as.numeric(red_tc$Time) #
  GRGas.mcf <- as.numeric(red_tc$Gas.mcf)
  GROil.bbl <- as.numeric(red_tc$Oil.bbl)
  GRNgl.bbl <- as.numeric(GRGas.mcf/1000 * red_inputs$ngl.yield)
  NetDryGas <- as.numeric(GRGas.mcf * red_inputs$shrink * red_inputs$nri * red_inputs$wi)
  NetOil <- as.numeric(GROil.bbl * red_inputs$nri * red_inputs$wi)
  NetNGL <- as.numeric(GRNgl.bbl * red_inputs$nri * red_inputs$wi)
  GasRev <- NetDryGas * y[z, 1] ##example --- price[4,1] -> 2.5
  OilRev <- NetOil * y[z, 2]  ##example --- user_price[1, 2]
  NGLRev <- NetNGL * y[z, 3]
  NetRev <- GasRev + OilRev + NGLRev
  OpIncome <- NetRev - red_inputs$opex
  Undisc.CF <- OpIncome - ifelse(Time == capex_mnth, red_inputs$capex * 1000, 0)
  Disc.Capex <- (ifelse(Time == capex_mnth, (1/(1 + red_inputs$discount.rate)^((capex_mnth - 1)/12))*red_inputs$capex*1000, 0))
  Disc.CF <- as.numeric(OpIncome *(1/(1 + red_inputs$discount.rate)^((Time - 0.5) / 12)) - Disc.Capex)
  CumDisc.CF <- cumsum(Disc.CF) 
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, NetDryGas, NetOil, NetNGL,
                        GasRev, OilRev, NGLRev, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF, CumDisc.CF)
  
  results_cshflow <- filter(results, OpIncome > 0)
  
  
  return(results_cshflow)
}

xy <- cshflow(2, user_price, 1)
unique(xy$TCName)

CashFlow1 <- data.frame()
for(i in 1:length(wellnames))
{
  cf1 <- cshflow(i, user_price, 1)
  CashFlow1 <- rbind(CashFlow1, cf1)
}
CashFlow1




CashFlow1 %>%
  group_by(TCName) %>%
  summarise(max(CumDisc.CF), sum(GROil.bbl), sum(GRGas.mcf), sum(NetDryGas),
            sum(NetRev), sum(GasRev), sum(OilRev), sum(NGLRev))



sensitivity_choice <- "GAS"
flat_price <- 3


if(sensitivity_choice == "GAS")
{
  gPrice <- flat_price
  oPrice <- seq(30, 70, by = 5)
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}else{
  gPrice <- seq(1, 5, by = 0.5)
  oPrice <- flat_price
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}



######------------------
#price sensitivity




CashFlowPrice <- data.frame()
for(i in 1:length(wellnames)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow(i, price, j)
    cf_price$scenario <- paste(price[j,1], '-' ,price[j,2], '-' , price[j,3])
    CashFlowPrice <- rbind( CashFlowPrice , cf_price)
    
  }
}

price[8,1]

CashFlowPrice %>%
  group_by(scenario) %>%
  summarise(max(CumDisc.CF))




###########-------------------------------------------

###sensitivity on price###

sensitivity_choice <- "GAS"
flat_price <- 3


if(sensitivity_choice == "GAS")
{
  gPrice <- flat_price
  oPrice <- seq(30, 70, by = 5)
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}else{
  gPrice <- seq(1, 5, by = 0.5)
  oPrice <- flat_price
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}
price[2, "gPrice"]
price[4, 1]

############discount sensitivity


cf <- function(prodMonth, capex, capexMonth, operIncome)
{
  #change units for capex
  capexM <- capex*1000
  #create discount rate range from 0 to 100%
  discRate <- seq(0, 1, by = 0.05) 
  #sequental list of discRate from 0 to 21
  discRateLng <- as.numeric(seq_along(discRate))
  
  ##sapply function to create a matrix of discount rate and production month
  discRateTbl <- (sapply(discRate, (function(x) 1/(1 + x)^(((prodMonth - 0.5) / 12)))))
  ##sapply function to create a matrix of discount rate for capex
  discCapex <- (sapply(discRate, (function(x) (1/(1 + x)^((capexMonth - 1)/12))*capexM)))
  ##cash flow table if oper income is > 0....ie..LOSS ZERO
  discCfTbl <- data.frame(sapply(discRateLng, function(x) ifelse((discRateTbl[,x]*operIncome)>0, 
                                                                 (discRateTbl[,x]*operIncome), NA)))
  ##subtract capex from cash flow table based on capex month              
  discCfTbl[capexMonth,] <- discCfTbl[capexMonth,] - discCapex 
  
  ##map function from purrr to sum up the column to get a single value
  ##could to use apply function instead
  discCfSum <- map_df(discCfTbl, sum, na.rm = TRUE)
  #discCfSum <- apply(discCfTbl, 2, sum)
  ##transpose
  discCfSum <- t(discCfSum)
  
  ##create a matrix called "result"
  #result <- data.frame(matrix(0, ncol = 2, nrow = length(discRateLng)))
  result <- data.frame(Disc.Rate = 0, NPV = discRateLng )
  ##change the column names
  #colnames(result) <- c("Disc.Rate", "NPV")
  
  ##write the results to the table
  result$Disc.Rate <- discRate
  result$NPV <- discCfSum
  
  result 
}


NPV.Table <- cf(Cashflow$Time, capex, capex_mnth, Cashflow$oper_income)




discRate <- seq(0, 1, by = 0.05) 
discRate[5]

cshflow_discount <- function(x, z) 
  
{ 
  red_tc <- subset(TCGroups, Name == tc_list[x]) #reduce/filter the typecurves one at a time 
  red_inputs <- inputs[x, 1:ncol(inputs)] #reduce/filter the user inputs to match the typecurves
  TCName = red_tc$Name
  Time <-  as.numeric(red_tc$Time) #
  NetRev <- CashFlow1$GasRev + CashFlow1$OilRev + CashFlow1$NGLRev
  OpIncome <- NetRev - red_inputs$opex
  Undisc.CF <- OpIncome - ifelse(Time == capex_mnth, red_inputs$capex * 1000, 0)
  Disc.Capex <- (ifelse(Time == capex_mnth, (1/(1 + discRate[z])^((capex_mnth - 1)/12))*red_inputs$capex*1000, 0))
  Disc.CF <- as.numeric(OpIncome *(1/(1 + discRate[z])^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(TCName, Time, Disc.CF)
  
  results_cshflow <- filter(results, OpIncome > 0)
  
  
  return(results_cshflow)
}

df <- cshflow_discount(2, 21) 
df %>% 
  group_by(TCName) %>%
  summarise(sum(Disc.CF))

Cashflow.disc <- data.frame()
for(i in 1:length(wellnames)){
  for(j in 1:length(discRate))
  {
    cf_dr <- cshflow_discount(i, j)
    cf_dr$scenario <- paste(discRate[j])
    Cashflow.disc <- rbind(Cashflow.disc, cf_dr)
    
  }
}

cashflow.dr <- Cashflow.disc %>%
  group_by(TCName, scenario) %>%
  summarise(sum(Disc.CF))

