
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")

library(dplyr)
library(purrr)
###########-------------------------------------------

###cashflow - F11 output 
TCGroups <- tcgroup

apply(TCGroups$Name, 2, function(x) unique(x[!is.na(x)]))

wellnames <- unique(TCGroups$Name[!is.na(TCGroups$Name)])

#wellnames <- unique(TCGroups$Name)

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
  oPrice <- seq(10, 100, by = 10)
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}else{
  gPrice <- seq(1, 5.5, by = 0.5)
  oPrice <- flat_price
  nPrice <- oPrice *0.4
  price <- cbind(gPrice, oPrice, nPrice)
}

price[2,2]

######------------------
#price sensitivity




CashFlowPrice <- data.frame()
for(i in 1:length(wellnames)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow(i, price, j)
    cf_price$scenario <- paste(price[j,1], '-' ,price[j,2], '-' , price[j,3])
    cf_price$price <- ifelse(sensitivity_choice == "GAS", price[j,2], price[j, 1]) 
    CashFlowPrice <- rbind( CashFlowPrice , cf_price)
    
  }
}

price[8,1]

rm(CashflowPrice)

CashFlowPrice <- CashFlowPrice %>% 
  group_by(TCName, scenario) %>% 
  summarise(NPV = max(CumDisc.CF), Price = mean(price)) %>%
  arrange(TCName, Price)





###########-------------------------------------------



###########-------------------------------------------

###discount sensitivity



discRate <- seq(0, 1, by = 0.05) 
discRate[21]

x <- 2
z <- 21
rm(x)
rm(z)
cshflow_discount <- function(x, z) 
  
{ 
  red_tc <- subset(CashFlow, TCName == tc_list[x]) #reduce/filter the typecurves one at a time 
  red_inputs <- subset(inputs, wellname == tc_list[x]) #reduce/filter the user inputs to match the typecurves
  #TCName = red_tc$Name
  Time <-  as.numeric(red_tc$Time) #
  NetRev <- red_tc$GasRev + red_tc$OilRev + red_tc$NGLRev
  OpIncome <- NetRev - red_inputs$opex
  Undisc.CF <- OpIncome - ifelse(Time == capex_mnth, red_inputs$capex * 1000, 0)
  Disc.Capex <- (ifelse(Time == capex_mnth, (1/(1 + discRate[z])^((capex_mnth - 1)/12))*red_inputs$capex*1000, 0))
  Disc.CF <- as.numeric(OpIncome *(1/(1 + discRate[z])^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(TCName = red_tc$TCName, Time, Disc.CF)
  
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
    cf_dr$Disc.Rate <- paste(as.numeric(discRate[j]))
    Cashflow.disc <- rbind(Cashflow.disc, cf_dr)
    
  }
}

Cashflow.disc <- Cashflow.disc %>%
  mutate(Disc.Rate = as.numeric(Disc.Rate)) %>%
  group_by(TCName, Disc.Rate) %>%
  summarise(NPV = sum(Disc.CF))






CF.Metrics <- function(n){
  irr_sub <- subset(Cashflow.disc, TCName == wellnames[n])
  IRR <- ifelse(irr_sub[21,3] > 0, 1, 
                unlist(approx(irr_sub$NPV, irr_sub$Disc.Rate, 0))[2])
  colnames(IRR) <- "IRR"
  
  dpi_sub <- subset(CashFlow, TCName == wellnames[n])
  DPI <- sum(dpi_sub$Disc.CF, na.rm = TRUE) / sum(dpi_sub$Disc.Capex, na.rm = TRUE) + 1  
  
  brkEven_sub <- subset(CashFlowPrice, TCName == wellnames[n])
  BrkEven <- unlist(approx(brkEven_sub$NPV, brkEven_sub$Price, 0)[2])
  
  metrics <- data.frame(IRR, DPI, BrkEven)
  
  return(metrics)
}





xy <- subset(Cashflow.disc, TCName == wellnames[2])
mm <- ifelse(xy[21,3] > 0 , 1, unlist(approx(xy$NPV, xy$Disc.Rate, 0))[2])

xy[21,2]
xy[21,3]


brkEven_sub <- subset(CashFlowPrice, TCName == wellnames[n])
BrkEven <- unlist(approx(brkEven_sub$NPV, brkEven_sub$Price, 0)[2])


n <- 3


Econ_Metrics <- data.frame(wellnames, map_df(1:length(wellnames), CF.Metrics))



