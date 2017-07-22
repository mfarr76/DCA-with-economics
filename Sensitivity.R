
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/econtbl.RData")

library(dplyr)
library(purrr)
###########-------------------------------------------

###cashflow - F11 output 
TCGroups <- tcgroup
econTbl <- EconTable
wellnames <- unique(TCGroups$tcName[!is.na(TCGroups$tcName)])

#wellnames <- unique(TCGroups$Name)





gUser <- econTbl[1,2]
oUser <- econTbl[1,3]
nUser <- oUser * 0.4
user_price <- cbind(gUser, oUser, nUser)


tc_list <- list()
for(i in seq_len(length(wellnames)))
{
  tc_list[i] <- list(wellnames[i])
}
capex_mnth <- 1
#inputs <- userinputs[1, 1:ncol(userinputs)]
#inputs <- userinputs

inputs[2,1]

red_tc <- subset(TCGroups, Name == tc_list[1])

red_inputs <- subset(inputs, wellname == tc_list[1])



##this function is setup to loop through the individual typecurves
##and use the user variable in the inputs table (capex, opex, wi, nri)
##x = tc_list generates unique names of typecurves
##y = price file name --- user pricefile or sensivity price 
##z = row in the matrix --- user only has 1 row -- sensitivity price has 9 rows
x <- 1
z <- 1
y <- user_price


cshflow <- function(x, y, z) 
{ #first use subset function (reduce) the TC table (TCGroup) to the first TC in the tc_list
  #do the same for the econTbl to so you have the correct values per TC
  red_tc <- subset(TCGroups, tcName == tc_list[x]); #reduce/filter the typecurves one at a time 
  red_inputs <- subset(econTbl, tcName == tc_list[x]);
  #TCnamd and date
  TCName = red_tc$tcName;
  Time <-  as.numeric(red_tc$Time);
  
  #gross prd
  GRGas.mcf <- as.numeric(red_tc$Gas.mcf);
  GROil.bbl <- as.numeric(red_tc$Oil.bbl);
  GRNgl.bbl <- as.numeric(GRGas.mcf/1000 * red_inputs$ngl.yield);
  GRBOE.bbl <- ((GRGas.mcf * red_inputs$shrink) / 6 + (GRNgl.bbl + GROil.bbl));
  
  #net prod
  NetDryGas.mcf <- as.numeric(GRGas.mcf * red_inputs$shrink * red_inputs$nri);
  NetOil.bbl <- as.numeric(GROil.bbl * red_inputs$nri);
  NetNGL.bbl <- as.numeric(GRNgl.bbl * red_inputs$nri);
  NetBOE <- (NetOil.bbl + NetNGL.bbl + NetDryGas.mcf/6);
  
  #net revenue
  NetGasRev <- NetDryGas.mcf * y[z, 1]; ##example --- price[4,1] -> 2.5
  NetOilRev <- NetOil.bbl * y[z, 2];  ##example --- user_price[1, 2]
  NetNGLRev <- NetNGL.bbl * y[z, 3];
  NetRev <- NetGasRev + NetOilRev + NetNGLRev;
  
  #net expenses
  NetOpex <- red_inputs$opex * red_inputs$wi;
  NetOpIncome <- NetRev - NetOpex;
  NetCapex <- ifelse(Time == capex_mnth, (red_inputs$capex * 1000 * red_inputs$wi), 0);
  
  #net cashflows
  NetUndiscCF <- NetOpIncome - NetCapex;
  NetDiscCapex <- 1 / (1 + red_inputs$discount.rate)^((capex_mnth - 1)/ 12) * NetCapex;
  NetDiscCF <- as.numeric(NetOpIncome *(1/(1 + red_inputs$discount.rate)^((Time - 0.5) / 12)) - NetDiscCapex);
  NetCumDiscCF <- cumsum(NetDiscCF);
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, GRBOE.bbl, NetDryGas.mcf, NetOil.bbl, NetNGL.bbl,
                        NetBOE, NetGasRev, NetOilRev, NetNGLRev, NetRev, NetOpex ,NetOpIncome, NetUndiscCF, NetDiscCapex, NetDiscCF, NetCumDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0) #stop reporting when income does negative....LOSS ZERO
  
  #return(results)
  return(results_cshflow)
}

###testing 
cshflow(3, user_price, 1)

x <-1
y <- user_price
z <- 1
cf <- 
head(cf)
TCGroups
econTbl[12]
ncol(econTbl)
###


CashFlow <- data.frame()
for(i in seq_len(length(wellnames))) #number of time to loop
{
  cf1 <- cshflow(i, user_price, 1) #call cshflow function
  CashFlow <- rbind(CashFlow , cf1) #store the results of each loop
}





cashflow %>%
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


seq_along(wellnames)

CashFlowPrice <- data.frame()
for(i in seq_along(wellnames)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow(i, price, j)
    cf_price$scenario <- paste(price[j,1], '-' ,price[j,2], '-' , price[j,3])
    cf_price$price <- ifelse(sensitivity_choice == "GAS", price[j,2], price[j, 1]) 
    CashFlowPrice <- rbind(CashFlowPrice , cf_price)
    
  }
}

price[8,1]

rm(cashflow.price)

CashFlowPrice <- CashFlowPrice %>% 
  group_by(TCName, scenario) %>% 
  summarise(NPV = max(NetCumDiscCF), Price = mean(price)) %>%
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
  #red_inputs <- subset(inputs, wellname == tc_list[x]) #reduce/filter the user inputs to match the typecurves
  Time <-  as.numeric(red_tc$Time); 
  NetOpIncome <- red_tc$NetOpIncome;
  NetDiscCF <- as.numeric(red_tc$NetOpIncome *(1/(1 + discRate[z])^((Time - 0.5) / 12)) - red_tc$NetDiscCapex);
  
  results <- data.frame(TCName = red_tc$TCName, Time, NetDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0)
  
  
  return(results_cshflow)
}

df <- cshflow_discount(2, 21) 
df %>% 
  group_by(TCName) %>%
  summarise(sum(NetDiscCF))

CashFlowDisc <- data.frame()
for(i in seq_along(wellnames)){
  for(j in seq_along(discRate))
  {
    cf_dr <- cshflow_discount(i, j)
    cf_dr$Disc.Rate <- paste(as.numeric(discRate[j]))
    CashFlowDisc <- rbind(CashFlowDisc, cf_dr)
    
  }
}

CashFlowDisc <- CashFlowDisc %>%
  mutate(Disc.Rate = as.numeric(Disc.Rate)) %>%
  group_by(TCName, Disc.Rate) %>%
  summarise(NPV = sum(NetDiscCF))


n <- 1


CF.Metrics <- function(n){
  
  irr.sub <- subset(CashFlowDisc, TCName == wellnames[n]); #subset to link colnames with tc name
  IRR <- ifelse(irr.sub[21,3] > 0, 1, 
                unlist(approx(irr.sub$NPV, irr.sub$Disc.Rate, 0))[2]); #approx fun iterates to find the 0 value
  colnames(IRR) <- "IRR" ;
  
  dpi.sub <- subset(CashFlow, TCName == wellnames[n]); #subset to link colnames with tc name
  DPI <- sum(dpi.sub$NetDiscCF, na.rm = TRUE) / sum(dpi.sub$NetDiscCapex, na.rm = TRUE) + 1; 
  Payout.disc <- unlist(approx(dpi.sub$NetCumDiscCF, dpi.sub$Time, 0))[2];
  EUR.MBOE <- sum(dpi.sub$GRBOE.bbl, na.rm = TRUE) / 1000;
  Capex <- max(dpi.sub$NetDiscCapex, na.rm = TRUE);
  FnD <-  max(dpi.sub$NetDiscCapex, na.rm = TRUE) / sum(dpi.sub$NetBOE, na.rm = TRUE);
  Lifting.Cost <- sum(dpi.sub$NetOpex, na.rm = TRUE) / sum(dpi.sub$NetBOE, na.rm = TRUE);
  NPV15 <- max(dpi.sub$NetCumDiscCF, na.rm = TRUE);
  
  
  brkEven.sub <- subset(CashFlowPrice, TCName == wellnames[n]); #subset to link colnames with tc name
  BrkEven <- unlist(approx(brkEven.sub$NPV, brkEven.sub$Price, 0)[2]);
  
  
  
  
  metrics <- data.frame(EUR.MBOE, NPV15 ,IRR, DPI, BrkEven, Payout.disc, FnD, Lifting.Cost)
  
  return(metrics)
}


Econ_Metrics <- data.frame(wellnames, map_df(1:length(wellnames), CF.Metrics))



