
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow_v2.RData")



###########------------------



wellnames <- unique(TCGroups$Name)

tc_list <- list()
for(i in 1:length(wellnames))
{
  tc_list[i] <- list(wellnames[i])
}
capex_mnth <- 1

cshflow <- function(x)
{
  red_tc <- subset(TCGroups, Name == tc_list[x]) #reduce/filter the typecurves one at a time 
  red_inputs <- inputs[x, 1:ncol(inputs)] #reduce/filter the user inputs to match the typecurves
  TCName = red_tc$Name
  Time = as.numeric(red_tc$Time)
  GRGas.mcf = as.numeric(red_tc$Gas.mcf)
  GROil.bbl = as.numeric(red_tc$Oil.bbl)
  GRNgl.bbl = as.numeric(GRGas.mcf/1000 * red_inputs$ngl.yield)
  NetDryGas = as.numeric(GRGas.mcf * red_inputs$shrink * red_inputs$nri * red_inputs$wi)
  NetOil = as.numeric(GROil.bbl * red_inputs$nri * red_inputs$wi)
  NetNGL = as.numeric(GRNgl.bbl * red_inputs$nri * red_inputs$wi)
  GasRev = NetDryGas * gas_price
  OilRev = NetOil * oil_price
  NGLRev = NetNGL * ngl_price
  NetRev = GasRev + OilRev + NGLRev
  OpIncome = NetRev - red_inputs$opex
  Undisc.CF = OpIncome - ifelse(Time == capex_mnth, red_inputs$capex * 1000, 0)
  Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + red_inputs$Discount_Rate)^((capex_mnth - 1)/12))*red_inputs$capex*1000, 0))
  Disc.CF = as.numeric(OpIncome *(1/(1 + red_inputs$Discount_Rate)^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, NetDryGas, NetOil, NetNGL,
                        GasRev, OilRev, NGLRev, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF)
  
  results <- filter(results, OpIncome > 0)
  
  
  return(results)
}

xy <- cshflow(2)
unique(xy$TCName)

CashFlow <- data.frame()
for(i in 1:length(wellnames))
{
  cf1 <- cshflow(i)
  CashFlow <- rbind(CashFlow, cf1)
}
CashFlow

###-------------------------------------------------------------


###sensitivity on price###

sensitivity_choice <- "GAS"
flat_price <- 3


price <- matrix(0, 9, 3)
colnames(price) <- c("gPrice", "oPRice", "nPrice")


price <- ifelse(sensitivity_choice == "GAS",(
  price[,1] <- flat_price
  price[,2] <- seq(30, 70, by = 5)
  price[,3] <- price[,2] *0.4),( 
    price[,1] <- seq(1, 5, by = 0.5)
    price[,2] <- flat_price
    price[,3] <- price[,2] *0.4)) 


price[,1:3] <- c(flat_price, seq(30, 70, by = 5),price[,2] *0.4)

rm(price)

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
price[[2, "gPrice"]]

gPrice <- seq(1, 5, by = 0.5)
oPrice <- seq(30, 70, by = 5)
nPrice <- oPrice *0.4
price <- cbind(gPrice, oPrice, nPrice)


cshflow_price <- function(x, y)
{
  red_tc <- subset(TCGroups, Name == tc_list[x])
  red_inputs <- inputs[x, 1:ncol(inputs)]
  TCName = red_tc$Name
  Time = as.numeric(red_tc$Time)
  GRGas.mcf = as.numeric(red_tc$Gas.mcf)
  GROil.bbl = as.numeric(red_tc$Oil.bbl)
  GRNgl.bbl = as.numeric(GRGas.mcf/1000 * red_inputs$ngl.yield)
  NetDryGas = as.numeric(GRGas.mcf * red_inputs$shrink * red_inputs$nri * red_inputs$wi)
  NetOil = as.numeric(GROil.bbl * red_inputs$nri * red_inputs$wi)
  NetNGL = as.numeric(GRNgl.bbl * red_inputs$nri * red_inputs$wi)
  GasRev = NetDryGas * price[[y, "gPrice"]]
  OilRev = NetOil * price[[y, "oPrice"]]
  NGLRev = NetNGL * price[[y, "nPrice"]]
  NetRev = GasRev + OilRev + NGLRev
  OpIncome = NetRev - red_inputs$opex
  Undisc.CF = OpIncome - ifelse(Time == capex_mnth, red_inputs$capex * 1000, 0)
  Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + red_inputs$discount.rate)^((capex_mnth - 1)/12))*red_inputs$capex*1000, 0))
  Disc.CF = as.numeric(OpIncome *(1/(1 + red_inputs$discount.rate)^((Time - 0.5) / 12)) - Disc.Capex)
  CumDisc.CF = cumsum(Disc.CF) 
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, NetDryGas, NetOil, NetNGL,
                        GasRev, OilRev, NGLRev, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF, CumDisc.CF)
  
  results_price <- filter(results, OpIncome > 0)
  
  
  return(results_price)
}

cshflow_price(1, 5)

CashFlowPrice <- data.frame()
for(i in 1:length(wellnames)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow_price(i,j)
    cf_price$scenario <- paste(j)
    CashFlowPrice <- rbind( CashFlowPrice , cf_price)
    
  }
}


unique(CashFlowPrice$TCName)



