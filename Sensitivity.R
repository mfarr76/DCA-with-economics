
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/econtbl.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")


library(dplyr)
library(purrr)
###########-------------------------------------------

###cashflow - F11 output 
#TCGroups <- tcgroup
econTbl <- EconTable
#tcName <- unique(TCGroups$tcName[!is_na(TCGroups$tcName)])
tcName <- unique(typewell$tcName[!is_na(typewell$tcName)])
#wellnames <- unique(TCGroups$Name)

unique(TCGroups$tcName)

#user_price[,1]
user_price <- data_frame()
for(i in seq_along(tcName))
{
  econ_red <- subset(econTbl, tcName == tc_list[i]);
  us1 <- cbind(econ_red[1,2], econ_red[1,3], econ_red[1,3] * 0.4)
  user_price <- rbind(user_price, us1)
  
}


##########wasn't able to match price with well cost with this code########
#gUser <- econTbl[1,2]
#oUser <- econTbl[1,3]
#nUser <- oUser * 0_4
#user_price <- cbind(gUser, oUser, nUser) ##price inputs by user


tc_list <- list()
for(i in seq_along(tcName))
{
  tc_list[i] <- list(tcName[i])
}
capex_mnth <- 1
#inputs <- userinputs[1, 1:ncol(userinputs)]
#inputs <- userinputs


##this function is setup to loop through the individual typecurves
##and use the user variable in the inputs table (capex, opex, wi, nri)
##x = tc_list generates unique names of typecurves
##y = price file name --- user pricefile or sensivity price 
##z = row in the matrix --- user only has 1 row -- sensitivity price has 9 rows
x <- 1
z <- 1
y <- user_price

table <- typewell

cshflow <- function(table, x, y, z) 
{ #first use subset function (reduce) the TC table (TCGroup) to the first TC in the tc_list
  #do the same for the econTbl to so you have the correct values per TC
  red_tc <- subset(table, tcName == tc_list[x]); #reduce/filter the typecurves one at a time 
  red_inputs <- subset(econTbl, tcName == tc_list[x]);
  #TCnamd and date
  tcName = red_tc$tcName;
  Time <-  as_numeric(red_tc$Time);
  
  #gross prd
  GRGas_mcf <- as_numeric(red_tc$Gas_mcf);
  GROil_bbl <- as_numeric(red_tc$Oil_bbl);
  GRNgl_bbl <- as_numeric(GRGas_mcf/1000 * red_inputs$ngl_yield);
  GRBOE <- ((GRGas_mcf * red_inputs$shrink) / 6 + (GRNgl_bbl + GROil_bbl));
  
  #net prod
  NetDryGas_mcf <- as_numeric(GRGas_mcf * red_inputs$shrink * red_inputs$nri);
  NetOil_bbl <- as_numeric(GROil_bbl * red_inputs$nri);
  NetNGL_bbl <- as_numeric(GRNgl_bbl * red_inputs$nri);
  NetBOE <- (NetOil_bbl + NetNGL_bbl + NetDryGas_mcf/6);
  
  #net revenue
  NetGasRev <- NetDryGas_mcf * y[z, 1]; ##example --- price[4,1] -> 2_5
  NetOilRev <- NetOil_bbl * y[z, 2];  ##example --- user_price[1, 2]
  NetNGLRev <- NetNGL_bbl * y[z, 3];
  NetRev <- NetGasRev + NetOilRev + NetNGLRev;
  
  #net expenses
  NetOpex <- red_inputs$opex * red_inputs$wi;
  NetOpIncome <- NetRev - NetOpex;
  NetCapex <- ifelse(Time == 1, ##put capex_month if you want to change the month
                     (red_inputs$capex * 1000 * red_inputs$wi), 0);
  
  #net cashflows
  NetUndiscCF <- NetOpIncome - NetCapex;
  NetDiscCapex <- 1 / (1 + red_inputs$discount_rate)^((capex_mnth - 1)/ 12) * NetCapex;
  NetDiscCF <- as_numeric(NetOpIncome *(1/(1 + red_inputs$discount_rate)^((Time - 0.5) / 12)) - NetDiscCapex);
  NetCumDiscCF <- cumsum(NetDiscCF);
  
  results <- data_frame(tcName, Time, GRGas_mcf, GROil_bbl, GRNgl_bbl, GRBOE, NetDryGas_mcf, NetOil_bbl, NetNGL_bbl,
                        NetBOE, NetGasRev, NetOilRev, NetNGLRev, NetRev, NetOpex ,NetOpIncome, NetUndiscCF, NetDiscCapex, NetDiscCF, NetCumDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0) #stop reporting when income does negative____LOSS ZERO
  
  #return(results)
  return(results_cshflow)
}

###testing############
cshflow(typewell, 1, user_price, 1)
cf_check <- cshflow(typewell, 1, user_price, 1)
head(cf_check)
write_csv(cf_check, file = "cfCheck.csv")
#############

x <-1
y <- user_price
z <- 1
cf <- 
head(cf)
TCGroups
econTbl[12]
ncol(econTbl)
###
user_price[1,]

CashFlow <- data_frame()
for(i in seq_along(tcName)) #number of time to loop
{
  cf1 <- cshflow(i, user_price[i,], 1) #call cshflow function
  CashFlow <- rbind(CashFlow , cf1) #store the results of each loop
}

write_csv(CashFlow, file = "cashflow_csv")



cashflow %>%
  group_by(TCName) %>%
  summarise(max(CumDisc_CF), sum(GROil_bbl), sum(GRGas_mcf), sum(NetDryGas),
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



CashFlowPrice <- data_frame()
for(i in seq_along(tcName)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow(i, price, j)
    cf_price$scenario <- paste(price[j,1], '-' ,price[j,2], '-' , price[j,3])
    cf_price$price <- ifelse(sensitivity_choice == "GAS", price[j,2], price[j, 1]) 
    CashFlowPrice <- rbind(CashFlowPrice , cf_price)
    
  }
}

price[8,1]


CashFlowPrice <- CashFlowPrice %>% 
  group_by(tcName, scenario) %>% 
  summarise(NPV = max(NetCumDiscCF), Price = mean(price)) %>%
  arrange(tcName, Price)





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
  Time <-  as_numeric(red_tc$Time); 
  NetOpIncome <- red_tc$NetOpIncome;
  NetDiscCF <- as_numeric(red_tc$NetOpIncome *(1/(1 + discRate[z])^((Time - 0.5) / 12)) - red_tc$NetDiscCapex);
  
  results <- data_frame(TCName = red_tc$TCName, Time, NetDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0)
  
  
  return(results_cshflow)
}

df <- cshflow_discount(2, 21) 
df %>% 
  group_by(TCName) %>%
  summarise(sum(NetDiscCF))

CashFlowDisc <- data_frame()
for(i in seq_along(tcName)){
  for(j in seq_along(discRate))
  {
    cf_dr <- cshflow_discount(i, j)
    cf_dr$Disc_Rate <- paste(as_numeric(discRate[j]))
    CashFlowDisc <- rbind(CashFlowDisc, cf_dr)
    
  }
}

CashFlowDisc <- CashFlowDisc %>%
  mutate(Disc_Rate = as_numeric(Disc_Rate)) %>%
  group_by(tcName, Disc_Rate) %>%
  summarise(NPV = sum(NetDiscCF))

rm(n)
n <- 1


CF_Metrics <- function(n){
  
  irr_sub <- subset(CashFlowDisc, tcName == tc_list[n]); #subset to link colnames with tc name
  IRR <- ifelse(irr_sub[21,3] > 0, 1, 
                unlist(approx(irr_sub$NPV, irr_sub$Disc_Rate, 0))[2]); #approx fun iterates to find the 0 value
  colnames(IRR) <- "IRR" ;
  
  dpi_sub <- subset(CashFlow, tcName == tc_list[n]); #subset to link colnames with tc name
  DPI <- sum(dpi_sub$NetDiscCF, na_rm = TRUE) / sum(dpi_sub$NetDiscCapex, na_rm = TRUE) + 1; 
  Payout_disc <- unlist(approx(dpi_sub$NetCumDiscCF, dpi_sub$Time, 0))[2] / 12;
  NetEUR_MBOE <- sum(dpi_sub$NetBOE, na_rm = TRUE) / 1000;
  Capex_M <- max(dpi_sub$NetDiscCapex, na_rm = TRUE) / 1000;
  FnD <-  max(dpi_sub$NetDiscCapex, na_rm = TRUE) / sum(dpi_sub$NetBOE, na_rm = TRUE);
  Lifting_Cost <- sum(dpi_sub$NetOpex, na_rm = TRUE) / sum(dpi_sub$NetBOE, na_rm = TRUE);
  NPV15 <- max(dpi_sub$NetCumDiscCF, na_rm = TRUE);
  
  
  brkEven_sub <- subset(CashFlowPrice, tcName == tc_list[n]); #subset to link colnames with tc name
  BrkEven <- unlist(approx(brkEven_sub$NPV, brkEven_sub$Price, 0)[2]);
  
  
  
  
  metrics <- data_frame(NetEUR_MBOE, Capex_M, NPV15 ,IRR, DPI, BrkEven, Payout_disc, FnD, Lifting_Cost)
  
  return(metrics)
}
tcNames <- wellnames

Econ_Metrics <- data_frame(tcName, map_df(seq_along(tc_list), CF_Metrics))


n <- 1
dpi_sub <- subset(cf_check, tcName == tc_list[n]); #subset to link colnames with tc name
DPI <- sum(dpi_sub$NetDiscCF, na_rm = TRUE) / sum(dpi_sub$NetDiscCapex, na_rm = TRUE) + 1; 
Payout_disc <- unlist(approx(dpi_sub$NetCumDiscCF, dpi_sub$Time, 0))[2];
EUR_MBOE <- sum(dpi_sub$GRBOE_bbl, na_rm = TRUE) / 1000;
Capex <- max(dpi_sub$NetDiscCapex, na_rm = TRUE);
FnD <-  max(dpi_sub$NetDiscCapex, na_rm = TRUE) / sum(dpi_sub$NetBOE, na_rm = TRUE);
Lifting_Cost <- sum(dpi_sub$NetOpex, na_rm = TRUE) / sum(dpi_sub$NetBOE, na_rm = TRUE);
NPV15 <- max(dpi_sub$NetCumDiscCF, na_rm = TRUE)