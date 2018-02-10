
rm(list = ls())

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/econtbl.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tbl4r.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")

##copy below into spotfire==================================================================

##Michael Farr SM Energy 10/18/17===========================================================
##This data function will generate a cash flow model built using Aries cash flow as a check

##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package==============================================================================
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)


##user inputs saved to a tbl
econTable

econTable <- na.omit(econTable)

#uniques typecurve name and not NA to run economics
#tc_unique_names <- unique(econTable$TC_Group[!is.na(econTable$TC_Group)])
tc_name <- unique(econTable$TC_Group)

capex_mnth <- 1 #hard code, not setup to change at this time

##for loop to generate uniques wellnames and store it in tc_list
tc_list <- list()
for(i in seq_along(tc_name))
{
  tc_list[i] <- list(tc_name[i])
}



####get price entered from user input box
user_price <- data.frame()
for(i in seq_along(tc_name))
{
  econ_red <- subset(econTable, TC_Group == tc_list[i]);
  us1 <- cbind(econ_red[1,2], econ_red[1,3], econ_red[1,3] * 0.4)
  user_price <- rbind(user_price, us1)
  
}


##this function is setup to loop through the individual typecurves
##and use the user variable in the inputs table (capex, opex, wi, nri)
#x <- 1
#z <- 1
#y <- user_price
##x = tc.list generates unique names of typecurves
##y = price file name --- user pricefile or sensivity price 
##z = row in the matrix --- user only has 1 row -- sensitivity price has 9 rows

TcForecast <- TcForecast %>%
  filter(Well_Type == "TypeCurve") %>%
  arrange(TC_Group, Time) %>%
  select(TC_Group, Time, Gas_mcf_TC, Oil_bbl_TC)

cshflow <- function(x, y, z) 
{ #first use subset function (reduce) the TC table (TCGroup) to the first TC in the tc.list
  #do the same for the econTable to so you have the correct values per TC
  red_tc <- subset(TcForecast, TC_Group == tc_list[i]); #reduce/filter the typecurves one at a time 
  red_inputs <- subset(econTable, TC_Group == tc_list[i]);
  #TCname and date
  TC_Group = red_tc$TC_Group;
  Time <-  as.numeric(red_tc$Time);
  
  #gross prd
  GRGas_mcf <- (red_tc$Gas_mcf);
  GROil_bbl <- (red_tc$Oil_bbl);
  GRNgl_bbl <- (GRGas_mcf/1000 * red_inputs$NGL_Yield);
  GRBOE <- ((GRGas_mcf * red_inputs$Shrink) / 6 + (GRNgl_bbl + GROil_bbl));
  
  #net prod
  NetDryGas_mcf <- (GRGas_mcf * red_inputs$Shrink * red_inputs$NRI);
  NetOil_bbl <- (GROil_bbl * red_inputs$NRI);
  NetNGL_bbl <- (GRNgl_bbl * red_inputs$NRI);
  NetBOE <- (NetOil_bbl + NetNGL_bbl + NetDryGas_mcf/6);
  
  #net revenue
  NetGasRev <- NetDryGas_mcf * y[z, 1]; ##example --- price[4,1] -> 2.5
  NetOilRev <- NetOil_bbl * y[z, 2];  ##example --- user.price[1, 2]
  NetNGLRev <- NetNGL_bbl * y[z, 3];
  NetRev <- NetGasRev + NetOilRev + NetNGLRev;
  
  #net expenses
  NetOpex <- red_inputs$Opex * red_inputs$WI;
  NetOpIncome <- NetRev - NetOpex;
  NetCapex <- ifelse(Time == capex_mnth, (red_inputs$Capex * 1000 * red_inputs$WI), 0);
  
  #net cashflows
  NetUndiscCF <- NetOpIncome - NetCapex;
  NetDiscCapex <- 1 / (1 + red_inputs$Discount_Rate)^((capex_mnth - 1)/ 12) * NetCapex;
  NetDiscCF <- (NetOpIncome *(1/(1 + red_inputs$Discount_Rate)^((Time - 0.5) / 12)) - NetDiscCapex);
  NetCumDiscCF <- cumsum(NetDiscCF);
  
  results <- data.frame(TC_Group, Time, GRGas_mcf, GROil_bbl, GRNgl_bbl, GRBOE, NetDryGas_mcf, NetOil_bbl, 
                        NetNGL_bbl, NetBOE, NetGasRev, NetOilRev, NetNGLRev, NetRev, NetOpex ,NetOpIncome, 
                        NetUndiscCF, NetDiscCapex, NetDiscCF, NetCumDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0) #stop reporting when income does negative....LOSS ZERO
  
  #return(results)
  return(results_cshflow)
}

#test <- cshflow(1, user_price, 1)

#cashflow loop for all the TC
CashFlow <- data.frame()
for(i in seq_along(tc_name)) #number of time to loop
{
  cf1 <- cshflow(i, user_price[i,], 1) #call cshflow function
  CashFlow <- rbind(CashFlow , cf1) #store the results of each loop
}

#-----------------------------------------
##price sensitivity

#sensitivity_phase <- "GAS"
#flat_price <- 40

##user inputs
sensitivity_phase
flat_price

price <- data.frame()
##generate price file to use for sensitivity analysis
if(sensitivity_phase == "OIL")
{
  price <- cbind(gPrice = flat_price, 
                 oPrice = seq(10, 100, by = 10), 
                 nPrice = oPrice *0.4)
}else{

  price <- cbind(gPrice = seq(1, 5.5, by = 0.5), 
                 oPrice = flat_price, 
                 nPrice = oPrice *0.4)
}



CashFlow.Price <- data.frame()
for(i in seq_along(tc_name)){
  for(j in 1:nrow(price))
  {
    cf_price <- cshflow(i, price, j)
    cf_price$scenario <- paste(price[j,1], '-' ,price[j,2], '-' , price[j,3])
    cf_price$price <- ifelse(sensitivity_phase == "OIL", price[j,2], price[j, 1]) 
    CashFlow.Price <- rbind(CashFlow.Price , cf_price)
    
  }
}

##reduce the table

CashFlow.Price <- CashFlow.Price %>% 
  group_by(TC_Group, scenario) %>% 
  summarise(NPV = max(NetCumDiscCF), Price = mean(price)) %>%
  arrange(TC_Group, Price)

#------------------------------------------------
##discount rate sensitivity to calc IRR

#create a discount rate file
disc_rate_tbl <- seq(0, 1, by = 0.05) 


cshflow_discount <- function(x, z) 
{ 
  red_tc <- subset(CashFlow, TC_Group == tc_list[x]) #reduce/filter the typecurves one at a time 
  #red_inputs <- subset(inputs, wellname == tc.list[x]) #reduce/filter the user inputs to match the typecurves
  Time <-  as.numeric(red_tc$Time); 
  NetOpIncome <- red_tc$NetOpIncome;
  NetDiscCF <- as.numeric(red_tc$NetOpIncome *(1/(1 + disc_rate_tbl[z])^((Time - 0.5) / 12)) - red_tc$NetDiscCapex);
  
  results <- data.frame(TC_Group = red_tc$TC_Group, Time, NetDiscCF)
  
  results_cshflow <- filter(results, NetOpIncome > 0)
  
  
  return(results_cshflow)
}


CashFlow.Disc <- data.frame()
for(i in seq_along(tc_name)){
  for(j in seq_along(disc_rate_tbl))
  {
    cf_dr <- cshflow_discount(i, j)
    cf_dr$Disc_Rate <- paste(as.numeric(disc_rate_tbl[j]))
    CashFlow.Disc <- rbind(CashFlow.Disc, cf_dr)
    
  }
}

CashFlow.Disc <- CashFlow.Disc %>%
  mutate(Disc_Rate = as.numeric(Disc_Rate)) %>%
  group_by(TC_Group, Disc_Rate) %>%
  summarise(NPV = sum(NetDiscCF))

#n <-2

CF_Metrics <- function(n){
  
  irr_sub <- subset(CashFlow.Disc, TC_Group == tc_list[n]); #subset to link colnames with tc name
  IRR <- ifelse(irr_sub[21,3] > 0, 1, 
                unlist(approx(irr_sub$NPV, irr_sub$Disc_Rate, 0))[2]); #approx fun iterates to find the 0 value
  colnames(IRR) <- "IRR" ;
  
  dpi_sub <- subset(CashFlow, TC_Group == tc_list[n]); #subset to link colnames with tc name
  DPI <- sum(dpi_sub$NetDiscCF, na.rm = TRUE) / sum(dpi_sub$NetDiscCapex, na.rm = TRUE) + 1; 
  Payout_disc <- unlist(approx(dpi_sub$NetCumDiscCF, dpi_sub$Time, 0))[2] / 12; #units = years
  NetEUR_MBOE <- sum(dpi_sub$NetBOE, na.rm = TRUE) / 1000;
  Capex_M <- max(dpi_sub$NetDiscCapex, na.rm = TRUE) / 1000;
  FnD <-  max(dpi_sub$NetDiscCapex, na.rm = TRUE) / sum(dpi_sub$NetBOE, na.rm = TRUE);
  Lifting_Cost <- sum(dpi_sub$NetOpex, na.rm = TRUE) / sum(dpi_sub$NetBOE, na.rm = TRUE);
  NPV15_M <- max(dpi_sub$NetCumDiscCF, na.rm = TRUE) / 1000;
  
  
  brkEven_sub <- subset(CashFlow.Price, TC_Group == tc_list[n]); #subset to link colnames with tc name
  BrkEven <- unlist(approx(brkEven_sub$NPV, brkEven_sub$Price, 0)[2]);
  
  
  
  
  metrics <- data.frame(NetEUR_MBOE, Capex_M, NPV15_M, IRR, DPI, BrkEven, Payout_disc, FnD, Lifting_Cost)
  
  return(metrics)
}


Econ.Metrics <- data.frame(TC_Group = tc_name, map_df(seq_along(tc_list), CF_Metrics))


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/cashflow.RData',sep=''), RFormat=T )))
