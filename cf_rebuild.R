rm(list = ls())

##load for testing==========================================================================
#load("C:/Users/MFARR/Documents/R_files/Spotfire_data/average_RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/loop_tbl.RData")
#forecast_parm <- read.csv("forecast_parm.csv", stringsAsFactors = FALSE)
#dca <- read.csv("aries_run.csv", stringsAsFactors = FALSE)
#names <- unique(forecast_parm$tcName)



##create tcName and price tbls==============================================================

#dt <- forecast_parm
names <- unique(dca$Name)

#num <- 1

##cf fxn====================================================================================

cshflow <- function(num) #x = num
{ df <- subset(dca, Name == names[num]);                                   #setup loop through tc
  cf_parm <- subset(inputs, tcName == names[num]);                  #setup loop through inputs
  
  #gross production 
  df$GrNgl_Bbl <- (df$GrGas_Mcf/1000 * lookup[[24]]);
  #df$GrBoe <- ((df$GrGas_Mcf * misc[[6]]) / 6 + (df$GrNgl_Bbl + df$GrOil_Bbl));
  #df$CumMboe <-  cumsum(df$GrBoe) / 1000;
  
  #net prod
  df$NetDryGas_Mcf <- (df$GrGas_Mcf * misc[[6]] * cf_parm$nri);
  df$NetOil_Bbl <- (df$GrOil_Bbl * cf_parm$nri);
  df$NetNGL_Bbl <- (df$GrNgl_Bbl * cf_parm$nri);
  #df$NetBOE <- (df$NetOil_Bbl + df$NetNGL_Bbl + df$NetDryGas_Mcf/6);
  
  #net revenue
  #revenue with 5yr price forecast
  gas <- price[price$product == "Gas",];
  oil <- price[price$product == "Oil",];
  ngl <- price[price$product == "Ngl",];
  
  #create a match of rows to years 1-5
  dca_match <- match(df$Year, 1:5);
  
  #product prices
  df$GasPrice <- gas$NetPrice[dca_match];
  df$OilPrice <- oil$NetPrice[dca_match];
  df$NglPrice <- ngl$NetPrice[dca_match];
  
  #net revenue
  df$NetGasRev <- df$NetDryGas_Mcf * df$GasPrice;
  df$NetOilRev <- df$NetOil_Bbl * df$OilPrice;
  df$NetNglRev <- df$NetNGL_Bbl * df$NglPrice;
  df$NetSales <- df$NetGasRev + df$NetOilRev + df$NetNglRev;
  
  #net expenses
  capex_mnth <- 1;
  df$gas_trans_factor <- ( df$NetGasRev /  cf_parm$nri ) * ( exp[[6]] / 100 ); #trans factor - percent of gas and ngl rev
  df$ngl_trans_factor <- ( df$NetNglRev / cf_parm$nri ) * ( exp[[6]] / 100 );
  df$trans_diff_dry <- exp[[7]] * ( df$GrGas_Mcf * misc[[7]] * misc[[6]] ); #trans diff - gross trans $/mmbtu
  df$oil_trans <- exp[[9]] * df$GrOil_Bbl;
  df$NetLoe <- opex[[3,2]] * cf_parm$wi;
  df$NetWoe <- opex[[3,3]] * cf_parm$wi;
  df$NetFlovhd <- opex[[3,4]] * cf_parm$wi
  df$NetSevTax <-  df$NetGasRev * opex[[3,6]] + df$NetOilRev * opex[[3,7]] + df$NetNglRev * opex[[3,8]];
  df$advt <- ( df$NetSales - df$NetSevTax ) * advt[[6]] / 100;
  df$NetTotalExp <- df$gas_trans_factor + df$ngl_trans_factor + df$trans_diff_dry + df$oil_trans + df$NetLoe + df$NetWoe + 
                    df$NetFlovhd + df$NetSevTax + df$advt;
  df$NetOpIncome <- df$NetSales - df$NetTotalExp;
  df$NetCapex <- ifelse(df$Time == capex_mnth, (cf_parm$capex * 1000 * cf_parm$wi), 0);
  
  #net cashflows
  df$NetUndiscCF <- df$NetOpIncome - df$NetCapex;
  df$NetDiscCapex <- 1 / (1 + cf_parm$discRate)^((capex_mnth - 1)/ 12) * df$NetCapex;
  df$NetDiscCF <- (df$NetOpIncome *(1/(1 + cf_parm$discRate)^((df$Time - 0.5) / 12)) - df$NetDiscCapex);
  df$NetCumDiscCF <- cumsum(df$NetDiscCF);
  
  #output <- filter(df, NetOpIncome > 0) #stop reporting when income does negative....LOSS ZERO
  output <- df
  
  
  return(output)
}


##cf loop==================================================================================


cf <- cshflow(1)

CashFlow <- data.frame()

for(i in seq_along(names)) #number of time to loop
{
  cf1 <- cshflow(i) #call cshflow function -- price file[row (i), column (i)]
  CashFlow <- rbind(CashFlow , cf1) #store the results of each loop
}



(eur <- CashFlow %>%
  group_by(Name) %>%
  summarise(MMcfe = max(CumMboe) * 6))


write.csv(cf, file = "cf.csv")






