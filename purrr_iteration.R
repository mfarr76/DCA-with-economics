
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
library(dplyr)
library(purrr)
#gPrice <- c(2, 3, 4, 5, 6)
gPrice <- seq(1, 5, by = 0.5)
#oPrice <- c(20, 30, 40, 50, 60)
oPrice <- seq(30, 70, by = 5)
#nPrice <- c(5, 10, 20, 30, 40)
nPrice <- oPrice *0.4
price <- cbind(gPrice, oPrice, nPrice)

gas_price[1]

##------------------------------------

###unique well inputs will have the same order...nglyield, shrink, wi, nri, capex, opex 



cashflow_sensitivity <- function(x)
{
  x <- data.frame(Sensitivity = (cf_model$NetDryGas * gPrice[x]
                         + cf_model$NetOil * oPrice[x] + cf_model$NetNGL * nPrice[x]))
  x$Name <- cf_model$TCName
  
  return(x)
}

cf_sens(3)


purrr_out <- data.frame()
for(i in 1:length(wellnames)){
  
  cf_reduced <- subset(cf_model %>% select(TCName, Time, NetDryGas, NetOil, NetNGL),
                     TCName == wellnames[i])
  inputs_reduced <- subset(inputs %>% select(Name, capex, opex),
                           Name == wellnames[i])
  
  
  purrrModel <- data.frame(map(1:9, cashflow_cashflow_sensitivity(inputs_reduced)))
  purrr_out <- cbind(purrr_out, purrrModel)
  
}



purrrModel <- data.frame(map(1:9, cf_sens))

CFmodel <- bind_cols(NCF.tbl, purrrModel)

#----------------------------------------------
###F11 multiwell cashflow calcualtions


rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
#install.packages("purrr")
#library(data.table)
library(dplyr)
library(purrr)

TCGroups %>%
  group_by(Name) %>%
  summarise(max(cumOil.mbo))



gas_price <- 3
oil_price <- 60
ngl_price <- oil_price * 0.4
#shrink <- .8
#ngl_yield <- 100 #bbl/mmcf

#capex <- 5000
capex_mnth <- 1
#opex <- 1000
#work_int <- 1
#NRI <- .75
discRateSingle <- 0.15


wellnames <- unique(TCGroups$Name)
well1 <- data.frame(Name = wellnames[1], ngl.yield = 100, shrink = 0.8, 
                    wi = 1, nri = 0.75, capex = 5000, opex = 1000, discount.rate = 0.15 ,stringsAsFactors = FALSE)
well2 <- data.frame(Name = wellnames[2], ngl.yield = 1, shrink = 1, 
                    wi = 1, nri = 1, capex = 10, opex = 10, discount.rate = 0.15, stringsAsFactors = FALSE)
inputs <- rbind(well1, well2)




cshflow <- function(tc, inputs)
{
  TCName = tc$Name
  Time = as.numeric(tc$Time)
  GRGas.mcf = as.numeric(tc$Gas.mcf)
  GROil.bbl = as.numeric(tc$Oil.bbl)
  GRNgl.bbl = as.numeric(GRGas.mcf/1000 * inputs$nglyield)
  NetDryGas = as.numeric(GRGas.mcf * inputs$shrink * inputs$nri * inputs$wi)
  NetOil = as.numeric(GROil.bbl * inputs$nri * inputs$wi)
  NetNGL = as.numeric(GRNgl.bbl * inputs$nri * inputs$wi)
  GasRev = NetDryGas * gas_price
  OilRev = NetOil * oil_price
  NGLRev = NetNGL * ngl_price
  NetRev = GasRev + OilRev + NGLRev
  OpIncome = NetRev - inputs$opex
  Undisc.CF = OpIncome - ifelse(Time == capex_mnth, inputs$capex * 1000, 0)
  Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*inputs$capex*1000, 0))
  Disc.CF = as.numeric(OpIncome *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, NetDryGas, NetOil, NetNGL,
                        GasRev, OilRev, NGLRev, NetRev, OpIncome, Undisc.CF, Disc.Capex, Disc.CF)
  
  results <- filter(results, OpIncome > 0)
  
  return(results)
}


#xcash <- cshflow(TCGroups, inputs)



#-------------------for loop to calc F11 output for all TC wells
cf_model <- data.frame()
for(i in 1:length(wellnames))
{
  for_in_tc <- inputs[i, 1:ncol(inputs)] ##for_in will filter the input table to for the i row
  for_sub_tc <- subset(TCGroups, Name == wellnames[i]) ##subset to only the rows with wellname[i]
  
  model <- cshflow(for_sub_tc, for_in_tc) ##run cshflow function on the above
  cf_model <- rbind(cf_model, model) ##store the results
  
}

#-------------------for loop to calc price sensitivity


price_model <- data.frame()
for(i in 1:length(wellnames))
{
  
  x <- data.frame(map(1:9, function(x)data.frame(Name = cf_model$TCName, 
                                            Net.Rev = (cf_model$NetDryGas * gPrice[x] 
                                                           + cf_model$NetOil * oPrice[x] 
                                                           + cf_model$NetNGL * nPrice[x]))))
  
  
  for_in_price <- inputs[i, 1:ncol(inputs)] ##for_in will filter the input table to for the i row
  for_sub_price <- subset(x, Name == wellnames[i]) ##subset to only the rows with wellname[i]
  
  x1 <- data.frame(map(1:9, function(x)data.frame(Name = cf_model$TCName, 
                                                 Net.Rev = (cf_model$NetDryGas * gPrice[x] 
                                                            + cf_model$NetOil * oPrice[x] 
                                                            + cf_model$NetNGL * nPrice[x]))))
  
  model1 <- 
    ###update with new function to calc from rev to end....(for_sub, for_in) ##run cshflow function on the above
  price_model <- rbind(price_model, model1) ##store the results
  
}





test <- cf_model %>%
  split(.$TCName) %>%
  map("GRGas.mcf")


write.csv(TCGroups, "tcgroups.csv")
write.csv(Cashflow, "cashflow.csv")
write.csv(result, "result.csv")
write.csv(results, "results.csv")
write.csv(cf_model, "cf_model.csv")
write.csv(purrrModel, "purrr.csv")
write.csv(test, "test.csv")


