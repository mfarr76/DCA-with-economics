rm(list = ls())

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
gPrice[1]


mapModel <- data.frame(map(1:5, cfmodel))

cfmodel1 <- function(x){
  data.frame(Scenario = (Cashflow$net_gas.mcf * gPrice[x]
                         + Cashflow$net_oil.bbl * oPrice[x] + Cashflow$net_ngl.bbl * nPrice)
             - opex)
}

cfmodel2 <- function(x){
  data.frame(Scenario = (NCF.tbl$NetDryGas * gPrice[x]
                         + NCF.tbl$NetOil * oPrice[x] + NCF.tbl$NetNGL * nPrice) 
             - opex)
}



cfmodel1(1)

(mapModel1 <- data.frame(map(1:4, cfmodel1)))
(mapModel2 <- data.frame(map(1:9, cfmodel2)))

(new.CFmodel <- bind_cols(NCF.tbl, mapModel2))
##------------------------------------

###unique well inputs will have the same order...nglyield, shrink, wi, nri, capex, opex 

Well1 <- c(100, 0.8, 1, 0.75, 5000, 1000)
well2 <- c(100, 0.8, 1, 0.75, 5000, 1000)


###cashflow sensitivity with purrr
NCF.tbl <- TypeCurve %>%
  mutate(Time = as.numeric(Time),
         NetDryGas = as.numeric(Gas.mcf * shrink * NRI * work_int),
         NetOil = as.numeric(Oil.bbl * NRI * work_int),
         NetNGL = as.numeric(Gas.mcf/1000 * ngl_yield * NRI * work_int)) %>%
  select(Time, NetDryGas, NetOil, NetNGL)


cfmodel <- function(x){
  data.frame(Scenario = (NCF.tbl$NetDryGas * gPrice[x]
                         + NCF.tbl$NetOil * oPrice[x] + NCF.tbl$NetNGL * nPrice) 
             - opex)
}

purrrModel <- data.frame(map(1:9, cfmodel))

CFmodel <- bind_cols(NCF.tbl, purrrModel)

#----------------------------------------------
###F11 multiwell cashflow calcualtions


rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
#install.packages("purrr")
#library(data.table)
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


well1 <- data.frame(Name = wellnames[1], nglyield = 100, shrink = 0.8, wi = 1, nri = 0.75, capex = 5000, opex = 1000, stringsAsFactors = FALSE)
well2 <- data.frame(Name = wellnames[2], nglyield = 1, shrink = 1, wi = 1, nri = 1, capex = 10, opex = 10, stringsAsFactors = FALSE)
inputs <- rbind(well1, well2)





out <- data.frame()
intake <- data.frame()
modelout<- data.frame()
for(i in 1:length(wellnames))
{ 
  
  intake <- inputs[i, 1:ncol(inputs)]
  
  
  out <- subset(TCGroups, Name == wellnames[i])
  
  
  model <- cshflw_redo(out, intake)
  modelout <- rbind(modelout, model)
  
}



tc <- TCGroups


cshflw_redo <- function(tc, inputs)
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
  OpIncome = ifelse(NetRev - inputs$opex < 0, 0 ,NetRev - inputs$opex)
  OpIncome1 = NetRev - inputs$opex
  Undisc.CF = OpIncome - ifelse(Time == capex_mnth, inputs$capex * 1000, 0)
  Disc.Capex = (ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*inputs$capex*1000, 0))
  Disc.CF = as.numeric(OpIncome *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - Disc.Capex)
  
  results <- data.frame(TCName, Time, GRGas.mcf, GROil.bbl, GRNgl.bbl, NetDryGas, NetOil, NetNGL, GasRev, OilRev, NGLRev, NetRev,OpIncome, OpIncome1, Undisc.CF, 
                        Disc.Capex, Disc.CF)
  #results <- filter(results, OpIncome > 0) ###REMEMBER TO TURN THIS BACK ON...OFF FOR TESTING
  
  #results$Scenario <- Scenario
  
  
  return(results)
}

cshflw_redo(TCGroups, inputs)




write.csv(TCGroups, "tcgroups.csv")
write.csv(Cashflow, "cashflow.csv")
write.csv(result, "result.csv")
write.csv(results, "results.csv")
write.csv(modelout, "modelout.csv")



