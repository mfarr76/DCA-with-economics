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



gas * gPrice[1] + oil * oPrice[1]

cfmodel <- function(x){
  cf <- data.frame(run = gas * gPrice[x] + oil * oPrice[x])
  return(cf)
}

cfmodel(1)

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

