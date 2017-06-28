
#rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
#install.packages("purrr")
#library(data.table)
library(purrr)


gas_price <- 3
oil_price <- 60
ngl_price <- oil_price * 0.4
shrink <- .8
ngl_yield <- 100 #bbl/mmcf

capex <- 5000
capex_mnth <- 1
opex <- 1000
work_int <- 1
NRI <- .75
discRateSingle <- 0.15



##economics start on 01/01/2017
#time_series <- seq(as.Date("2017/01/01"), by = "month", length.out = 360)


Cashflow <- TypeCurve %>%
  mutate(
    gr_wet_gas.mcf = Gas.mcf, #mcf
    gr_oil.bbl = Oil.bbl, #bbl
    gr_dry_gas.mcf = Gas.mcf * shrink, #shrink gas
    gr_ngl.bbl = Gas.mcf / 1000 * ngl_yield, #bbl
    net_gas.mcf = Gas.mcf * NRI * work_int, #mcf
    net_oil.bbl = Oil.bbl * NRI * work_int, #bbl
    net_ngl.bbl = gr_ngl.bbl * NRI * work_int, #bbl
    net_gas_rev =  net_gas.mcf * gas_price, #units $
    net_oil_rev = net_oil.bbl * oil_price, #units $
    net_ngl_rev = net_ngl.bbl * ngl_price, #units $
    net_rev = net_gas_rev + net_oil_rev + net_ngl_rev, #units $
    oper_income = net_rev - opex, #units $
    undisc_cash_flow = oper_income - ifelse(Time == capex_mnth, capex * 1000, 0),
    disc_capex = ifelse(Time == capex_mnth, (1/(1 + discRateSingle)^((capex_mnth - 1)/12))*capex*1000, 0), 
    disc_cash_flow = oper_income *(1/(1 + discRateSingle)^((Time - 0.5) / 12)) - disc_capex) %>%
  filter(oper_income >= 0) %>%
  select(Time, gr_wet_gas.mcf, gr_oil.bbl, 
         gr_dry_gas.mcf, gr_ngl.bbl, net_gas.mcf, 
         net_oil.bbl, net_ngl.bbl, net_gas_rev, 
         net_oil_rev, net_ngl_rev, net_rev, 
         oper_income, undisc_cash_flow, disc_capex, disc_cash_flow)




cf <- function(prodMonth, capex, capexMonth, operIncome)
{
  #change units for capex
  capexM <- capex*1000
  #create discount rate range from 0 to 100%
  discRate <- seq(0, 1, by = 0.05) 
  #sequental list of discRate 0 to 21
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
  discCfSum <- map_df(discCfTbl, sum, na.rm = TRUE)
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




####inerpolate
IRR <- ifelse(as.numeric(NPV.Table[21,2]) > 0, 1,unlist(approx(NPV.Table$NPV, NPV.Table$Disc.Rate, 0))[2])
IRR

DPI <- sum(Cashflow$disc_cash_flow, na.rm = TRUE)/sum(Cashflow$disc_capex, na.rm = TRUE) + 1
DPI

mod <- lm(formula = Disc.Rate ~ as.vector(NPV), data = NPV.Table)

new_data <- data.frame(NPV = 0)
predict(mod, newdata = new_data)

#################STOP RUNNNING
write.csv(NPV.Table, file = "npvTable.csv")
#write.csv(cash, file = "cash.csv")
#write.csv(cashflow, file = "cshflw.csv")
#write.csv(disc_cash_flow, file = "dcf.csv")




TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/cashflowRData.RData',sep=''), RFormat=T )))

