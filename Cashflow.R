<<<<<<< HEAD
#rm(list = ls())

#install.packages("purrr")
#library(data.table)
library(purrr)


gas_price <- 0
oil_price <- 60

capex <- 5000
capex_mnth <- 1
opex <- 1000
NRI <- .75
discRateSingle <- 0.15
prodMonth <- Cashflow$prd_mnth
operIncome <- Cashflow$oper_income


TypeWell1 <- TypeWell %>%
              mutate(prd_mnth = Time, 
                     oil_prod = qProd, 
                     gas_prod = qProd) %>%
              select(prd_mnth, 
                     oil_prod, 
                     gas_prod)



Cashflow <- TypeWell1 %>%
          mutate(net_gas_rev = gas_prod * gas_price * NRI, #units $
                 net_oil_rev = oil_prod * oil_price * NRI, #units $
                 net_rev = net_gas_rev + net_oil_rev, #units $
                 oper_income = net_rev - opex, #units $
                 undisc_cash_flow = oper_income - 
                    ifelse(prd_mnth == capex_mnth, capex * 1000, 0),
                 ##duplicate undisc_cash_flow to start after capex month
                 ##used to filter negative cash flow out of data....LOSS ZERO
                 undisc_cash_flow1 = if_else(prd_mnth > (capex_mnth), undisc_cash_flow, 0),
                 #disc capex and cash flow
                 disc_capex = ifelse(prd_mnth == capex_mnth, (1/(1 + discRateSingle)^
                    ((capex_mnth - 1)/12))*capex*1000, 0), 
                 disc_cash_flow = oper_income *(1/(1 + discRateSingle)^
                    ((prd_mnth - 0.5) / 12)) - disc_capex) %>%
           filter(undisc_cash_flow1 >= 0)

#capexMonth <- capex_mnth

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


NPV.Table <- cf(Cashflow$prd_mnth, capex, capex_mnth, Cashflow$oper_income)

####inerpolate
IRR <- unlist(approx(NPV.Table$NPV, NPV.Table$Disc.Rate, 0))[2]
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
