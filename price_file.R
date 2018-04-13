rm(list = ls())
library(dplyr, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)
library(purrr)

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/loop_tbl.RData")
#rm(adapt, production, wellcomp, wellspacing)


d_sidefile <- sidefile %>%
  filter(FILENAME == "CORPCALL")

cc_list <- strsplit(d_sidefile$EXPRESSION, " ")

price <- data.frame()
for(i in 2:16){
  cc <- data.frame(price_file = paste(cc_list[[1]], collapse = ""),
    product = ifelse(i < 7, "Gas",
                                    ifelse(i >= 6 & i < 12,"Oil", "Ngl")), 
                   price = as.numeric(cc_list[[i]][1]), 
                   units = cc_list[[i]][3],
                   year = 1)
  price <- rbind(price, cc)
}

price <- price %>%
  group_by(product) %>%
  mutate(year = cumsum(year))

county <- "WEBB"

lookup <- data.frame(arlookup[arlookup$NAME == "OVERLAYS_Q118CC" & arlookup$Var0 == "AREAB2", ])
misc <- data.frame(arlookup[arlookup$NAME == "MISC_Q118CC" & arlookup$Var0 == "AREAB2", ])
exp <- data.frame(arlookup[arlookup$NAME == "EXP_TRANS_Q118CC" & arlookup$Var0 == "AREAB2", ])
opef <- data.frame(arlookup[arlookup$NAME == "OPEF_TRANDIF2_CCQ118" & arlookup$Var0 == "AREAB2", ])
advt <- data.frame(arlookup[arlookup$NAME == "ADVT" & arlookup$Var0 == county, ])

lookup[-c(1:5)] <- sapply(lookup[-c(1:5)], as.numeric)
misc[-c(1:5)] <- sapply(misc[-c(1:5)], as.numeric)
exp[-c(1:5)] <- sapply(exp[-c(1:5)], as.numeric)
opef[-c(1:5)] <- sapply(opef[-c(1:5)], as.numeric)
advt[-c(1:5)] <- sapply(advt[-c(1:5)], as.numeric)

#sapply(lookup, class)
#sapply(opef, class)
#table(unlist(lapply(price, class)))
#sapply(df, class)
#table(lapply(price, class))
#row.names(lookup) <- NULL


price$cc_decs <- "AREAB2"
price$diff_per <- NA
price$btu <- as.numeric(rep(lookup[6], nrow(price)))
price$wh_price_adj <- as.numeric(rep(lookup[25], nrow(price)))
price$basis_fee_dry <- as.numeric(rep(lookup[7], nrow(price)))
price$basis_fee_ngl <- as.numeric(rep(lookup[8], nrow(price)))


for(i in seq_len(nrow(price))){
  
  price$diff_per[i] <- as.numeric(lookup[8+i])
  
}

price <- price %>%
  rowwise() %>%
  mutate(NetPrice = if_else(product == "Gas", price * basis_fee_dry * diff_per * wh_price_adj * btu, 
                            if_else(product == "Ngl", price * basis_fee_ngl * diff_per, 
                            price * diff_per)))
         

(opex <- prop_tbl %>%
  filter(RSV_CAT == "1.3PUD") %>%
  group_by(CORPCALL_DEC) %>%
  summarise(loe = mean(LOE, na.rm = TRUE), 
            woe = mean(WOE, na.rm = TRUE), 
            flovhd = mean(FLOVHD, na.rm = TRUE), 
            copas = mean(COPAS, na.rm = TRUE), 
            sev_gas = mean(GAS_SEV_RATE, na.rm = TRUE) / 100, 
            sev_oil = mean(OIL_SEV_RATE, na.rm = TRUE) / 100, 
            sev_ngl = mean(NGL_SEV_RATE, na.rm = TRUE) / 100))


#write.csv(price, file = "price.csv")


