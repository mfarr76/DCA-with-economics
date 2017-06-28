rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")


library(dplyr, warn.conflicts = FALSE)


#rename average table
tc_table <- Average 

##use pPhase_docProp to allow the user to select OIL (1) or GAS (2) as the primary phase
tc_table$pPhase <- if(og_select == 1){Average$Oil} else {Average$Gas}


##filter pPhases to first 12 months in order to find the IP
tc_table$Prod.IP <- ifelse(tc_table$Months < 12, tc_table$pPhase, NA)
##find the IP from the Prop.IP column
qi <- max(tc_table$Prod.IP, na.rm = TRUE)





b <- 1.1
Di <- 76
Dmin <- 8
Years <- 30
Time1 <- 5
Yield1 <- 100
Yield2 <- 1000
Yield3 <- 2000
abRate <- 150




Di <- Di/100
Dmin <- Dmin/100

mnth_day_select <- 1
time_units <- ifelse(mnth_day_select == 1, 12, 365)
#t.units <- ifelse(TimeUnits == "Months", 12, 365) #change time units days/months
total_time <- Years * time_units #input from user on years * units
time.to.peak <- tc_table$Months[which.max(tc_table$Prod.IP)]



time_wBU <- total_time - time.to.peak
prod_time1 <- seq_along(1:time_wBU)
prod_time2 <- prod_time1 - 1
a.yr <- (1 / b) * ((1 / (1 - Di))^b - 1) #nominal deline in years
time_trans <- ceiling(( a.yr / ( -log (1 - Dmin)) - 1)/( a.yr * b) * time_units) #time to transition to Dmin



DCA <- function(qi, Di, b, Dmin, Years)
{
  if(qi == 0)
{
  TypeCurve <- data.frame(Time = c(0), 
                          qProd = c(0),
                          NpProd = c(0),
                          Yield = c(0))
}else{
    TypeCurve <- if(b == 0)
    { #exponential decline
      ai <- -log(1-Di)/time_units
      Exp.Np1 <- qi / ai * (1 - exp(-ai * prod_time1))
      Exp.Np2 <- qi / ai * (1 - exp(-ai * prod_time2))
      
      Exp.Np1 - Exp.Np2
      
      
    }else if(b == 1){ #harmonic decline
      
      ai <- (Di / (1 - Di) / time_units)
      Har.Np1 <- qi / ai * log(1 + ai * prod_time1)
      Har.Np2 <- qi / ai * log(1 + ai * prod_time2)
      
      Har.Np1 - Har.Np2
      
      
    }else{
      
      ###############Hyperbolic - b value is not 0 or 1
      #determine parameters for Dmin
      ai <- (1 / (time_units * b))*((1 - Di)^- b-1) #nominal deline per time units 
      #a.yr <- (1 / b) * ((1 / (1 - Di))^b - 1) #nominal deline in years
      #t.trans <- ceiling(( a.yr / ( -log (1 - Dmin)) - 1)/( a.yr * b) * t.units) #time to reach Dmin
      t_trans_seq1 <- seq_along(1:time_trans)
      t_trans_seq2 <- t_trans_seq1 - 1
      
      ###########forecast to Dmin################
      Hyp.Np.toDmin1 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t_trans_seq1)^((1 - b) / b))))
      Hyp.Np.toDmin2 <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b *  t_trans_seq2)^((1 - b) / b))))
      Hyp.NpDmin <- Hyp.Np.toDmin1  - Hyp.Np.toDmin2
      
      ##########forecast expontintal to abandonment rate (Terminal decline portion of the curve)#########
      aDmin <- -log(1 - Dmin)/time_units
      q.trans <- qi / ((1 + b * ai * time_trans))^(1 / b) #rate at transition month 
      Np.trans <- (qi / (( 1 - b) * ai)) * (1-(1/((1 + ai * b * time_trans)^((1 - b) / b)))) #cum volume at tranistion month
      time_ab1 <- seq_along(1:(time_wBU - time_trans))
      time_ab2 <- 0:(time_wBU - time_trans - 1)
      
      #########forecast from Dmin to end of forecast
      Exp.Np1 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * time_ab1))
      Exp.Np2 <- Np.trans + q.trans / aDmin * (1 - exp(-aDmin * time_ab2))
      Exp.Np <- Exp.Np1 - Exp.Np2
      
      append(Hyp.NpDmin, Exp.Np)
      
    }
    rate.BU <- tc_table$Prod.IP[1:time.to.peak]

    #ifelse(append(rate.BU, TypeCurve) > abRate, append(rate.BU, TypeCurve), NA)
    append(rate.BU, TypeCurve)
    
  }
  
}

TypeCurve <- data.frame(Time = seq(1:total_time), 
                          Primary = DCA(qi, Di, b, Dmin, Years))



                       
TypeCurve <- left_join(TypeCurve, YieldForecast, by = c("Time")) %>%
  filter(Primary >= abRate) %>%
  mutate(Gas.mcf = if(og_select == 1){Primary/1000 * Secondary}else{Primary}, #mcf
         Oil.bbl = if(og_select == 1){Primary}else{Primary * Secondary/1000}, #bbl
         Ratio = Secondary,
         cumGas.mmcf = cumsum(Gas.mcf)/1000, #mmcf
         cumOil.mbo = cumsum(Oil.bbl)/1000) %>% #mbo 
  select(Time, 
         Gas.mcf, 
         Oil.bbl, 
         Ratio, 
         cumGas.mmcf, 
         cumOil.mbo)


TypeCurve <- TypeCurve %>%
  filter(Primary >= abRate) %>%
  mutate(Gas.mcf = if(og_select == 1){Primary/1000 * Secondary}else{Primary}, #mcf
         Oil.bbl = if(og_select == 1){Primary}else{Primary * Secondary/1000}, #bbl
         Ratio = Secondary,
         cumGas.mmcf = cumsum(Gas)/1000, #mmcf
         cumOil.mbo = cumsum(Oil)/1000) %>% #mbo 
  select(Time, 
         Gas.mcf, 
         Oil.bbl, 
         Ratio, 
         cumGas.mmcf, 
         cumOil.mbo)




forecast.table <- TypeCurve %>%
    mutate(CumGas12MO.mmcf = cumGas.mmcf, #mmcf
           CumOil12MO.mbo = cumOil.mbo, #mbo
           GasEUR.mmcf = sum(Gas.mcf, na.rm = TRUE)/1000, #mmcf
           OilEUR.mbo = sum(Oil.bbl, na.rm = TRUE)/1000, #mbo
           TotalEUR.mboe = GasEUR.mmcf/6 + OilEUR.mbo) %>% #mboe
    filter(Time == 12) %>%
    select(CumGas12MO.mmcf,
           CumOil12MO.mbo,
           GasEUR.mmcf, 
           OilEUR.mbo, 
           TotalEUR.mboe)
forecast.table


library(ggplot2)
ggplot(TypeCurve, aes(x = Time, y = NpProd)) + geom_point()

write.csv(TypeCurve, 'Typecurve.csv')

save(list=ls(), file='C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData', RFormat=TRUE)
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/DCAwBU.RData',sep=''), RFormat=T )))
