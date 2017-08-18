rm(list = ls())
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.daily.AT.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.monthly.AT.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
#####not to load in spotfire
og_select <- 1
prod_tbl <- read.csv("IHS_PROD.csv")

min_lat <- 1000
normal_lat <- 5000
####
WellId <- prod_tbl$Entity
EffLat <- prod_tbl$PerfIntervalGross
Time <- prod_tbl$c.Production.Date
Oil <- prod_tbl$Liquid
Gas <- prod_tbl$Gas
input <- data.frame(WellId, Time, Oil, Gas, EffLat) #create Average table 


##Michael Farr
#This script will average oil & gas rates based on the primary phase that is selected by the user

#----------------------------------------------------------------------------------

##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)


##"og_select" document property control allows the user to select OIL (5) or GAS (2) as the primary phase
##the number convention will be used in the dca portion of the workflow
pPhase <- ifelse(og_select == 5, "Oil", "Gas") 

##filter and rename the production table
input <- prod_tbl %>%
  ##need to ensure Time is time/data units for consistency
  mutate(Time = as.POSIXct(as.Date(Time , "%m/%d/%Y"), 
                           origin = "1970-01-01", tz="UTC")) %>%
  select(WellId = Entity,
         Time,
         Oil = Liquid, 
         Gas = Gas, 
         EffLat = PerfIntervalGross)

##based upon the pPhase, filter out zero months and minimum lateral lengths
if(pPhase == "Oil")
{
  input <- filter(input, Oil > 0 & EffLat > min_lat)
}else{
  input <- filter(input, Gas > 0 & EffLat > min_lat)
}



input1 <- input %>%
  arrange(WellId, Time) %>%
  group_by(WellId) %>%
  mutate(RowCount = 1, #create a column for wellcount by placeing a 1 in every row
         Months = cumsum(RowCount), 
         Oil1 = Oil / EffLat * normal_lat, #normalized to effective lateral 
         Gas1 = Gas / EffLat * normal_lat, #normalized to effective lateral
         CUMOil1 = cumsum(ifelse(is.na(Oil1),0,Oil1)),
         CUMGas1 = cumsum(ifelse(is.na(Gas1), 0, Gas1))) %>% 
  group_by(Months) %>%
  summarise(Oil = mean(Oil1[Oil1 > 0]))

write.csv(input1, file = "input1.csv")
write.csv(AVERAGE.MONTHLY, file = "average.csv")

 ####create table called Average to house the data used for DCA
if(nrow(input) < 1)
{#in no wells are selected, create an Average table with zeros 
  Average.Monthly <- data.frame(WellName = c("None"), Time = c(Sys.time()), 
                        Oil = c(0), Gas = c(0), EffLat = c(0), Months = c(0), WellCount = c(0), 
                        CUMGas = c(0), CUMOil = c(0))
}else{
  
  ##################dplyr package used for data wrangling
 AVERAGE.MONTHLY <- input %>%
    arrange(WellId, 
            Time) %>%
    group_by(WellId) %>%
    mutate(RowCount = 1, #create a column for wellcount by placeing a 1 in every row
           Months = cumsum(RowCount), 
           Oil1 = Oil / EffLat * normal_lat, #normalized to effective lateral 
           Gas1 = Gas / EffLat * normal_lat, #normalized to effective lateral
           CUMOil1 = cumsum(Oil1),
           CUMGas1 = cumsum(Gas1)) %>% 
    group_by(Months) %>%
    summarise(Gas = mean(Gas1[Gas1 > 0]), #mcf
              GasP10 = quantile(Gas1[Gas1 > 0], p = 0.90), #mcf
              GasP90 = quantile(Gas1[Gas1 > 0], p = 0.10), #mcf
              Oil = mean(Oil1[Oil1 > 0]), #bbl
              OilP10 = quantile(Oil1[Oil1 > 0], p = 0.90), #bbl 
              OilP90 = quantile(Oil1[Oil1 > 0], p = 0.10), #bbl
              CUMGas = mean(CUMGas1[CUMGas1 > 0]) / 1000, #mmcf
              CUMGasP10 = quantile(CUMGas1[CUMGas1 > 0], p = 0.90) / 1000, #mmcf
              CUMGASP90 = quantile(CUMGas1[CUMGas1 > 0], p = 0.10) / 1000, #mmcf
              CUMOil = mean(CUMOil1[CUMOil1 > 0]) / 1000, #mbo
              CUMOilP10 = quantile(CUMOil1[CUMOil1 > 0], p = 0.90) / 1000, #mbo,
              CUMOilP90 = quantile(CUMOil1[CUMOil1 > 0], p = 0.10) / 1000, #mbo,
              Yield = Oil / (Gas / 1000), #bbl/mmcf
              YieldP10 = OilP10 / (GasP10 / 1000), #bbl/mmcf 
              YieldP90 = OilP90 / (GasP90 / 1000), #bbl/mmcf
              GOR = Gas / (na_if(Oil,0) / 1000),  #scf/bbl
              GORP10 = GasP10 / (na_if(OilP10, 0) / 1000), #scf/bbl 
              GORP90 = GasP90 / (na_if(OilP90,0) / 1000), #scf/bbl
              WellCount = sum(RowCount)) %>%  #sum up RowCount which will give you a wellcount column
    filter(WellCount > minWellcount)
 
  #######################
  
}

summarise(AVERAGE.MONTHLY, mean(Oil>0))

mean(AVERAGE.MONTHLY$Oil > 0)
mean(AVERAGE.MONTHLY)


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/average.RData',sep=''), RFormat=T )))
