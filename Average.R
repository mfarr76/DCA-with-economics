rm(list = ls())

##load spotfire data===================================================================

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.daily.AT.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/avg.mnth.at.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/average.RData")
#####not to load in spotfire
og_select <- 1
prod_tbl <- read.csv("IHS_PROD.csv")

min_lat <- 1000
normal_lat <- 5000
t_select <- 1

write.csv(prod_tbl, file = "prod_tbl.csv")
####
WellId <- prod_tbl$Entity
EffLat <- prod_tbl$PerfIntervalGross
Time <- prod_tbl$c.Production.Date
Oil <- prod_tbl$Liquid
Gas <- prod_tbl$Gas
input <- data.frame(WellId, Time, Oil, Gas, EffLat) #create Average table


##Michael Farr
#This script will average oil & gas rates and normalized based on the primary phase that is selected by the user

##script begins=========================================================================

##install package if it is not already installed========================================
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package==========================================================================
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

pkgs <- list(search = search(), loadedNamespaces = loadedNamespaces())
dump("pkgs", file="C:/Users/MFARR/Documents/R_files/Spotfire.data/debugPackages.txt")

##property controls...user can change these properties==================================
prod_tbl #production table
og_select #oil/gas selection as primary phase
normal_lat #normalized lateral length
min_lat #minimum lateral length
t_select <- 1 #set time units to months

##"og_select" document property control allows the user to select OIL (1) or GAS (2) as the primary phase
#pPhase <- ifelse(og_select == 5, "Oil", "Gas") 
t_units <- ifelse(t_select == 1, 1, 365/12)

##filter and rename the production table================================================

##use ihs prod tbl for the code below===================================================
input <- prod_tbl %>%
  ##need to ensure Time is time/data units for consistency
  mutate(Time = as.POSIXct(as.Date(ProductionDate , "%m/%d/%Y"), 
                           origin = "1970-01-01", tz = "UTC"), 
         Liquid = as.numeric( Liquid / t_units ), 
         Gas = as.numeric( Gas / t_units )) %>%
  select(WellId = Entity,
         Time,
         Oil = Liquid, 
         Gas, 
         EffLat = PerfIntervalGross)
##end of ihs prod tbl=====================================================================

##use ariesMaster monthly (AC_PRODUCT prod tbl for the code below=========================
input <- prod_tbl %>%
  mutate(Time = as.POSIXct(as.Date(P_DATE , "%m/%d/%Y"),           #convert P_DATE to consistant units
                           origin = "1970-01-01", tz="UTC")) %>%
  select(WellId = PROPNUM,
         Time,
         Oil = OIL, 
         Gas = GAS, 
         EffLat = EFF_LAT)

##end of ariesMaster prod tbl===============================================================


##based upon the pPhase, filter out zero months and minimum lateral lengths
if(og_select == 6)
{
  input <- filter(input, Oil > 0 & EffLat > min_lat)
}else{
  input <- filter(input, Gas > 0 & EffLat > min_lat)
}


##create average table===================================================================
if(nrow(input) == 0)
{#in no wells are selected, create an Average table with zeros 
  AVERAGE.MONTHLY <- data.frame(WellName = c("None"), Time = Sys.time(),
                                EffLat = 0, Months = 0, WellCount = 0,
                                Gas_avg = 0, Gas_p10 = 0, Gas_p50 = 0, Gas_p90 = 0, 
                                Oil_avg = 0, Oil_p10 = 0, Oil_p50 = 0, Oil_p90 = 0, 
                                CUMGas_avg = 0, CUMGas_p10 = 0, CUMGas_p50 = 0, CUMGas_P90 = 0, 
                                CUMOil_avg = 0, CUMOil_p10 = 0, CUMOil_p50 = 0, CUMOil_p90 = 0, 
                                Yield_avg = 0, GOR_avg = 0)
}else{
  
  ##################dplyr package used for data wrangling####################
  AVERAGE.MONTHLY <- input %>%
    arrange(WellId, Time) %>%
    group_by(WellId) %>%
    mutate(RowCount = 1, #create a column for wellcount by placeing a 1 in every row
           Months = cumsum(RowCount), 
           Oil1 = Oil / EffLat * normal_lat, #normalized to effective lateral 
           Gas1 = Gas / EffLat * normal_lat,
           Yield1 = ifelse( !is.na(Oil) & !is.na(Gas), Oil / ( Gas / 1000 ), NA), #bbl/mmcf
           GOR1 = ifelse( !is.na(Oil) & !is.na(Gas), ( Gas * 1000 ) / Oil, NA), #scf/bbl
           CUMOil1 = cumsum(ifelse( !is.na(Oil1), Oil1, 0 )),
           CUMGas1 = cumsum(ifelse( !is.na(Gas1), Gas1, 0 ))) %>% 
    group_by( Months ) %>%
    summarise(Gas_avg = mean(Gas1, na.rm = TRUE), #mcf
              Gas_p10 = quantile(Gas1, p = 0.90, na.rm = TRUE), #mcf
              Gas_p50 = median(Gas1, na.rm = TRUE), #mcf
              Gas_p90 = quantile(Gas1, p = 0.10, na.rm = TRUE), #mcf
              Oil_avg = mean(Oil1, na.rm = TRUE), #bbl
              Oil_p10 = quantile(Oil1, p = 0.90, na.rm = TRUE), #bbl
              Oil_p50 = median(Oil1, na.rm = TRUE), #bbl
              Oil_p90 = quantile(Oil1, p = 0.10, na.rm = TRUE), #bbl
              CUMGas_avg = mean(CUMGas1) / 1000, #mmcf
              CUMGas_p10 = quantile(CUMGas1, p = 0.90) / 1000, #mmcf
              CUMGas_p50 = median(CUMGas1) / 1000, #mmcf
              CUMGas_p90 = quantile(CUMGas1, p = 0.10) / 1000, #mmcf
              CUMOil_avg = mean(CUMOil1) / 1000, #mbo
              CUMOil_p10 = quantile(CUMOil1, p = 0.90) / 1000, #mbo,
              CUMOil_p50 = median(CUMOil1) / 1000, #mbo
              CUMOil_p90 = quantile(CUMOil1, p = 0.10) / 1000, #mbo,
              Yield_avg = mean(Yield1, na.rm = TRUE), #bbl/mmcf
              #YieldP_10 = quantile(Yield1, p = 0.90, na.rm = TRUE), #bbl/mmcf 
              #Yield_p50 = median(Yield1, na.rm = TRUE), #bbl/mmcf 
              #Yield_p90 = quantile(Yield1, p = 0.10, na.rm = TRUE), #bbl , #bbl/mmcf
              GOR_avg = mean(GOR1, na.rm = TRUE),  #scf/bbl
              #GOR_p10 = quantile(GOR1, p = 0.90, na.rm = TRUE), #scf/bbl 
              #GOR_p50 = median(GOR1, na.rm = TRUE),  #scf/bbl
              #GOR_p90 = quantile(GOR1, p = 0.10, na.rm = TRUE), #scf/bbl
              WellCount = sum(RowCount)) %>%  #sum up RowCount which will give you a wellcount column
    filter(WellCount > minWellcount) %>%
    mutate_if(is.integer, as.numeric)
}

##create a Rdata file to load in R========================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/average.RData',sep=''), RFormat=T )))


##end script==============================================================================