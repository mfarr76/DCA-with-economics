
rm(list = ls())
library(dplyr, warn.conflicts = FALSE)

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin_Monthly_AT.RData")



##choose which data you want to load....need average data
#prod <- load("C:/Users/MFARR/Google Drive/r scripts/Spotfire/average.in.RData")
prod_tbl <- read.csv("C:/Users/MFARR/Google Drive/r scripts/Production/IHS.PROD.csv")



qi <- 5000
b <- 1.2
Di <- 70
Dmin <- 8
Years <- 10
TC.Name <- c("TEST")
TC.Spacing <- 300
TC.Lbs.Ft <- 1500


#Typecurve name
#CurveName <- paste(TC.Name, ' -- Spacing', TC.Spacing, ' -- Lbs/Ft', TC.Lbs.Ft)
CurveName <- paste(TC.Name)

prod_tbl <- production %>%
  



##make data types the same
prod_tbl$c.Months <- as.numeric(prod_tbl$c.Months)
TypeCurve$Time <- as.numeric(TypeCurve$Time)


##copy typecurve to a new datatable called "TCGroups"
TCGroups <- TypeCurve %>%
  mutate(Name = CurveName) %>%
  select(Time, Name, Gas.mcf, Oil.bbl, 
         cumOil.mbo, cumGas.mmcf)


##copy 12 month cum for every well in the typecurve and name it CurveName
CUMprod <- prod_tbl %>%
  filter(c.Months == 12 ) %>% 
  mutate(Name = CurveName, 
         Time = as.numeric(c.Months)) %>%
  select(Name,
         Time, 
         CUM12MOOil = c.CumLIQUID.MBO.NORM, 
         CUM12MOGas = c.CumGAS.MMCF.NORM)


##copy the 12 month cum from the typecurve and name it CUMTC
CUMtc <- TypeCurve %>%
  filter(Time == 12) %>%
  mutate(Name = paste("CUMTC", CurveName),
         Time = as.numeric(Time)) %>%
  select(Time, Name, CUM12MOOil = cumOil.mbo, CUM12MOGas = cumGas.mmcf)

##Combine CUMprod and CUMtc into 1 table called "TC.Cums"
TC.Cums <- bind_rows(CUMprod, CUMtc)

##create a table for typecurve documenting purposes...create a record
TCWellList <- wellheader %>%
  mutate(Name = CurveName,
         Norm.Lat.Length = NormalizedLateralLength,
         Lbs.Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl.Ft = (FluidAmountTotal/42) / PerfIntervalGross,
         TC.qi = qi, TC.b = b, TC.Di = Di, TC.Dmin = Dmin, TC.Years = Years) %>%
  select(Name, Entity, API, PerfIntervalGross, ProppantAmountTotal, FluidAmountTotal,
         Lbs.Ft, Bbl.Ft, Norm.Lat.Length, Spacing.Avg, Max.Infill.Time,
         TC.qi, TC.b, TC.Di, TC.Dmin, TC.Years)

dummytable <- c("Blank")


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/tcjoin.RData',sep=''), RFormat=T )))

