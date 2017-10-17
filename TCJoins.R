
rm(list = ls())
library(dplyr, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)

#########################LOAD - TESTING ONLY#################################
##

load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin_Monthly_AT.RData")

#-------------------------------------------------------------------------------
##choose which data you want to load....need average data
#prod <- load("C:/Users/MFARR/Google Drive/r scripts/Spotfire/average.in.RData")
prod_tbl <- read.csv("C:/Users/MFARR/Google Drive/r scripts/Production/IHS.PROD.csv")
#-------------------------------------------------------------------------------

qi <- 5000
b <- 1.2
Di <- 70
Dmin <- 8
Years <- 10
user_TCname <- c("TEST")
TC_Spacing <- 300
TC_Lbs_Ft <- 1500
##############################################################################



##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)


##tables needed
wellheader #wellheader table to get spacing
prod_tbl #production table
DCA.Forecast #typecurve table
wellheader #wellheader table to get spacing




#Typecurve name
#CurveName <- paste(TC.Name, ' -- Spacing', TC.Spacing, ' -- Lbs/Ft', TC.Lbs.Ft)
CurveName <- paste(user_TCname)#name comes from user input

##make data types the same
#prod_tbl$c.Months <- as.numeric(prod_tbl$c.Months)
#TypeCurve$Time <- as.numeric(TypeCurve$Time)


##copy typecurve to a new datatable called "TCGroups"
TC_Groups <- DCA.Forecast %>%
  mutate(TC_Name = CurveName) %>%
  select(TC_Name, Time, Gas_mcf, Oil_bbl, 
         cumOil_mbo, cumGas_mmcf)





#############################################################################
##copy 12 month cum for every well in the typecurve and name it CurveName
CUM_prod <- prod_tbl %>%
  filter(c.Months == 12 ) %>% 
  mutate(TC_Name = CurveName, 
         Time = as.numeric(c.Months)) %>%
  select(TC_Name,
         Time, 
         CUM12MOOil = c.Cum.Liquid.Mbo.Norm, 
         CUM12MOGas = c.Cum.Gas.MMcf.Norm)

##copy the 12 month cum from the typecurve and name it CUMTC
CUM_tc <- DCA.Forecast %>%
  filter(Time == 12) %>%
  mutate(TC_Name = paste("CUMTC", CurveName),
         Time = as.numeric(Time)) %>%
  select(Time, TC_Name, CUM12MOOil = cumOil_mbo, CUM12MOGas = cumGas_mmcf)

##Combine CUMprod and CUMtc into 1 table called "TC.Cums"
TC_Cums <- bind_rows(CUM_prod, CUM_tc)
##############################################################################

##----------------------------------------------------------------------------
#input parameters for tcwelllist
#TC_qi, TC_b, TC_Di, TC_Dmin, TC_Years
og_select
curveSelect <- 0
minLat
EffLat
MinWellCount
ms
time_ms
di_ms
abRate
time_segment
ratio_1_user
ratio_2_user
ratio_3_user
forecast_years
##----------------------------------------------------------------------------

##create a table for typecurve documenting purposes...create a record
TC_WellList <- wellheader %>%
  mutate(TC_Name = CurveName,
         Norm_Lat_Length = NormalizedLateralLength,
         Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal/42) / PerfIntervalGross) %>%
  select(TC_Name, Entity, API, PerfIntervalGross, ProppantAmountTotal, FluidAmountTotal,
         Lbs_Ft, Bbl_Ft, Norm_Lat_Length, Spacing_Avg, Max_Infill_Time)

##create tbl with forecast parameters
TC_Parameters <- data.frame(TC_Name = CurveName, Primary_phase = ifelse(og_select == 2, "Gas", "Oil"), 
                            Curve_description = ifelse(curveSelect == 0, "Average", ifelse(curveSelect == 1, "P10", "P90")),
                            Min_lat_length = minLat, Norm_lat_length = EffLat,Min_well_count = MinWellCount, 
                            MS_On_Off = ifelse(ms == 1, "ON", "OFF"), MS_Time = time_ms,
                            MS_Di = di_ms, qi, Di = di, b, Dmin, Ratio_segment = time_segment,
                            Initial_ratio = ratio_1_user, Second_ratio = ratio_2_user, Final_ratio = ratio_3_user,
                            Ab_rate = abRate, Forecast_years = forecast_years, Gas_EUR_mmcf = max(DCA.Forecast$cumGas_mmcf, na.rm = TRUE), 
                            Oil_EUR_mbo = max(DCA.Forecast$cumOil_mbo, na.rm = TRUE))

##establish the primary phase
P_phase <- ifelse(TC_Parameters$Primary_phase == "Gas", 2, 3)

##create first prod mnth and qi month (time to peak mnth)
TC_Parameters <- TC_Parameters %>%
  mutate(First_prod_month = DCA.Forecast[1,P_phase],
         qi_month = DCA.Forecast$Time[which.max(DCA.Forecast[,P_phase])]) %>%
  select(TC_Name, Primary_phase, Curve_description, Min_lat_length, Norm_lat_length, 
         Min_well_count, First_prod_month, qi_month, qi, MS_On_Off, MS_Time, MS_Di, Di, b, Dmin, Ratio_segment, Initial_ratio, Second_ratio, 
         Final_ratio, Ab_rate, Forecast_years, Gas_EUR_mmcf, Oil_EUR_mbo)



##use a blank table to prevent the data function from being removed due to IP script
#dummytable <- c("Blank")


##write to access file

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
#dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dLocation <- "N:/Dept/Prod/Aries/Db/Houston/2017 Projects/A&D/PRESIDIO/ValVerdeTypeCurves.accdb"

##channel created for connection
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

##save function to save table created to access
sqlSave(ch, TC_Groups, tablename = "TC_Groups", rownames = FALSE, append = TRUE)
sqlSave(ch, TC_Cums, tablename = "TC_Cums", rownames = FALSE, append = TRUE)
sqlSave(ch, TC_WellList, tablename = "TC_WellList", rownames = FALSE, append = TRUE)
sqlSave(ch, TC_Parameters, tablename = "TC_Parameters", rownames = FALSE, append = TRUE)


close(ch)

TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/tcgroup.RData',sep=''), RFormat=T )))
