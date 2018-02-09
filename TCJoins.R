
rm(list = ls())
library(dplyr, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)

##load data files================================================================
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/Yield.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tbl4r.RData")

##choose which data you want to load....need average data
#prod <- load("C:/Users/MFARR/Google Drive/r scripts/Spotfire/average.in.RData")
prod_tbl <- read.csv("C:/Users/MFARR/Google Drive/r scripts/Production/IHS.PROD.csv")

##do not load into spotfire=========================================================
qi <- 5000
b <- 1.2
Di <- 70
Dmin <- 8
Years <- 10
user_TCname <- c("TEST")
TC_Spacing <- 300
TC_Lbs_Ft <- 1500
prod_tbl 
DCA.Forecast <- DCA
NormalizedLateralLength <- 5000


##install packageif it is not already installed====================================

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

##user inputs
user_TCname #name of TC group - input box on DCA tab

##TcCums table==================================================================
##copy 12 month cum for every well in the typecurve and name it user_TCname
##then copy the 12 month cum for tc and name it CUMTC_user_TCname

TcCums <- prod_tbl %>%
  filter(c.Months == 12 ) %>% 
  mutate(TC_Name = user_TCname,
         TC_Group = user_TCname,
         Time = as.numeric(c.Months)) %>%
  select(TC_Name,
         TC_Group,
         Time, 
         CUM12MOOil_Norm = c.Cum.Liquid.Mbo.Norm, 
         CUM12MOGas_Norm = c.Cum.Gas.MMcf.Norm) %>%
  bind_rows(., DCA.Forecast %>%
  filter(Time == 12) %>%
  mutate(TC_Name = paste("CUMTC", user_TCname),
         TC_Group = user_TCname,
         Time = as.numeric(Time)) %>%
  select(Time, TC_Name, TC_Group, CUM12MOOil_Norm = cumOil_mbo, CUM12MOGas_Norm = cumGas_mmcf)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_if(is.character, as.factor)

##TcForecast====================================================================
##create a table with every well in TC (for documentation) and the average curve of all the wells
##copy over the TC generated from the DCA tab
##Gas_mcf_Norm = rate time of every producing well
##cumGas_mmcf_Norm = cum time of every producing well
##Gas_avg = average rate per TC group
##WellCount = well count over time
##Gas_mcf_TC = TC of gas phase in mcf
##cumGas_mmcf = cum TC of has phase in mmcf

TcForecast <- prod_tbl %>%
  filter(!is.na(c.Months)) %>%
  mutate( TC_Group = user_TCname,
          Well_Type = paste("PDP"), 
          c.Months = as.numeric(c.Months)) %>%
  select(WellID = PrimaryWellAPI,
         TC_Group,
         Well_Type, 
         Time = c.Months,
         Gas_mcf_Norm = c.Gas.Normalized, 
         Oil_bbl_Norm = c.Liquid.Normalized, 
         cumOil_mbo_Norm = c.Cum.Liquid.Mbo.Norm, 
         cumGas_mmcf_Norm = c.Cum.Gas.MMcf.Norm) %>%
  bind_rows(., DCA.Forecast %>%
              mutate(WellID = user_TCname,
                     TC_Group = user_TCname,
                     Well_Type = "TypeCurve") %>%
              select(WellID, TC_Group, Well_Type, Time,
                     Gas_avg, Oil_avg, WellCount,
                     Gas_mcf_TC = Gas_mcf, Oil_bbl_TC = Oil_bbl, 
                     cumOil_mbo_TC = cumOil_mbo, 
                     cumGas_mmcf_TC = cumGas_mmcf)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_if(is.character, as.factor)

##user inputs for TcWellList=======================================================
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


##TcWellList=======================================================================
##create a table for typecurve documenting purposes...create a record
TcWellList <- wellheader %>%
  mutate(TC_Group = user_TCname,
         API = as.character(API),
         Entity = as.character(Entity),
         Norm_Lat_Length = as.numeric(NormalizedLateralLength),
         Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal/42) / PerfIntervalGross, 
         CUM12MOGas_MMcf_Norm = (First12MonthGas / 1000) / PerfIntervalGross * Norm_Lat_Length, 
         CUM12MOOil_Mbo_Norm = (First12MonthLiquid / 1000) / PerfIntervalGross * Norm_Lat_Length) %>%
  select(TC_Group, Entity, API, PerfIntervalGross, TotalDepthTVD, ProppantAmountTotal, FluidAmountTotal,
         Lbs_Ft, Bbl_Ft, Norm_Lat_Length, Spacing_Avg, Max_Infill_Time, Ratio, 
         CUM12MOGas_MMcf_Norm, CUM12MOOil_Mbo_Norm) %>%
  left_join(., welltest %>%
              mutate(WGR_Bbl_MMcf_WellTest = FlowWater / (FlowGas / 1000), 
                     WOR_Bbl_Bbl_WellTest = FlowWater / FlowOil, 
                     API = as.character(API), 
                     Rsi_Scf_Bbl = (FlowGas * 1000) / FlowOil) %>%
              select(API, FlowOil_WellTest = FlowOil,
                     FlowGas_WellTest = FlowGas, 
                     FlowWater_WellTest = FlowWater,
                     Rsi_Scf_Bbl,
                     WGR_Bbl_MMcf_WellTest, 
                     WOR_Bbl_Bbl_WellTest, 
                     ChokeTopDescription_WellTest = ChokeTopDescription, 
                     GravityOil_WellTest = GravityOil, 
                     GravityCondensate_WellTest = GravityCondensate), by = "API") %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_if(is.character, as.factor)


##create tbl with forecast parameters=====================================================

TcParameters <- data.frame(TC_Group = user_TCname, Primary_phase = ifelse(og_select == 2, "Gas", "Oil"), 
                            Curve_description = ifelse(curveSelect == 0, "Average", ifelse(curveSelect == 1, "P10", "P90")),
                            Min_lat_length = minLat, Norm_lat_length = EffLat,Min_well_count = MinWellCount, 
                            MS_On_Off = ifelse(ms == 1, "ON", "OFF"), MS_Time = time_ms,
                            MS_Di = di_ms, qi, Di = di, b, Dmin, Ratio_segment = time_segment,
                            Initial_ratio = ratio_1_user, Second_ratio = ratio_2_user, Final_ratio = ratio_3_user,
                            Ab_rate = abRate, Forecast_years = forecast_years, Gas_EUR_mmcf = max(DCA.Forecast$cumGas_mmcf, na.rm = TRUE), 
                            Oil_EUR_mbo = max(DCA.Forecast$cumOil_mbo, na.rm = TRUE))

##establish the primary phase
P_phase <- ifelse(TcParameters$Primary_phase == "Gas", 2, 3)

##create first prod mnth and qi month (time to peak mnth)
TcParameters <- TcParameters %>%
  mutate(First_prod_month = DCA.Forecast[1,P_phase],
         qi_month = DCA.Forecast$Time[which.max(DCA.Forecast[,P_phase])]) %>%
  select(TC_Group, Primary_phase, Curve_description, Min_lat_length, Norm_lat_length, 
         Min_well_count, First_prod_month, qi_month, qi, MS_On_Off, MS_Time, MS_Di, Di, b, Dmin, Ratio_segment, Initial_ratio, Second_ratio, 
         Final_ratio, Ab_rate, Forecast_years, Gas_EUR_mmcf, Oil_EUR_mbo) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_if(is.character, as.factor)

##write to access file============================================================
##user must have a odbc data source "Microsoft Access Driver (*.mdb, *.accdb)
##installed on their machine to communicate with Access

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}" ##load the odbc driver
#dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dLocation <- AccessFilePath ##file path from input table

##channel created for connection
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

##save function to save table created to access
sqlSave(ch, TcForecast, tablename = "TcForecast", rownames = FALSE, append = TRUE)
sqlSave(ch, TcCums, tablename = "TcCums", rownames = FALSE, append = TRUE)
sqlSave(ch, TcWellList, tablename = "TcWellList", rownames = FALSE, append = TRUE)
sqlSave(ch, TcParameters, tablename = "TcParameters", rownames = FALSE, append = TRUE)

close(ch)

##save script to file============================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/tcgroup.RData',sep=''), RFormat=T )))


##end of script==================================================================





