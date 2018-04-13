
rm(list = ls())
library(dplyr, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)

load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin_at.RData")


##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)

##tables needed
wellheader #wellheader table 
prod_tbl #production table
DCA.Forecast #typecurve table
forecast.table

##user inputs
user_TCname #name of TC group - input box on DCA tab
og_select
curveSelect
EffLat
minWellcount
msOnOff
msTime
msDi
qi
di
b
dmin
timeRatio
ratio1
ratio2
ratio3
abRate
forecast_years
autoPick
ipMonth


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
  filter(!is.na(TIME.MONTHS)) %>%
  mutate( TC_Group = user_TCname,
          Well_Type = paste("PDP"), 
          Time = TIME.MONTHS) %>%
  select(PROPNUM,
         LEASE, 
         TC_Group,
         Well_Type, 
         Time,
         Gas_mcf_Norm = GAS.NORMALIZED, 
         Oil_bbl_Norm = OIL.NORMALIZED, 
         cumOil_mbo_Norm = CUM.OIL.MBO.NORMALIZED, 
         cumGas_mmcf_Norm = CUM.GAS.MMCF.NORMALIZED) %>%
  bind_rows(., DCA.Forecast %>%
              mutate(PROPNUM = user_TCname,
                     LEASE = user_TCname,
                     TC_Group = user_TCname,
                     Well_Type = "TypeCurve") %>%
              select(PROPNUM,
                     LEASE,
                     TC_Group, 
                     Well_Type, 
                     Time,
                     Gas_avg, Oil_avg, WellCount,
                     Gas_mcf_TC = Gas_mcf_d, 
                     Oil_bbl_TC = Oil_bbl_d, 
                     cumOil_mbo_TC = cumOil_mbo, 
                     cumGas_mmcf_TC = cumGas_mmcf)) 



##TcCums table==================================================================
##copy 12 month cum for every well in the typecurve and name it user_TCname
##then copy the 12 month cum for tc and name it CUMTC_user_TCname
TcCum <- prod_tbl %>%
  filter(TIME.MONTHS == 1) %>%
  mutate(Name = user_TCname) %>%
  select(PROPNUM, LEASE, Name) %>%
  left_join(., prod_tbl %>%
              filter(TIME.MONTHS == 6) %>%         
              mutate(Time = TIME.MONTHS, 
                     Name = user_TCname) %>%
              select(PROPNUM, Time, 
                     CUM6MOOil = CUM.OIL.MBO.NORMALIZED, 
                     CUM6MOGas = CUM.GAS.MMCF.NORMALIZED), by = "PROPNUM") %>%
  bind_rows(., DCA.Forecast %>%
              filter(Time == 6) %>%
              mutate(Name = paste("CUMTC", user_TCname), 
                     PROPNUM = "0", 
                     LEASE = paste("CUMTC", user_TCname)) %>%
  select(PROPNUM, LEASE, Time, Name, CUM6MOOil = cumOil_mbo, CUM6MOGas = cumGas_mmcf)) %>%
  


##create tbl with forecast parameters=====================================================
TcParameters <- data.frame(TC_Group = user_TCname, 
                           Primary_phase = ifelse(og_select == 2, "Gas", "Oil"), 
                           Curve_description = ifelse(curveSelect == 0, "Average", 
                                                      ifelse(curveSelect == 1, "P10", 
                                                             ifelse(curveSelect == 2, "P50", "P90"))),
                           Norm_lat_length = EffLat,
                           Min_well_count = minWellcount, 
                           autoPick = ifelse(autoPick == 1, "ON", "OFF"),
                           autoPick_ipMonth = ipMonth,
                           Month1_volume = ifelse(og_select == 2, DCA.Forecast$Gas_mcf[1], DCA.Forecast$Oil_bbl[1]),
                           time_to_peak = Forecast.Table$time_to_peak, 
                           qiAvg_rate = Forecast.Table$qi_back_calc,
                           MS_On_Off = ifelse(msOnOff == 1, "ON", "OFF"), 
                           MS_Time = msTime, 
                           MS_Di = msDi,
                           Di = di, b, dmin, Ratio_segment = timeRatio, Initial_ratio = ratio1, Second_ratio = ratio2, 
                           Final_ratio = ratio3, Ab_rate = abRate, Forecast_years = forecast_years, 
                           Gas_EUR_mmcf = Forecast.Table$GasEUR_mmcf, 
                           Oil_EUR_mbo = Forecast.Table$OilEUR_mbo)


##write to access file============================================================
##user must have a odbc data source "Microsoft Access Driver (*.mdb, *.accdb)
##installed on their machine to communicate with Access

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"  ##load the odbc driver
#dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dLocation <- AccessFilePath                                    ##file path from input table

##channel created for connection
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

##save function to save table created to access
sqlSave(ch, TcForecast, tablename = "TcForecast", rownames = FALSE, append = TRUE)
sqlSave(ch, TcCum, tablename = "TcCum", rownames = FALSE, append = TRUE)
sqlSave(ch, TcParameters, tablename = "TcParameters", rownames = FALSE, append = TRUE)
#sqlSave(ch, TcWellList, tablename = "TcWellList", rownames = FALSE, append = TRUE)

close(ch)




TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/tcjoin_at.RData',sep=''), RFormat=T )))
