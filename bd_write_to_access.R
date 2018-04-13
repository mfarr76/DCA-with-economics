rm(list = ls())

load("C:/Users/MFARR/Documents/R_files/Spotfire.data/writetoaccess.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tbl4r.RData")

library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(RODBC, warn.conflicts = FALSE)


##tables needed
wellheader #wellheader table to get spacing
prod_tbl #production table
DCAForecast #typecurve table
welltest #welltest table

##user inputs==================================================================================
user_TCname #name of TC group - input box on DCA tab
og_select #oil or gas select
curveSelect #average, p10, p50, p90 curve select
minLat #min lateral length
EffLat #normalized lat length
minWellcount #min well count for dca
msOnOff #multi segment on/off
msTime #first segment time
msDi #multi segment decline
qi #initial rate
Di #initial decline
b #b factor
Dmin #terminal decline
rtime #1st ratio decline segment
ratio1 #initial ratio
ratio2 #2nd ratio
ratio3 #final ratio
abRate #abondenment rate
years #forecast time
autoPick #turn ip autopick on/off
ipMonth #if auto is turned off, select month to ip the tc
AccessFilePath #file path to access file to save tables 

##tc cums========================================================================================
##copy 12 month cum for every well in the typecurve and name it from the user input 
##box on DCA tab = user_TCname 
##then copy the 12 month cum for tc and name it CUMTC_user_TCname and then join the rows

TcCums <- prod_tbl %>%
  filter(c.Months == 12 ) %>% 
  mutate(TC_Name = user_TCname, 
         TC_Group = user_TCname,
         Time = as.numeric(c.Months)) %>%
  select(TC_Name,
         TC_Group,
         Time, 
         CUM12MOOil_Norm = c.Cum_Liquid_Mbo_Norm, 
         CUM12MOGas_Norm = c.Cum_Gas_MMcf_Norm) %>%
  bind_rows(., DCA.Forecast %>%
              filter(Time == 12) %>%
              mutate(TC_Name = paste("CUMTC", user_TCname),
                     TC_Group = user_TCname,
                     Time = as.numeric(Time)) %>%
              select(Time, TC_Name, TC_Group, CUM12MOOil_Norm = cumOil_mbo, CUM12MOGas_Norm = cumGas_mmcf))

##tc forecast====================================================================================
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
         Gas_mcf_d_Norm_pdp = c.Gas_Norm_Days, #well gas rate from prod tbl
         Oil_bbl_d_Norm_pdp = c.Liquid_Norm_Days, #well oil rate from prod tbl
         cumOil_mbo_Norm_pdp = c.Cum_Liquid_Mbo_Norm, #well cum oil from prod tbl
         cumGas_mmcf_Norm_pdp = c.Cum_Gas_MMcf_Norm) %>% #well cum gas rate from prod tbl
  bind_rows(., DCA.Forecast %>%
              mutate(WellID = user_TCname,
                     TC_Group = user_TCname,
                     Well_Type = "TypeCurve") %>%
              select(WellID, TC_Group, Well_Type, Time,
                     Gas_avg_mcf_d, #from average table
                     Oil_avg_bbl_d, #from average table
                     WellCount, #from average table
                     Gas_mcf_d_TC = Gas_mcf_d, #dca generated tc
                     Oil_bbl_d_TC = Oil_bbl_d, #dca generated tc
                     cumOil_mbo_TC = cumOil_mbo, #dca generated tc
                     cumGas_mmcf_TC = cumGas_mmcf)) #dca generated tc


##tc wellist======================================================================================

##create a table for typecurve documenting purposes...create a record
TcWellList <- wellheader %>%
  mutate(TC_Group = user_TCname,
         API = API,
         Entity = Entity,
         Norm_Lat_Length = EffLat,
         Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal/42) / PerfIntervalGross, 
         CUM12MOGas_MMcf_Norm = (First12MonthGas / 1000) / PerfIntervalGross * Norm_Lat_Length, 
         CUM12MOOil_Mbo_Norm = (First12MonthLiquid / 1000) / PerfIntervalGross * Norm_Lat_Length) %>%
  select(TC_Group, Entity, API, PerfIntervalGross, TotalDepthTVD, ProppantAmountTotal, FluidAmountTotal,
         Lbs_Ft, Bbl_Ft, Norm_Lat_Length, c.Spacing_Avg, c.Max_Infill_Time, Ratio) %>%
  left_join(., welltest %>%
              mutate(WGR_Bbl_MMcf_WellTest = ifelse(FlowWater == 0 | FlowGas == 0, "", FlowWater / (FlowGas / 1000)), 
                     WOR_Bbl_Bbl_WellTest = ifelse(FlowWater == 0 | FlowOil == 0, "", FlowWater / FlowOil), 
                     API = API, 
                     Rsi_Scf_Bbl = ifelse(FlowGas == 0 | FlowOil == 0, "", (FlowGas * 1000) / FlowOil)) %>%
              select(API, FlowOil_WellTest = FlowOil,
                     FlowGas_WellTest = FlowGas, 
                     FlowWater_WellTest = FlowWater,
                     Rsi_Scf_Bbl,
                     WGR_Bbl_MMcf_WellTest, 
                     WOR_Bbl_Bbl_WellTest, 
                     ChokeTopDescription_WellTest = ChokeTopDescription, 
                     GravityOil_WellTest = GravityOil, 
                     GravityCondensate_WellTest = GravityCondensate), by = "API")



##tc parameters====================================================================================
TcParameters <- data.frame(TC_Group = user_TCname, Primary_phase = ifelse(og_select == 2, "Gas", "Oil"), 
                           Curve_description = ifelse(curveSelect == 0, "Average", ifelse(curveSelect == 1, "P10", "P90")),
                           Min_lat_length = minLat, Norm_lat_length = EffLat,Min_well_count = minWellcount, 
                           MS_On_Off = ifelse(msOnOff == 1, "ON", "OFF"), msTime,
                           msDi, qi, Di, b, Dmin, Ratio_Time = rtime,
                           Initial_ratio = ratio1, Second_ratio = ratio2, Final_ratio = ratio3,
                           Ab_rate = abRate, Forecast_years = years, Gas_EUR_mmcf = max(DCA.Forecast$cumGas_mmcf, na.rm = TRUE), 
                           Oil_EUR_mbo = max(DCA.Forecast$cumOil_mbo, na.rm = TRUE)) %>%
  mutate_(First_prod_volume = DCA.Forecast[1,og_select], 
          qi_month = DCA.Forecast$Time[which.max(DCA.Forecast[,og_select])]) %>%
  select(TC_Group, Primary_phase, Curve_description, Min_lat_length, Norm_lat_length, 
         Min_well_count, First_prod_volume, qi_month, qi, MS_On_Off, msTime, msDi, Di, 
         b, Dmin, Ratio_Time, Initial_ratio, Second_ratio, Final_ratio, Ab_rate, 
         Forecast_years, Gas_EUR_mmcf, Oil_EUR_mbo)


##write to access file==============================================================================

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
#dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dLocation <- AccessFilePath

##channel created for connection
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

##save function to save table created to access
sqlSave(ch, TcForecast, tablename = "TcForecast", rownames = FALSE, append = TRUE)
sqlSave(ch, TcCums, tablename = "TcCums", rownames = FALSE, append = TRUE)
sqlSave(ch, TcWellList, tablename = "TcWellList", rownames = FALSE, append = TRUE)
sqlSave(ch, TcParameters, tablename = "TcParameters", rownames = FALSE, append = TRUE)



close(ch)

TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/writetoaccess.RData',sep=''), RFormat=T )))
