##script loads TcForecast, TcCums, TcWellList table from access

library(RODBC, warn.conflicts = FALSE)

##open connect to access==========================================

##doc property
AccessFilePath

driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
dLocation <- AccessFilePath
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))


##check if table already exist====================================
check_tbl <- c("TcCum", "TcForecast", "TcParameters", "TcWellList")
access_tbl <- sqlTables(ch)[3]


##load table from access==========================================
if(any(sapply(access_tbl, function(x) check_tbl %in% x))){

TcForecast <- sqlQuery(ch, paste("select * from TcForecast"))
TcCum <- sqlQuery(ch, paste("select * from TcCum"))
TcWellList <- sqlQuery(ch, paste("select * from TcWellList"))

}else{
  TcCum <- data.frame(TC_Name = "None", TC_Group = "None", Time = 0, 
                      CUM12MOOil_Norm = 0, CUM12MOGas_Norm = 0)
  TcForecast <- data.frame(WellID = "None", TC_Group = "None", Well_Type = "None", 
                           Time = 0, Gas_mcf_d_Norm_pdp = 0, Oil_bbl_d_Norm_pdp = 0, 
                           cumOil_mbo_Norm_pdp = 0, cumGas_mmcf_Norm_pdp = 0, 
                           Gas_avg_mcf_d = 0, Oil_avg_bbl_d = 0, WellCount = 0, 
                           Gas_mcf_d_TC = 0, Oil_bbl_d_TC = 0, cumOil_mbo_TC = 0, 
                           cumGas_mmcf_TC = 0)
  TcWellList <- data.frame(TC_Group = "None", Entity = "None", API = 0, PerfIntervalGross = 0, 
                           TotalDepthTVD = 0, ProppantAmountTotal = 0, FluidAmountTotal = 0, 
                           Lbs_Ft = 0, Bbl_Ft = 0, Norm_Lat_Length = 0, cSpacing_avg = 0, 
                           cMax_Infill_Time = 0, Ratio = 0, FlowOil_WellTest = 0, FlowGas_WellTest = 0, 
                           WGR_Bbl_MMcf_WellTest = 0, WOR_Bbl_Bbl_WellTest = 0, ChokeTopDescription_WellTest = 0, 
                           GravityOil_WellTest = 0, GravityCondensate_WellTest = 0)
}

close(ch)


