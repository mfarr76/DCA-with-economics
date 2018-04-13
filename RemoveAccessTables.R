rm(list = ls())

AccessFilePath <- "C:/Users/mfarr/Documents/Spotfire.accdb"

##data function will remove the following tables in access
##TcCum, TcForest, TcParameters, TcWellList

library(RODBC)

##open connect to access==========================================

##doc property
AccessFilePath

driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
dLocation <- AccessFilePath
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))


##check if table already exist====================================
check_tbl <- c("TcCum", "TcForecast", "TcParameters", "TcWellList")
access_tbl <- sqlTables(ch)[3]


if(any(sapply(access_tbl, function(x) check_tbl %in% x))){
  ##remove the table in access
  sqlDrop(ch, "TcCum")
  sqlDrop(ch, "TcForecast")
  sqlDrop(ch, "TcParameters")
  sqlDrop(ch, "TcWellList")
  
  msg <- "Tables Removed"

}else{
  msg <- "No Tables to remove - Proceed with workflow"
}
  
close(ch)


