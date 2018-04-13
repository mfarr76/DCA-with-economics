rm(list = ls())

#install.packages("RODBC")
install.packages(c("dplyr"))
library(RODBC)
library(dplyr)
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/DCAwBU.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")

##purpose is to write/read table from access============================================
##user must have a "Microsoft Access Driver (*.mdb, *accdb) installed on their machine 
##there are two different ways to communicate with access
##1 create an odbc connection to the access file "odbcConnect"
##2 create a driver and navigate to the access file location and then communicate
##======================================================================================

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}" #driver option
dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb" #file location
dTable <- "TcParameters" #table to save

ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation)) #open a channel
TC_Parameters$TC_Name <- as.character(TC_Parameters$TC_Name)

##load table from access
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))
tableOut <- sqlQuery( ch , paste ("select * from ",dTable))
output <- tableOut

##write table to access
driver="Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
ch<- odbcDriverConnect(paste(driver,';DBQ=',dLocation))
sqlSave(ch, TC_Parameters, tablename = dTable,  append = TRUE, verbose = TRUE)

TC_Parameters$qi_month <- 10
TC_Parameters$TC_Name <- as.character(TC_Parameters$TC_Name)
sqlUpdate(ch, TC_Parameters, tablename = "TC_Parameters")
sqlDrop(ch, "TC_Parameters")
######-----------------------
ch <- odbcConnect("Spotfire_Test")
close(ch)

sqlTables(ch)
sqlFetch(ch, "TC_Groups")
sqlQuery(ch, paste("select * from TCGroups"))

sqlSave(ch, DCA.Forecast, tablename = "DCA2", append = FALSE)


sqlSave(ch, DCA.Forecast, tablename = tablenames, append = FALSE)
sqlDrop(ch, "TC_Parameters")
sqlDrop(ch, "TCCums")
#sqlUpdate(ch, dcaTbl, tablename = "DCA2")
sqlSave(ch, dcaTbl, tablename = "DCA_Table", append = FALSE)
close(ch)



driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dummyTable <- "BLANK"

##load table from access
channel <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))
TCGroups <- sqlQuery(ch, paste("select * from TcForecast"))
#TCCums <- sqlQuery(channel, paste("select * from TCCums"))

sqlUpdate(ch, TCGroups, tablename = "TcForecast")

TCGroups <- filter(TCGroups, Time > 120)





