
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/econtbl.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/spacing_opt.RData")

library(dplyr)
library(purrr)


##user input
spacing.unit <- 640

spac_tbl <- TCWellList %>%
  mutate(tcName = as.factor(tcName),
         WellSpacingUnit = (Norm.Lat.Length * Spacing.Avg) / 43560,
         WellsPerUnit = spacing.unit / WellSpacingUnit) %>%
  group_by(tcName) %>%
  summarise(Lat.Length.avg = mean(PerfIntervalGross),
            Norm.Lat.Length = mean(Norm.Lat.Length),
            Proppant = mean(ProppantAmountTotal) / 42, 
            Fluid = mean(FluidAmountTotal), 
            Lbs.Ft = mean(Lbs.Ft),
            Bbl.Ft = mean(Bbl.Ft), 
            Spacing.avg.ft = mean(Spacing.Avg),
            WellSpacingUnit.ac = mean(WellSpacingUnit),
            WellsPerUnit = mean(WellsPerUnit)) %>%
  select(tcName, Lat.Length.avg, Norm.Lat.Length, Proppant, Fluid, Lbs.Ft, Bbl.Ft, Spacing.avg.ft, 
         WellSpacingUnit.ac, WellsPerUnit)

EconMetrics$tcName <- as.character(EconMetrics$tcName)
spac_tbl$tcName <- as.character(spac_tbl$tcName)


DSU <- left_join(spac_tbl, EconMetrics, by = "tcName") %>%
  mutate(EUR.SpacingUnit.MBOE = WellsPerUnit * EUR.MBOE, 
         PV15.SpacingUnit = NPV15 * WellsPerUnit, 
         Capex.SpacingUnit = Capex * WellPerUnit)


clrTbl <- 1
clrEcon <- if(clrTbl == 1){rm(EconTable)}



TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/spacing_opt.RData',sep=''), RFormat=T )))
