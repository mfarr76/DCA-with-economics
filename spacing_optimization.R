
rm(list = ls())
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcjoin.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/econtbl.RData")

library(dplyr)
library(purrr)


##user input
spacing.unit <- 640

spac_tbl <- TCWellList %>%
  mutate(WellSpacingUnit = (Norm.Lat.Length * Spacing.Avg) / 43560,
         WellsPerUnit = spacing.unit / WellSpacingUnit) %>%
  group_by(Name) %>%
  summarise(Lat.Length.avg = mean(PerfIntervalGross),
            Norm.Lat.Length = mean(Norm.Lat.Length),
            Proppant = mean(ProppantAmountTotal) / 42, 
            Fluid = mean(FluidAmountTotal), 
            Lbs.Ft = mean(Lbs.Ft),
            Bbl.Ft = mean(Bbl.Ft), 
            Spacing.avg.ft = mean(Spacing.Avg),
            WellSpacingUnit.ac = mean(WellSpacingUnit),
            WellsPerUnit = mean(WellsPerUnit)) %>%
  select(Name, Lat.Length.avg, Norm.Lat.Length, Proppant, Fluid, Lbs.Ft, Bbl.Ft, Spacing.avg.ft, 
         WellSpacingUnit.ac, WellsPerUnit)














TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/spacing_opt.RData',sep=''), RFormat=T )))
