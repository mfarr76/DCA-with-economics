
rm(list = ls())
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cashflow.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tbl4r.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/spacing_opt.RData")


##calculates spacing units per well and then wells per section


##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)



##user input
spacing_unit
##table
Econ.Metrics

spac_tbl <- TCWellList %>%
  mutate(tcName = as.factor(TC_Name),
         Single_Well_Unit = (Norm_Lat_Length * Spacing_Avg) / 43560,
         Wells_Per_DSU = spacing_unit / Single_Well_Unit) %>%
  group_by(TC_Name) %>%
  summarise(Lat_Length_avg = mean(PerfIntervalGross, is.na = TRUE),
            Norm_Lat_Length = mean(Norm_Lat_Length, is.na = TRUE),
            Proppant = mean(ProppantAmountTotal, is.na = TRUE) / 42, 
            Fluid = mean(FluidAmountTotal, is.na = TRUE), 
            Lbs_Ft = mean(Lbs_Ft, is.na = TRUE),
            Bbl_Ft = mean(Bbl_Ft, is.na = TRUE), 
            Spacing_Avg_FT = mean(Spacing_Avg, is.na = TRUE),
            Single_Well_Unit_ac = mean(Single_Well_Unit, is.na = TRUE),
            Wells_Per_DSU = mean(Wells_Per_DSU, is.na = TRUE)) %>%
  select(TC_Name, Lat_Length_avg, Norm_Lat_Length, Proppant, Fluid, Lbs_Ft, Bbl_Ft, Spacing_avg_ft, 
         Single_Well_Unit_ac, Wells_Per_DSU)

Econ.Metrics$TC_Name <- as.character(Econ.Metrics$TC_Name)
spac_tbl$TC_Name <- as.character(spac_tbl$TC_Name)


DSU <- left_join(spac_tbl, Econ.Metrics, by = "TC_Name") %>%
  mutate(EUR_MBOE_DSU = Wells_Per_DSU * NetEUR_MBOE, 
         NPV15_DSU = NPV15_M * Wells_Per_DSU, 
         Capex_DSU = Capex_M * Wells_Per_DSU) %>%
  select(TC_Name, Lat_Length_avg, Norm_Lat_Length, Proppant, Fluid, Lbs_Ft, Bbl_Ft, Spacing_Avg_FT, 
         Single_Well_Unit_ac, Wells_Per_DSU, NetEUR_MBOE, Capex_M, NPV15_M, IRR, DPI, NPV15_DSU, Capex_DSU, 
         EUR_MBOE_DSU)


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/spacing_opt.RData',sep=''), RFormat=T )))
